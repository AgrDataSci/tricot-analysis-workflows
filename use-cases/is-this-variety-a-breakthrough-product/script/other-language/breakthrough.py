# Python reference for the use case "Is this variety a breakthrough product?"
# Decision record: decisions/breakthrough-product.md
#
# This file mirrors script/helper/functions.R and script/helper/simulate-data.R.
# It exists because the R scripts could not be executed when the use case was
# drafted. Every number in the README comes from this code. Packages used in
# place of the R ones: numpy, pandas and scipy. The Plackett-Luce fit is a
# plain maximum-likelihood fit for complete rankings of three items without
# ties. It does not handle ties, so it is not a replacement for PlackettLuce.
#
# Scientific intent
#   Decide whether a test variety gives at least a set gain (default 25%) over
#   a check, and how often it does so across farms, with uncertainty.
# Approach
#   Step 1, per trial. Log yield on variety and farm, contrasts to the check
#     with their standard errors (after Dorado-Betancourt et al. 2026).
#   Step 2, across trials. Weighted random-effects mean of the contrasts, with
#     a variety-by-trial variance.
#   Decision. Reliability (Eskridge and Mumm 1992), the probability that the
#     variety outperforms the check, here also at the gain threshold.
import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.stats import norm


# ---------------------------------------------------------------- step 1 ----
def _connected_to_check(d, check):
    """Varieties linked to the check through farms that share varieties."""
    seen, new = {check}, {check}
    by_farm = d.groupby("farm").genotype.agg(set)
    while new:
        hit = set().union(*[s for s in by_farm if s & new]) if len(by_farm) else set()
        new = hit - seen
        seen |= new
    return seen


def trial_contrasts(data, check):
    """Step 1. Per trial, contrast of each variety to the check on log yield.

    data has the columns trial, farm, genotype and yield, one row per plot.
    Zero yields cannot enter a log model. They are counted and reported, and
    they stay in direct_reliability() and harvest_failure().
    """
    rows, skipped, rss, dfres = [], [], [], []
    for trial, d in data.groupby("trial", sort=True):
        n_in = len(d)
        d = d[d["yield"] > 0]
        d = d[d.groupby("farm")["yield"].transform("size") >= 2]
        if check not in set(d.genotype):
            skipped.append((trial, "check not grown"))
            continue
        ok = _connected_to_check(d, check)
        d = d[d.genotype.isin(ok)]
        d = d[d.groupby("farm")["yield"].transform("size") >= 2]
        items = [check] + sorted(set(d.genotype) - {check})
        if len(items) < 2:
            skipped.append((trial, "no variety shares a farm network with the check"))
            continue
        y = np.log(d["yield"].values)
        X = np.column_stack([(d.genotype.values == i).astype(float) for i in items[1:]])
        farm = d.farm.values
        dm = lambda a: a - pd.DataFrame(a).groupby(farm).transform("mean").values
        Xd, yd = dm(X), dm(y.reshape(-1, 1)).ravel()
        XtX = Xd.T @ Xd
        if np.linalg.matrix_rank(XtX) < XtX.shape[0]:
            skipped.append((trial, "contrasts to the check not estimable"))
            continue
        b = np.linalg.solve(XtX, Xd.T @ yd)
        rss.append(float(((yd - Xd @ b) ** 2).sum()))
        dfres.append(len(y) - d.farm.nunique() - len(b))
        v = np.diag(np.linalg.inv(XtX))
        for i, g in enumerate(items[1:]):
            rows.append(dict(trial=trial, genotype=g, d=b[i], v=v[i],
                             n_plots=int((d.genotype == g).sum()),
                             plots_in=n_in, plots_used=len(d)))
    out = pd.DataFrame(rows)
    # one residual variance for all trials, so that a small trial, which cannot
    # estimate its own, still enters with an honest standard error
    sigma2 = sum(rss) / sum(dfres) if sum(dfres) > 0 else np.nan
    if len(out):
        out["se"] = np.sqrt(sigma2 * out.pop("v"))
    out.attrs["skipped"] = skipped
    out.attrs["sigma2"] = sigma2
    return out


# ---------------------------------------------------------------- step 2 ----
def pool_trials(contrasts):
    """Step 2. Weighted random-effects mean of the trial contrasts per variety.

    The between-trial variance (tau2) is the variety-by-trial interaction,
    estimated with the DerSimonian-Laird moment estimator. With one trial it
    cannot be estimated and is set to zero, which the output flags.
    """
    rows = []
    for g, d in contrasts.groupby("genotype", sort=True):
        w = 1 / d.se.values ** 2
        k = len(d)
        mu_f = np.sum(w * d.d.values) / w.sum()
        tau2 = 0.0
        if k > 1:
            q = np.sum(w * (d.d.values - mu_f) ** 2)
            tau2 = max(0.0, (q - (k - 1)) / (w.sum() - np.sum(w ** 2) / w.sum()))
        ws = 1 / (d.se.values ** 2 + tau2)
        rows.append(dict(genotype=g, log_gain=np.sum(ws * d.d.values) / ws.sum(),
                         se=np.sqrt(1 / ws.sum()), tau2=tau2, n_trials=k,
                         n_plots=int(d.n_plots.sum())))
    return pd.DataFrame(rows)


# -------------------------------------------------------------- decision ----
def reliability_normal(log_gain, sd_diff, threshold=0.0):
    """Eskridge and Mumm (1992), equation 2, on the log scale.

    threshold = 0 is their reliability, the probability of outperforming the
    check. threshold = 0.25 is our extension, the probability that the gain
    is at least 25% in a farm drawn from the population the trials sampled.
    """
    return norm.cdf((np.asarray(log_gain) - np.log1p(threshold)) / np.asarray(sd_diff))


def wilson(x, n, level=0.95):
    if n == 0:
        return np.nan, np.nan
    z = norm.ppf(1 - (1 - level) / 2)
    p = x / n
    c = (p + z * z / (2 * n)) / (1 + z * z / n)
    h = z * np.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n)
    return c - h, c + h


def direct_reliability(data, check, threshold=0.25):
    """Nonparametric reliability from farms that grew the variety and the check.

    The share of such farms where the variety outyields the check, and where
    it does so by the threshold. Zero yields stay in. A pair where both plots
    failed counts as no win.
    """
    w = data.pivot_table(index=["trial", "farm"], columns="genotype", values="yield", aggfunc="mean")
    rows = []
    for g in [c for c in w.columns if c != check]:
        p = w[[g, check]].dropna()
        n = len(p)
        win0 = int((p[g] > p[check]).sum())
        wint = int(((p[g] >= (1 + threshold) * p[check]) & (p[g] > 0)).sum())
        lo, hi = wilson(wint, n)
        rows.append(dict(genotype=g, n_pairs=n, wins=win0, wins_threshold=wint,
                         direct_rel=win0 / n if n else np.nan,
                         direct_rel_threshold=wint / n if n else np.nan,
                         direct_lower=lo, direct_upper=hi))
    return pd.DataFrame(rows)


def harvest_failure(data):
    """Share of plots with zero yield per variety, the part a log model drops."""
    g = data.groupby("genotype")["yield"]
    return pd.DataFrame({"n_plots_all": g.size(), "n_failed": g.apply(lambda x: int((x <= 0).sum())),
                         "share_failed": g.apply(lambda x: float((x <= 0).mean()))}).reset_index()


def breakthrough_table(data, check, threshold=0.25, nboot=200, min_plots=30,
                       min_pairs=16, p_supported=0.8, seed=1):
    """The full table. One row per test variety."""
    con = trial_contrasts(data, check)
    if con.empty:
        raise ValueError("no trial has data on the check " + check)
    sigma2 = con.attrs["sigma2"]
    res = pool_trials(con)
    z = 1.959964
    res["gain_ratio"] = np.exp(res.log_gain)
    res["lower"] = np.exp(res.log_gain - z * res.se)
    res["upper"] = np.exp(res.log_gain + z * res.se)
    res["p_mean_gain"] = norm.cdf((res.log_gain - np.log1p(threshold)) / res.se)
    # a difference between two plots on one farm carries two residuals, plus
    # the variety-by-trial interaction
    res["sd_diff"] = np.sqrt(2 * sigma2 + res.tau2)
    res["rel"] = reliability_normal(res.log_gain, res.sd_diff, 0)
    res["rel_threshold"] = reliability_normal(res.log_gain, res.sd_diff, threshold)

    # interval of the reliability, resampling farms within trials
    rng = np.random.default_rng(seed)
    base = data[["trial", "farm", "genotype", "yield"]].reset_index(drop=True)
    farms = {t: d.farm.unique() for t, d in base.groupby("trial")}
    idx = base.groupby(["trial", "farm"]).indices
    boot = []
    for _ in range(nboot):
        rows, ids = [], []
        for t, f in farms.items():
            for j, k in enumerate(rng.choice(f, len(f))):
                rows.append(idx[(t, k)])
                ids.append(np.full(len(idx[(t, k)]), f"{t}-{j}", dtype=object))
        bd = base.iloc[np.concatenate(rows)].copy()
        bd["farm"] = np.concatenate(ids)
        bc = trial_contrasts(bd, check)
        if bc.empty:
            continue
        bp = pool_trials(bc)
        bp["r"] = reliability_normal(bp.log_gain, np.sqrt(2 * bc.attrs["sigma2"] + bp.tau2), threshold)
        boot.append(bp.set_index("genotype").r)
    B = pd.concat(boot, axis=1) if boot else pd.DataFrame(index=res.genotype)
    res["rel_threshold_lower"] = res.genotype.map(B.quantile(0.025, axis=1))
    res["rel_threshold_upper"] = res.genotype.map(B.quantile(0.975, axis=1))

    res = res.merge(direct_reliability(data, check, threshold), on="genotype", how="left")
    res = res.merge(harvest_failure(data), on="genotype", how="left")
    res["enough_pairs"] = res.n_pairs >= min_pairs

    def verdict(r):
        if r.n_plots < min_plots:
            return "insufficient data"
        if r.p_mean_gain >= p_supported:
            return "supported"
        if r.gain_ratio >= 1 + threshold:
            return "indicative"
        return "not supported"
    res["verdict"] = res.apply(verdict, axis=1)
    res = res.sort_values("log_gain", ascending=False).reset_index(drop=True)
    res.attrs.update(skipped=con.attrs["skipped"], sigma2=sigma2, check=check, threshold=threshold,
                     plots_in=len(data), plots_zero=int((data["yield"] <= 0).sum()),
                     check_failed=float((data.loc[data.genotype == check, "yield"] <= 0).mean()))
    return res


# ------------------------------------------------------------ preference ----
def _pl_fit(R, n):
    """Plackett-Luce log-worth, first item as reference. R holds, per farm, the
    three item indices ordered from best to worst."""
    def nll(t):
        w = np.concatenate([[0], t])[R]
        return -(w[:, 0] - np.logaddexp.reduce(w, axis=1) + w[:, 1] - np.logaddexp(w[:, 1], w[:, 2])).sum()
    return np.concatenate([[0], minimize(nll, np.zeros(n - 1), method="BFGS").x])


def preference_reliability(rankings, check, nboot=0, seed=1):
    """Probability that farmers rank each variety above the check.

    rankings has the columns farm, genotype and rank (1 is best), three plots
    per farm, no ties. Plackett-Luce worth by maximum likelihood, reliability
    as worth / (worth + worth of the check), as gosset::reliability(). With
    nboot > 0, a 95% interval from resampling farms.
    """
    r = rankings.dropna(subset=["rank"])
    ok = r.groupby("farm")["rank"].apply(lambda x: sorted(x) == [1, 2, 3])
    r = r[r.farm.isin(ok[ok].index)]
    if check not in set(r.genotype):
        return pd.DataFrame(columns=["genotype", "p_preferred", "p_lower", "p_upper", "n_rankings", "n_rankings_check"])
    items = [check] + sorted(set(r.genotype) - {check})
    ix = {v: i for i, v in enumerate(items)}
    R = np.array([[ix[v] for v in x.sort_values("rank").genotype] for _, x in r.groupby("farm")])
    n = len(items)
    rel = lambda t: 1 / (1 + np.exp(-t[1:]))
    out = pd.DataFrame({"genotype": items[1:], "p_preferred": rel(_pl_fit(R, n)),
                        "n_rankings": [int((R == i).any(axis=1).sum()) for i in range(1, n)]})
    out["n_rankings_check"] = int((R == 0).any(axis=1).sum())
    out["p_lower"], out["p_upper"] = np.nan, np.nan
    if nboot > 0:
        rng = np.random.default_rng(seed)
        B = np.array([rel(_pl_fit(R[rng.integers(0, len(R), len(R))], n)) for _ in range(nboot)])
        out["p_lower"], out["p_upper"] = np.percentile(B, 2.5, axis=0), np.percentile(B, 97.5, axis=0)
    return out


def trait_profile(rankings, check, traits=None, nboot=200, min_rankings=30, seed=1):
    """Profile of a variety against the check, one row per trait and variety.

    Yield is one driver of what farmers choose, not the single driver. The
    profile gives, for each trait farmers ranked (overall preference,
    marketability, maturity, disease and pest resistance, and others), the
    probability that the variety is ranked above the check.

    rankings is a long table with farm, genotype, trait and rank. Each trait
    is fitted on its own, since farmers and trials differ in which traits
    they ranked. reads is "above the check" when the whole interval is above
    0.5, "below the check" when it is under, "no clear difference" otherwise
    and "insufficient data" under min_rankings.
    """
    traits = traits or sorted(rankings.trait.unique())
    out = []
    for tr in traits:
        p = preference_reliability(rankings[rankings.trait == tr], check, nboot=nboot, seed=seed)
        p.insert(0, "trait", tr)
        out.append(p)
    out = pd.concat(out, ignore_index=True)

    def reads(r):
        if r.n_rankings < min_rankings or r.n_rankings_check < min_rankings:
            return "insufficient data"
        if r.p_lower > 0.5:
            return "above the check"
        if r.p_upper < 0.5:
            return "below the check"
        return "no clear difference"
    out["reads"] = out.apply(reads, axis=1) if len(out) else []
    return out


STATION_NOTE = ("On-farm data only. Ideally, combine these results with station trials "
                "(replicated, measured performance across locations) before a decision.")


# ------------------------------------------------------------- simulation ----
TRAIT_WORTH = {  # log-worth against the check, per ranked trait
    "marketability":      [-0.5, 0.2, 0.7, 0.0, 0.1, -0.2, 0.3],
    "maturity":           [0.4, -0.3, 0.2, 0.0, 0.0, 0.5, -0.4],
    "disease_resistance": [0.6, 0.3, -0.2, 0.1, 0.0, -0.5, 0.2],
    "pest_resistance":    [0.0, 0.4, 0.0, -0.3, 0.2, 0.0, -0.2],
}


def simulate_breakthrough(n_trials=6, farms_per_trial=150, seed=2026, sd_farm=0.6,
                          sd_resid=0.35, sd_gxt=0.05, p_fail=0.05, check_yield=800,
                          check_in_all=True):
    """On-farm trials with a known answer.

    Yield. Variety 01 is a breakthrough on yield (true gain 40%), Variety 02
    sits on the threshold (25%), Variety 03 gains 10% and the others are
    close to the check. Plots fail (zero yield) with the same probability for
    every variety.

    Other traits. Farmers also rank marketability, maturity, disease
    resistance and pest resistance, each with its own true worth. Variety 01
    wins on yield and disease resistance and loses on marketability. Variety
    03 has a modest yield gain and the best marketability. Overall preference
    weighs yield and these traits, so Variety 03 is preferred overall more
    than the yield breakthrough. The rankings are in attrs["rankings"].
    """
    rng = np.random.default_rng(seed)
    gain = {"Check": 1.0, "Variety 01": 1.40, "Variety 02": 1.25, "Variety 03": 1.10,
            "Variety 04": 1.03, "Variety 05": 1.00, "Variety 06": 0.97, "Variety 07": 0.90}
    items = list(gain)
    worth = {t: dict(zip(items, [0.0] + w)) for t, w in TRAIT_WORTH.items()}
    worth["overall_performance"] = {g: 1.5 * np.log(gain[g]) + 0.8 * worth["marketability"][g] +
                                    0.3 * worth["maturity"][g] + 0.3 * worth["disease_resistance"][g] +
                                    0.2 * worth["pest_resistance"][g] for g in items}
    rows, rk = [], []
    for t in range(n_trials):
        avail = items if (check_in_all or t > 0) else items[1:]
        gxt = {g: rng.normal(0, sd_gxt) for g in items}
        for f in range(farms_per_trial):
            fe = rng.normal(0, sd_farm)
            farm = f"T{t + 1}-{f + 1}"
            grown = rng.choice(avail, 3, replace=False)
            for g in grown:
                v = np.log(check_yield) + np.log(gain[g]) + gxt[g] - gxt["Check"] + fe + rng.normal(0, sd_resid)
                y = 0.0 if rng.uniform() < p_fail else float(np.exp(v))
                rows.append({"trial": f"T{t + 1}", "farm": farm, "genotype": g, "yield": y})
            for tr, w in worth.items():
                u = np.array([w[g] - np.log(-np.log(rng.uniform())) for g in grown])
                for g, r in zip(grown, np.argsort(np.argsort(-u)) + 1):
                    rk.append({"trial": f"T{t + 1}", "farm": farm, "genotype": g, "trait": tr, "rank": int(r)})
    dat = pd.DataFrame(rows)
    sd_d = np.sqrt(2 * sd_resid ** 2 + 2 * sd_gxt ** 2)
    truth = pd.DataFrame({"genotype": items[1:], "gain_ratio": [gain[g] for g in items[1:]]})
    truth["rel"] = norm.cdf(np.log(truth.gain_ratio) / sd_d)
    truth["rel_threshold"] = norm.cdf((np.log(truth.gain_ratio) - np.log(1.25)) / sd_d)
    for tr, w in worth.items():
        truth["p_" + tr] = [1 / (1 + np.exp(-w[g])) for g in items[1:]]
    dat.attrs["truth"] = truth
    dat.attrs["rankings"] = pd.DataFrame(rk)
    return dat
