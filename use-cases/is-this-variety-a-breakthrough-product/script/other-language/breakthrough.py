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
def preference_reliability(rankings, check):
    """Probability that farmers rank each variety above the check.

    rankings has the columns farm, genotype and rank (1 is best), three plots
    per farm, no ties. Plackett-Luce worth by maximum likelihood, reliability
    as worth / (worth + worth of the check), as gosset::reliability().
    """
    r = rankings.dropna(subset=["rank"])
    ok = r.groupby("farm")["rank"].apply(lambda x: sorted(x) == [1, 2, 3])
    r = r[r.farm.isin(ok[ok].index)]
    items = [check] + sorted(set(r.genotype) - {check})
    ix = {v: i for i, v in enumerate(items)}
    R = np.array([[ix[v] for v in x.sort_values("rank").genotype] for _, x in r.groupby("farm")])

    def nll(t):
        w = np.concatenate([[0], t])[R]
        return -(w[:, 0] - np.logaddexp.reduce(w, axis=1) + w[:, 1] - np.logaddexp(w[:, 1], w[:, 2])).sum()
    th = np.concatenate([[0], minimize(nll, np.zeros(len(items) - 1), method="BFGS").x])
    return pd.DataFrame({"genotype": items[1:], "p_preferred": 1 / (1 + np.exp(-th[1:])),
                         "n_rankings": [int((R == i).any(axis=1).sum()) for i in range(1, len(items))]})


# ------------------------------------------------------------- simulation ----
def simulate_breakthrough(n_trials=6, farms_per_trial=150, seed=2026, sd_farm=0.6,
                          sd_resid=0.35, sd_gxt=0.05, p_fail=0.05, check_yield=800,
                          check_in_all=True):
    """On-farm trials with a known answer.

    Variety 01 is a breakthrough product (true gain 40%), Variety 02 sits on
    the threshold (25%), Variety 03 gains 10% and the others are close to the
    check. Each farm grows three varieties. Plots fail (zero yield) with the
    same probability for every variety.
    """
    rng = np.random.default_rng(seed)
    gain = {"Check": 1.0, "Variety 01": 1.40, "Variety 02": 1.25, "Variety 03": 1.10,
            "Variety 04": 1.03, "Variety 05": 1.00, "Variety 06": 0.97, "Variety 07": 0.90}
    items = list(gain)
    rows = []
    for t in range(n_trials):
        avail = items if (check_in_all or t > 0) else items[1:]
        gxt = {g: rng.normal(0, sd_gxt) for g in items}
        for f in range(farms_per_trial):
            fe = rng.normal(0, sd_farm)
            grown = rng.choice(avail, 3, replace=False)
            ly = [np.log(check_yield) + np.log(gain[g]) + gxt[g] - gxt["Check"] + fe + rng.normal(0, sd_resid) for g in grown]
            u = [2 * np.log(gain[g]) - np.log(-np.log(rng.uniform())) for g in grown]
            rk = np.argsort(np.argsort(-np.array(u))) + 1
            for g, v, r in zip(grown, ly, rk):
                y = 0.0 if rng.uniform() < p_fail else float(np.exp(v))
                rows.append(dict(trial=f"T{t + 1}", farm=f"T{t + 1}-{f + 1}", genotype=g, **{"yield": y, "rank": int(r)}))
    dat = pd.DataFrame(rows)
    sd_d = np.sqrt(2 * sd_resid ** 2 + 2 * sd_gxt ** 2)
    truth = pd.DataFrame({"genotype": items[1:], "gain_ratio": [gain[g] for g in items[1:]]})
    truth["rel"] = norm.cdf(np.log(truth.gain_ratio) / sd_d)
    truth["rel_threshold"] = norm.cdf((np.log(truth.gain_ratio) - np.log(1.25)) / sd_d)
    dat.attrs["truth"] = truth
    return dat
