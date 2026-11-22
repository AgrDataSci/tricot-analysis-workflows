# Python port of script/helper/functions.R of the maniva use case
# "Do different groups of users prefer different varieties?"
# (AgrDataSci/tricot-analysis-workflows, commit 55c8561).
#
# Status: UNTESTED against the R functions. The port is checked on synthetic data with
# the known answers of the use case (tests in script/other-language/test_user_groups.py).
#
# Differences from the R functions
#   - Plackett-Luce fits come from the reference implementation below (complete rankings of
#     three items, no ties, npseudo = 0.5 as in the PlackettLuce package).
#   - cluster_varsel() reimplements the model of VarSelLCM (latent classes with conditional
#     independence, Gaussian for numeric and multinomial for categorical variables, each
#     variable declared relevant or irrelevant by BIC). It is not the VarSelLCM code and
#     its search differs, so partitions can differ from the R result.
#   - Quasi-variances follow Firth and de Menezes (2004), least squares on the log scale.
import itertools
import numpy as np
import pandas as pd
from scipy.optimize import minimize, least_squares
from scipy.stats import chi2, kendalltau, norm, poisson


# ------------------------------------------------------------- model ----
def encode(rank, levels=None):
    """rank holds the columns first, second, third (item names from best to worst)."""
    levels = list(levels) if levels is not None else sorted(pd.unique(rank[["first", "second", "third"]].values.ravel()))
    ix = {v: i for i, v in enumerate(levels)}
    return np.column_stack([rank[c].map(ix).values for c in ("first", "second", "third")]).astype(int), levels


def pl_fit(R, n, weights=None, npseudo=0.5, cov=False):
    """Plackett-Luce log-worth with item 0 as reference, the log-likelihood of the observed
    rankings and, with cov=True, the covariance matrix of the log-worths."""
    a, b, c = R[:, 0], R[:, 1], R[:, 2]
    w = np.ones(len(R)) if weights is None else np.asarray(weights, float)
    g = n

    def nll_grad(t):
        th = np.concatenate([[0.0], t])
        ea, eb, ec = np.exp(th[a]), np.exp(th[b]), np.exp(th[c])
        s3, s2 = ea + eb + ec, eb + ec
        ll = (w * (th[a] - np.log(s3) + th[b] - np.log(s2))).sum()
        gr = np.zeros(n + 1)
        np.add.at(gr, a, w * (1 - ea / s3))
        np.add.at(gr, b, w * (1 - eb / s3 - eb / s2))
        np.add.at(gr, c, w * (-ec / s3 - ec / s2))
        if npseudo > 0:
            ti, tg = th[:n], th[g]
            lse = np.logaddexp(ti, tg)
            ll += npseudo * ((ti - lse).sum() + (tg - lse).sum())
            p = np.exp(ti - lse)
            gr[:n] += npseudo * (1 - 2 * p)
            gr[g] += npseudo * (2 * p - 1).sum()
        return -ll, -gr[1:]

    o = minimize(nll_grad, np.zeros(n), jac=True, method="BFGS")
    th = np.concatenate([[0.0], o.x])[:n]
    x, y, z = th[a], th[b], th[c]
    ll = float((w * (x - np.logaddexp.reduce([x, y, z], axis=0) + y - np.logaddexp(y, z))).sum())
    if not cov:
        return th, ll, None
    k = len(o.x)
    H = np.zeros((k, k))
    for j in range(k):
        e = np.zeros(k)
        e[j] = 1e-4
        H[:, j] = (nll_grad(o.x + e)[1] - nll_grad(o.x - e)[1]) / 2e-4
    V = np.zeros((n + 1, n + 1))
    V[1:, 1:] = np.linalg.pinv((H + H.T) / 2)
    return th, ll, V[:n, :n]


def quasi_variances(V):
    """Quasi-variances q with q_i + q_j close to var(b_i - b_j) for every pair
    (Firth and de Menezes 2004), by least squares on the log scale."""
    n = len(V)
    i, j = np.triu_indices(n, 1)
    v = V[i, i] + V[j, j] - 2 * V[i, j]
    if (v <= 0).any() or not np.isfinite(v).all():
        raise ValueError("contrast variances are not positive")
    f = lambda q: np.log(np.maximum(q[i] + q[j], 1e-12)) - np.log(v)
    o = least_squares(f, np.full(n, v.mean() / 2))
    return o.x, float(np.sqrt(np.mean(o.fun ** 2)))


# -------------------------------------------- A. predefined groups ----
def group_eligibility(group, min_n=20, exclude=("Not specified", "")):
    """Which groups have enough answers to be compared."""
    g = pd.Series(group).dropna().astype(str)
    tb = g.value_counts()
    labelled = tb[~tb.index.isin(exclude)].sum()
    out = pd.DataFrame({"group": tb.index, "n": tb.values, "share": tb.values / labelled})
    out["eligible"] = (out.n >= min_n) & ~out.group.isin(exclude)
    out["reason"] = np.where(out.group.isin(exclude), "no_label", np.where(out.n < min_n, "too_few", "ok"))
    out.loc[out.group.isin(exclude), "share"] = np.nan
    return out.sort_values("n", ascending=False).reset_index(drop=True)


def confounding_check(group, stratum, max_v=0.3):
    """Cramer's V between group and stratum. Above max_v the group is flagged as confounded."""
    tab = pd.crosstab(pd.Series(group, name="group"), pd.Series(stratum, name="stratum"))
    e = np.outer(tab.sum(1), tab.sum(0)) / tab.values.sum()
    chi = ((tab.values - e) ** 2 / e).sum()
    v = float(np.sqrt(chi / (tab.values.sum() * (min(tab.shape) - 1))))
    return dict(counts=tab, cramers_v=v, confounded=v > max_v)


def _cell_loglik(R, n, cell):
    ll = 0.0
    for k in pd.unique(cell):
        ll += pl_fit(R[cell == k], n)[1]
    return ll, len(pd.unique(cell)) * (n - 1)


def preference_heterogeneity(R, n, group, nperm=200, seed=1):
    """One model for everyone against one model per group. Likelihood ratio with the
    chi-squared p value and a permutation p value (labels shuffled among answers)."""
    group = np.asarray(group, dtype=object)
    ll0 = pl_fit(R, n)[1]

    def dev(g):
        return 2 * (_cell_loglik(R, n, g)[0] - ll0)
    obs = dev(group)
    df = (len(pd.unique(group)) - 1) * (n - 1)
    rng = np.random.default_rng(seed)
    perm = np.array([dev(rng.permutation(group)) for _ in range(nperm)])
    return dict(deviance=obs, df=df, p_chisq=float(chi2.sf(obs, df)),
                p_permutation=float((1 + (perm >= obs).sum()) / (nperm + 1)), n=len(R))


def stratified_heterogeneity(R, n, group, stratum, min_cell=10, nperm=200, seed=1):
    """Do groups differ in preference within strata? One model per stratum against one
    model per stratum and group. Strata where any group has fewer than min_cell answers
    are left out and listed. The permutation shuffles group labels within each stratum."""
    group, stratum = np.asarray(group, dtype=object), np.asarray(stratum, dtype=object)
    cells = pd.crosstab(stratum, group)
    ok = cells.index[(cells >= min_cell).all(axis=1)].tolist()
    dropped = [s for s in cells.index if s not in ok]
    if not ok:
        raise ValueError(f"no stratum has at least {min_cell} answers in every group")
    keep = np.isin(stratum, ok)
    R, group, stratum = R[keep], group[keep], stratum[keep]
    l0, d0 = _cell_loglik(R, n, stratum)

    def dev(g):
        cell = np.array([f"{s}::{x}" for s, x in zip(stratum, g)], dtype=object)
        l1, d1 = _cell_loglik(R, n, cell)
        return 2 * (l1 - l0), d1 - d0
    obs, df = dev(group)
    p_perm = np.nan
    if nperm > 0:
        rng = np.random.default_rng(seed)
        idx = [np.where(stratum == s)[0] for s in ok]
        perm = np.empty(nperm)
        for b in range(nperm):
            g = group.copy()
            for i in idx:
                g[i] = g[rng.permutation(i)]
            perm[b] = dev(g)[0]
        p_perm = float((1 + (perm >= obs).sum()) / (nperm + 1))
    return dict(deviance=obs, df=df, p_chisq=float(chi2.sf(obs, df)), p_permutation=p_perm, n=len(R),
                strata_used=", ".join(map(str, ok)), strata_dropped=", ".join(map(str, dropped)))


def worth_with_intervals(R, levels, group, ref, level=0.95):
    """Log-worth per group relative to a reference item, with quasi-standard errors,
    intervals, the n of the group and how often each item was ranked in it."""
    n, r = len(levels), levels.index(ref)
    z = norm.ppf(1 - (1 - level) / 2)
    group = np.asarray(group, dtype=object)
    rows = []
    for g in sorted(pd.unique(group), key=str):
        Rg = R[group == g]
        th, _, V = pl_fit(Rg, n, cov=True)
        try:
            q, err = quasi_variances(V)
            qse = np.sqrt(np.where(q > 0, q, np.nan))
        except Exception:
            qse, err = np.full(n, np.nan), np.nan
        cnt = np.bincount(Rg.ravel(), minlength=n)
        est = th - th[r]
        for i, it in enumerate(levels):
            rows.append(dict(group=g, n=len(Rg), item=it, estimate=est[i], quasi_se=qse[i],
                             lower=est[i] - z * qse[i], upper=est[i] + z * qse[i], n_ranked=int(cnt[i]),
                             qv_relative_error=err))
    return pd.DataFrame(rows)


def agreement_between_groups(R, levels, group, top=3):
    """Kendall tau between the worth of each pair of groups and the top items they share."""
    n = len(levels)
    group = np.asarray(group, dtype=object)
    w = {g: pl_fit(R[group == g], n)[0] for g in sorted(pd.unique(group), key=str)}
    rows = []
    for a, b in itertools.combinations(w, 2):
        ta = set(np.argsort(-w[a])[:top])
        tb = set(np.argsort(-w[b])[:top])
        rows.append(dict(group_a=a, group_b=b, kendall_tau=kendalltau(w[a], w[b])[0],
                         shared_top=len(ta & tb), top=top))
    return pd.DataFrame(rows)


def balanced_ranking(R, levels, group):
    """Pooled worth weighs every answer equally. Balanced worth gives every group the same
    total weight. Report both when the smallest group holds under 25% of the answers."""
    n = len(levels)
    group = pd.Series(np.asarray(group, dtype=object))
    wt = 1 / group.map(group.value_counts()).values.astype(float)
    wt = wt / wt.mean()
    share = lambda th: 100 * np.exp(th) / np.exp(th).sum()
    wp, wb = share(pl_fit(R, n)[0]), share(pl_fit(R, n, weights=wt)[0])
    out = pd.DataFrame({"item": levels, "worth_pooled": wp, "worth_balanced": wb})
    out["rank_pooled"] = out.worth_pooled.rank(ascending=False, method="min").astype(int)
    out["rank_balanced"] = out.worth_balanced.rank(ascending=False, method="min").astype(int)
    return out.sort_values("rank_balanced").reset_index(drop=True)


def pairwise_heterogeneity(R, n, group, nperm=0, seed=1):
    """Likelihood-ratio test of one model against one per cluster, for every pair of clusters."""
    group = np.asarray(group, dtype=object)
    rows = []
    for a, b in itertools.combinations(sorted(pd.unique(group), key=str), 2):
        keep = np.isin(group, [a, b])
        h = preference_heterogeneity(R[keep], n, group[keep], nperm=nperm, seed=seed)
        rows.append(dict(cluster_a=a, cluster_b=b, deviance=h["deviance"], df=h["df"], p_value=h["p_chisq"],
                         p_permutation=h["p_permutation"] if nperm > 0 else np.nan, n=h["n"]))
    return pd.DataFrame(rows)


# ------------------------------------- B. user types from covariates ----
def adjusted_rand(a, b):
    tab = pd.crosstab(pd.Series(np.asarray(a)), pd.Series(np.asarray(b))).values
    c2 = lambda x: x * (x - 1) / 2
    sij, sa, sb = c2(tab).sum(), c2(tab.sum(1)).sum(), c2(tab.sum(0)).sum()
    exp, mx = sa * sb / c2(tab.sum()), (sa + sb) / 2
    return 1.0 if mx == exp else float((sij - exp) / (mx - exp))


class _Var:
    """One variable of the latent class model. Continuous variables are Gaussian, counts
    (non-negative whole numbers) are Poisson and categorical variables multinomial, the
    families VarSelLCM assigns to numeric, integer and factor columns."""

    def __init__(self, x, kind=None):
        num = pd.api.types.is_numeric_dtype(x)
        if kind is None:
            v = np.asarray(x, float)[~np.isnan(np.asarray(x, float))] if num else None
            kind = "categorical" if not num else ("count" if (v >= 0).all() and np.allclose(v, np.round(v)) else "continuous")
        self.kind = kind
        self.numeric = kind != "categorical"
        if self.numeric:
            self.x = np.asarray(x, float)
            self.obs = ~np.isnan(self.x)
            self.floor = max(np.nanstd(self.x) * 0.05, 1e-8)
            self.nu = 2 if kind == "continuous" else 1
        else:
            s = pd.Series(x).astype(object)
            self.obs = s.notna().values
            self.codes, lev = pd.factorize(s)
            self.m = len(lev)
            self.nu = self.m - 1

    def logdens(self, T):
        """n x k log density with parameters from the responsibilities T, zero where missing."""
        n, k = T.shape
        out = np.zeros((n, k))
        o = self.obs
        if self.numeric:
            x = self.x[o]
            w = T[o]
            sw = w.sum(0) + 1e-12
            mu = (w * x[:, None]).sum(0) / sw
            if self.kind == "count":
                out[o] = poisson.logpmf(x[:, None], np.maximum(mu, 1e-8))
            else:
                sd = np.maximum(np.sqrt((w * (x[:, None] - mu) ** 2).sum(0) / sw), self.floor)
                out[o] = norm.logpdf(x[:, None], mu, sd)
        else:
            c = self.codes[o]
            w = T[o]
            p = np.zeros((self.m, k))
            np.add.at(p, c, w)
            p = (p + 1e-6) / (p + 1e-6).sum(0)
            out[o] = np.log(p[c])
        return out

    def pooled(self):
        return self.logdens(np.ones((len(self.obs), 1)))[:, 0]


def _lcm_em(vars_, rel, T, iters=300, tol=1e-7):
    n, k = T.shape
    last = -np.inf
    for _ in range(iters):
        pi = np.maximum(T.mean(0), 1e-10)
        L = np.log(pi)[None, :] + sum(v.logdens(T) for v, r in zip(vars_, rel) if r) if any(rel) else np.tile(np.log(pi), (n, 1))
        m = L.max(1, keepdims=True)
        lse = m[:, 0] + np.log(np.exp(L - m).sum(1))
        T = np.exp(L - lse[:, None])
        ll = lse.sum()
        if ll - last < tol:
            break
        last = ll
    return T, float(ll)


def _lcm_fit(vars_, k, rng, starts=25):
    n = len(vars_[0].obs)
    pooled = [v.pooled().sum() for v in vars_]
    best = None
    for _ in range(starts):
        T = rng.dirichlet(np.full(k, 0.3), n)
        rel = [True] * len(vars_)
        for _ in range(15):          # alternate EM and the relevance of each variable
            T, ll = _lcm_em(vars_, rel, T)
            new = []
            for v, pl in zip(vars_, pooled):
                r = (T * v.logdens(T)).sum() - v.nu * k / 2 * np.log(n)
                new.append(bool(r > pl - v.nu / 2 * np.log(n)))
            if new == rel:
                break
            rel = new
        T, ll = _lcm_em(vars_, rel, T)
        ll_total = ll + sum(p for p, r in zip(pooled, rel) if not r)
        npar = (k - 1) + sum(v.nu * (k if r else 1) for v, r in zip(vars_, rel))
        bic = ll_total - npar / 2 * np.log(n)
        z = T.argmax(1)
        icl = bic + np.log(np.maximum(T[np.arange(n), z], 1e-300)).sum()
        if best is None or bic > best["BIC"]:
            best = dict(BIC=bic, ICL=icl, T=T, labels=z, rel=rel, loglik=ll_total)
    return best


def cluster_varsel(covariates, k=(2, 3, 4, 5), criterion="ICL", seed=1, starts=25, kinds=None, min_share=0.0):
    """Model-based clustering of numeric and categorical covariates with variable selection.

    Latent classes with conditional independence. Each variable is declared relevant
    (its distribution differs between clusters) or irrelevant (one distribution for
    everyone) by BIC, so a variable unrelated to the segments does not shape them.
    The number of clusters is chosen by ICL, which penalises overlapping clusters.
    Missing values are left out of the likelihood. Reimplements the model of VarSelLCM.

    With min_share above zero, k is chosen among the solutions whose smallest cluster holds
    at least that share of the participants. k may include 1, no clusters at all.

    Whole-number columns are modelled as counts (Poisson). kinds overrides the family of a
    column, {"column": "continuous" | "count" | "categorical"}. A continuous variable with
    a spike of identical values (zeros in a share, a score with few distinct values) lets
    a Gaussian cluster collapse on the spike, so enter it as classes instead."""
    kinds = kinds or {}
    vars_ = [_Var(covariates[c], kinds.get(c)) for c in covariates.columns]
    rng = np.random.default_rng(seed)
    fits = {kk: _lcm_fit(vars_, kk, rng, starts) for kk in k}
    names = list(covariates.columns)
    crit = pd.DataFrame({"k": list(k), "BIC": [fits[kk]["BIC"] for kk in k], "ICL": [fits[kk]["ICL"] for kk in k]})
    crit["smallest_share"] = [np.bincount(fits[kk]["labels"], minlength=kk).min() / len(covariates) for kk in k]
    crit["relevant"] = ["; ".join(c for c, r in zip(names, fits[kk]["rel"]) if r) for kk in k]
    ok = crit[crit.smallest_share >= min_share]        # step 3 of the use case, the smallest cluster share
    kb = int(ok.k[ok[criterion].idxmax()]) if len(ok) else int(crit.k[crit[criterion].idxmax()])
    f = fits[kb]
    crit["family"] = "; ".join(f"{c} {v.kind}" for c, v in zip(names, vars_))
    return dict(labels=f["labels"] + 1, k=kb, criteria=crit, criterion=criterion,
                relevant=[c for c, r in zip(names, f["rel"]) if r],
                irrelevant=[c for c, r in zip(names, f["rel"]) if not r],
                posterior=f["T"], all_labels={kk: fits[kk]["labels"] + 1 for kk in k})


def variable_influence(covariates, labels, cluster_fun, max_factor_ari=0.8, min_loo_ari=0.5):
    """Refit leaving out one variable at a time and compare with the full partition, and
    compare the partition with each categorical variable. Flags a variable that drives it."""
    rows = []
    cols = list(covariates.columns)
    for v in cols:
        loo = adjusted_rand(labels, cluster_fun(covariates[[c for c in cols if c != v]])) if len(cols) > 1 else np.nan
        x = covariates[v]
        rep = np.nan
        if not pd.api.types.is_numeric_dtype(x):
            ok = x.notna().values
            rep = adjusted_rand(np.asarray(labels)[ok], x[ok].values)
        rows.append(dict(variable=v, ari_without=loo, ari_with_variable=rep))
    out = pd.DataFrame(rows)
    out["drives_partition"] = (out.ari_with_variable > max_factor_ari) | (out.ari_without < min_loo_ari)
    return out


def cluster_profile(covariates, labels):
    """Mean of numeric and share of categorical covariates per cluster."""
    lab = pd.Series(np.asarray(labels), index=covariates.index)
    rows = {}
    for v in covariates.columns:
        x = covariates[v]
        if pd.api.types.is_numeric_dtype(x):
            rows[v] = x.groupby(lab).mean()
        else:
            tab = pd.crosstab(x, lab, normalize="columns")
            for lev in tab.index:
                rows[f"{v} = {lev}"] = tab.loc[lev]
    out = pd.DataFrame(rows).T
    out.columns = [f"cluster_{c}" for c in out.columns]
    return out
