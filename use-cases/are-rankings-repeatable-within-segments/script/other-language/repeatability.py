# Python reference for the use case "Are rankings repeatable within segments?"
# Decision record: decisions/repeatability.md
#
# This file mirrors script/helper/functions.R and script/helper/simulate-data.R.
# It exists because the R scripts could not be executed when the use case was
# drafted. Every number in the README comes from this code. numpy, pandas and
# scipy stand in for PlackettLuce, qvcalc and gosset. The Plackett-Luce fit
# handles complete rankings of three items without ties, which is what a
# tricot comparison with a best and a worst choice gives.
#
# Scientific intent
#   Measure how repeatable a ranking of items (varieties or product concepts)
#   is, and test whether it is more repeatable within segments (market
#   segments, envirotypes) than in the unsegmented data.
# Approach
#   Step 1. Plackett-Luce log-worth of each item with its variance, per group
#     (Dorado-Betancourt et al. 2026).
#   Step 2. Variance among items against the variance of their estimates, as
#     the repeatability of the item means (Cullis form, as their qH2()).
#   Fair test. Segmenting shrinks the sample, so every comparison is made at
#     the same number of participants, and against random segments of the
#     same sizes.
import numpy as np
import pandas as pd
from scipy.optimize import minimize, minimize_scalar
from scipy.stats import kendalltau


# ------------------------------------------------------------ rankings ----
def as_rankings(data, items=("item_1", "item_2", "item_3"), best="best", worst="worst"):
    """Order the three items of each participant from best to worst.

    best and worst hold the position (A, B or C) of the item chosen as best
    and as worst. Rows with a missing or inconsistent answer are dropped and
    counted in attrs["dropped"].
    """
    pos = {"A": 0, "B": 1, "C": 2}
    b, w = data[best].map(pos), data[worst].map(pos)
    ok = b.notna() & w.notna() & (b != w) & data[list(items)].notna().all(axis=1)
    d = data[ok]
    b, w = b[ok].astype(int).values, w[ok].astype(int).values
    m = 3 - b - w
    it = d[list(items)].values
    r = np.arange(len(d))
    out = pd.DataFrame({"first": it[r, b], "second": it[r, m], "third": it[r, w]}, index=d.index)
    out.attrs["dropped"] = int((~ok).sum())
    return out


def _encode(rank, levels=None):
    levels = list(levels) if levels is not None else sorted(pd.unique(rank[["first", "second", "third"]].values.ravel()))
    ix = {v: i for i, v in enumerate(levels)}
    return np.column_stack([rank[c].map(ix).values for c in ("first", "second", "third")]).astype(int), levels


# -------------------------------------------------------- step 1, model ----
def pl_fit(R, n, npseudo=0.5, variances=True):
    """Plackett-Luce log-worth, centred on zero, with the variance of each.

    R holds, per participant, the indices of the three items from best to
    worst. npseudo adds a win and a loss of each item against a hypothetical
    item, the default of the PlackettLuce package, so that an item that was
    never chosen as best or worst still has a finite estimate.
    """
    a, b, c = R[:, 0], R[:, 1], R[:, 2]
    g = n  # index of the hypothetical item

    def nll_grad(t):
        th = np.concatenate([[0.0], t])  # item 0 is the reference, the ghost is last
        ea, eb, ec = np.exp(th[a]), np.exp(th[b]), np.exp(th[c])
        s3, s2 = ea + eb + ec, eb + ec
        ll = (th[a] - np.log(s3) + th[b] - np.log(s2)).sum()
        gr = np.zeros(n + 1)
        np.add.at(gr, a, 1 - ea / s3)
        np.add.at(gr, b, 1 - eb / s3 - eb / s2)
        np.add.at(gr, c, -ec / s3 - ec / s2)
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
    est = th - th.mean()
    if not variances:
        return est, None
    k = len(o.x)
    H = np.zeros((k, k))
    for j in range(k):  # Hessian from central differences of the gradient
        e = np.zeros(k)
        e[j] = 1e-4
        H[:, j] = (nll_grad(o.x + e)[1] - nll_grad(o.x - e)[1]) / 2e-4
    V = np.zeros((n + 1, n + 1))
    V[1:, 1:] = np.linalg.pinv((H + H.T) / 2)
    V = V[:n, :n]
    C = np.eye(n) - 1.0 / n
    return est, np.diag(C @ V @ C.T)


# ------------------------------------------------ step 2, repeatability ----
def repeatability(est, var, cullis=False):
    """Repeatability of the item means from their estimates and variances.

    The items are a random effect with variance sigma2, observed with known
    error variances (the weighted second step of Dorado-Betancourt et al.
    2026, dispersion fixed at one). sigma2 is estimated by REML. The
    repeatability is the mean of sigma2 / (sigma2 + variance), the share of
    the variance among estimated item means that is real difference between
    items and not estimation error. Zero when no variance among items is
    found.

    Their qH2() uses the Cullis form, 1 - mean(PEV) / sigma2, where the
    prediction error also counts the uncertainty of the overall mean. With
    k items that puts a ceiling of about 1 - 1/k on the result, 0.875 for
    eight concepts however large the sample. The form here leaves that term
    out, since the log-worths are already centred. With many items the two
    agree. cullis=True returns their form.
    """
    y, v = np.asarray(est, float), np.asarray(var, float)

    def reml(s2):
        w = 1 / (s2 + v)
        mu = np.sum(w * y) / w.sum()
        return 0.5 * (np.sum(np.log(s2 + v)) + np.log(w.sum()) + np.sum(w * (y - mu) ** 2))
    hi = max(np.var(y, ddof=1) * 4, 1e-6)
    s2 = minimize_scalar(reml, bounds=(0, hi), method="bounded", options={"xatol": 1e-10}).x
    if s2 < 1e-8 or reml(0.0) <= reml(s2):
        return 0.0, 0.0
    if cullis:
        w = 1 / (s2 + v)
        P = np.diag(w) - np.outer(w, w) / w.sum()
        return float(1 - (s2 - s2 ** 2 * np.diag(P)).mean() / s2), float(s2)
    return float(np.mean(s2 / (s2 + v))), float(s2)


def group_repeatability(R, n):
    est, var = pl_fit(R, n)
    r, s2 = repeatability(est, var)
    return r, s2, est


# ---------------------------------------------------------- comparisons ----
def repeatability_curve(rank, sizes=(50, 100, 200, 400, 800, 1600), B=200, seed=1):
    """Repeatability of the unsegmented data against the number of participants.

    Subsamples without replacement. Read it as sample-size guidance, the
    number of participants at which the ranking of the items stabilises.
    """
    R, lev = _encode(rank)
    rng = np.random.default_rng(seed)
    rows = []
    for m in [s for s in sizes if s <= len(R)]:
        x = [group_repeatability(R[rng.choice(len(R), m, replace=False)], len(lev))[0] for _ in range(B)]
        rows.append(dict(n=m, repeatability=np.mean(x), lower=np.percentile(x, 2.5), upper=np.percentile(x, 97.5)))
    r = group_repeatability(R, len(lev))
    rows.append(dict(n=len(R), repeatability=r[0], lower=np.nan, upper=np.nan))
    return pd.DataFrame(rows)


def equal_n(rank, segment, m, B=200, seed=1):
    """Repeatability of the pooled data and of each segment at the same n.

    For the pooled data and for every segment with at least m participants,
    draw B subsamples of m and compute the repeatability. Where a group has
    at least 2m, also draw two disjoint subsamples of m and correlate their
    log-worths (split-half, Pearson and Kendall). A segment gains only if
    it beats the pooled data at the same m.
    """
    R, lev = _encode(rank)
    seg = pd.Series(segment).reset_index(drop=True)
    rng = np.random.default_rng(seed)
    groups = {"pooled": np.arange(len(R))}
    groups.update({str(k): np.where(seg.values == k)[0] for k in sorted(seg.dropna().unique(), key=str)})
    rows = []
    for name, idx in groups.items():
        row = dict(group=name, n=len(idx), m=m)
        if len(idx) >= m:
            x = [group_repeatability(R[rng.choice(idx, m, replace=False)], len(lev))[0] for _ in range(B)]
            row.update(repeatability=np.mean(x), lower=np.percentile(x, 2.5), upper=np.percentile(x, 97.5))
        if len(idx) >= 2 * m:
            pr, kt = [], []
            for _ in range(B):
                s = rng.choice(idx, 2 * m, replace=False)
                e1 = pl_fit(R[s[:m]], len(lev), variances=False)[0]
                e2 = pl_fit(R[s[m:]], len(lev), variances=False)[0]
                pr.append(np.corrcoef(e1, e2)[0, 1])
                kt.append(kendalltau(e1, e2)[0])
            row.update(split_half_r=np.mean(pr), split_half_lower=np.percentile(pr, 2.5),
                       split_half_upper=np.percentile(pr, 97.5), split_half_tau=np.mean(kt))
        rows.append(row)
    return pd.DataFrame(rows)


def _loglik(R, est):
    a, b, c = est[R[:, 0]], est[R[:, 1]], est[R[:, 2]]
    return float((a - np.logaddexp.reduce([a, b, c], axis=0) + b - np.logaddexp(b, c)).sum())


def segment_test(rank, segment, B=500, seed=1):
    """Do the segments differ, and does segmenting raise repeatability?

    Two statistics, each against random segments of the same sizes
    (segment labels shuffled among participants).
      deviance. Twice the gain in log-likelihood of one model per segment
        over one pooled model. A small p says preferences differ between
        the segments.
      repeatability. The mean repeatability of the segments, weighted by
        their size. A small p says the segments are more repeatable than
        random groups of the same sizes, which is the claim under test.
    Random segments carry the same loss of sample size as real ones, so
    the comparison is fair.
    """
    R, lev = _encode(rank)
    n = len(lev)
    seg = pd.Series(segment).reset_index(drop=True)
    keep = seg.notna().values
    R, lab = R[keep], seg[keep].values
    ll0 = _loglik(R, pl_fit(R, n, variances=False)[0])

    def stats(lab):
        dev, rep, wt = -2 * ll0, 0.0, 0
        for k in np.unique(lab):
            Rk = R[lab == k]
            est, var = pl_fit(Rk, n)
            dev += 2 * _loglik(Rk, est)
            rep += len(Rk) * repeatability(est, var)[0]
            wt += len(Rk)
        return dev, rep / wt
    obs = stats(lab)
    rng = np.random.default_rng(seed)
    null = np.array([stats(rng.permutation(lab)) for _ in range(B)])
    pooled = group_repeatability(R, n)[0]
    return dict(n=len(R), segments=int(len(np.unique(lab))), smallest=int(pd.Series(lab).value_counts().min()),
                deviance=obs[0], deviance_null=float(null[:, 0].mean()),
                p_deviance=float((1 + (null[:, 0] >= obs[0]).sum()) / (B + 1)),
                repeatability_segments=obs[1], repeatability_random=float(null[:, 1].mean()),
                random_lower=float(np.percentile(null[:, 1], 2.5)), random_upper=float(np.percentile(null[:, 1], 97.5)),
                p_repeatability=float((1 + (null[:, 1] >= obs[1]).sum()) / (B + 1)),
                repeatability_pooled=pooled)


def cv_gain(rank, segment, blocks=None, k=10, reps=5, seed=1):
    """Gain in held-out log-likelihood per ranking from one model per segment.

    Positive when knowing the segment of a participant predicts their
    ranking better than the pooled model. With blocks (village, district,
    season), each block is held out in turn, so the test fold holds places
    the training folds did not see, as the climate decision record asks.
    Without blocks, k random folds, repeated.
    """
    R, lev = _encode(rank)
    n = len(lev)
    seg = pd.Series(segment).reset_index(drop=True)
    keep = seg.notna().values
    R, lab = R[keep], seg[keep].values
    rng = np.random.default_rng(seed)
    if blocks is not None:
        blk = pd.Series(blocks).reset_index(drop=True)[keep].values
        folds = [pd.factorize(blk)[0]]
    else:
        folds = [rng.permutation(len(R)) % k for _ in range(reps)]
    gains = []
    for fold in folds:
        g = 0.0
        for f in np.unique(fold):
            tr, te = fold != f, fold == f
            pooled = pl_fit(R[tr], n, variances=False)[0]
            g -= _loglik(R[te], pooled)
            for s in np.unique(lab):
                if (te & (lab == s)).any():
                    est = pl_fit(R[tr & (lab == s)], n, variances=False)[0] if (tr & (lab == s)).sum() >= 20 else pooled
                    g += _loglik(R[te & (lab == s)], est)
        gains.append(g / len(R))
    return dict(gain_per_ranking=float(np.mean(gains)),
                gain_sd=float(np.std(gains, ddof=1)) if len(gains) > 1 else np.nan,
                folds="blocks" if blocks is not None else f"{k} random folds x {reps}")


def individual_fit(rank, segment=None):
    """How well the model predicts one participant's ranking.

    McFadden pseudo R2, one minus the log-likelihood of the model over that
    of choosing at random among the six orders of three items. It is about
    single answers. Repeatability is about the item means. A ranking of
    items can be highly repeatable while single answers stay hard to
    predict, and the two should not be confused.
    """
    R, lev = _encode(rank)
    n = len(lev)
    ll0 = len(R) * np.log(1 / 6)
    out = dict(n=len(R), pseudo_r2_pooled=1 - _loglik(R, pl_fit(R, n, variances=False)[0]) / ll0)
    if segment is not None:
        seg = pd.Series(segment).reset_index(drop=True).values
        keep = pd.notna(seg)
        ll = sum(_loglik(R[seg == k], pl_fit(R[seg == k], n, variances=False)[0]) for k in pd.unique(seg[keep]))
        out["pseudo_r2_segments"] = 1 - ll / (keep.sum() * np.log(1 / 6))
    return out


def benchmark_reliability(rank, ref, segment=None, B=300, seed=1):
    """Probability that each item is ranked above a reference item.

    Reliability in the sense of Eskridge and Mumm (1992), from Plackett-Luce
    worth as gosset::reliability() computes it. Pooled, and per segment when
    one is given. Intervals from resampling participants.
    """
    R, lev = _encode(rank)
    n, r = len(lev), lev.index(ref)
    rng = np.random.default_rng(seed)
    groups = {"pooled": np.arange(len(R))}
    if segment is not None:
        seg = pd.Series(segment).reset_index(drop=True)
        groups.update({str(k): np.where(seg.values == k)[0] for k in sorted(seg.dropna().unique(), key=str)})
    rel = lambda e: 1 / (1 + np.exp(-(e - e[r])))
    rows = []
    for name, idx in groups.items():
        p = rel(pl_fit(R[idx], n, variances=False)[0])
        bt = np.array([rel(pl_fit(R[rng.choice(idx, len(idx))], n, variances=False)[0]) for _ in range(B)])
        for i, it in enumerate(lev):
            if i != r:
                rows.append(dict(group=name, n=len(idx), item=it, reliability=p[i],
                                 lower=np.percentile(bt[:, i], 2.5), upper=np.percentile(bt[:, i], 97.5)))
    return pd.DataFrame(rows)


def worth_table(rank, segment=None):
    """Centred log-worth of each item, pooled and per segment."""
    R, lev = _encode(rank)
    out = {"pooled": pl_fit(R, len(lev), variances=False)[0]}
    if segment is not None:
        seg = pd.Series(segment).reset_index(drop=True)
        for k in sorted(seg.dropna().unique(), key=str):
            out[str(k)] = pl_fit(R[seg.values == k], len(lev), variances=False)[0]
    return pd.DataFrame(out, index=lev)


# ------------------------------------------------------- building segments ----
def adjusted_rand(a, b):
    """Adjusted Rand index of two partitions of the same units, 1 for identical and about 0 for unrelated."""
    t = pd.crosstab(pd.Series(a).values, pd.Series(b).values).values.astype(float)
    c2 = lambda x: x * (x - 1) / 2
    n, s = t.sum(), c2(t).sum()
    ra, rb = c2(t.sum(axis=1)).sum(), c2(t.sum(axis=0)).sum()
    e = ra * rb / c2(n)
    return float((s - e) / ((ra + rb) / 2 - e)) if (ra + rb) / 2 != e else 1.0


def _kmeans(Z, k, rng, starts=20, iters=100):
    best, best_ss = None, np.inf
    for _ in range(starts):
        c = Z[rng.choice(len(Z), k, replace=False)]
        for _ in range(iters):
            lab = ((Z[:, None, :] - c[None, :, :]) ** 2).sum(axis=2).argmin(axis=1)
            new = np.array([Z[lab == j].mean(axis=0) if (lab == j).any() else c[j] for j in range(k)])
            if np.allclose(new, c):
                break
            c = new
        ss = ((Z - c[lab]) ** 2).sum()
        if ss < best_ss:
            best, best_ss = lab, ss
    return best


def build_segments(X, k, log=(), seed=1, stability_runs=50):
    """Segments from covariates alone, never from the rankings.

    X is a table of numeric covariates, one row per unit (a participant for
    market segments, a location for envirotypes). Columns named in log are
    log-transformed (log1p), all columns are standardised, missing values
    take the column median, and k-means gives k groups. Stability is the
    mean adjusted Rand index between the groups of the full data and the
    groups refitted on random 80% subsamples. A partition below about 0.7
    is not stable enough to name.

    k-means on standardised numeric covariates is the simplest defensible
    choice. With categorical covariates use model-based clustering or FAMD
    first, as the decision record on socioeconomic heterogeneity says.
    """
    X = pd.DataFrame(X).apply(pd.to_numeric, errors="coerce")
    T = X.copy()
    for c in log:
        T[c] = np.log1p(T[c].clip(lower=0))
    T = T.fillna(T.median())
    sd = T.std(ddof=0).replace(0, 1)
    Z = ((T - T.mean()) / sd).values
    rng = np.random.default_rng(seed)
    lab = _kmeans(Z, k, rng)
    aris = []
    for _ in range(stability_runs):
        idx = rng.choice(len(Z), int(0.8 * len(Z)), replace=False)
        aris.append(adjusted_rand(lab[idx], _kmeans(Z[idx], k, rng, starts=5)))
    order = np.argsort(-np.bincount(lab, minlength=k))  # label 1 is the largest group
    relab = np.empty(k, int)
    relab[order] = np.arange(1, k + 1)
    labels = pd.Series(relab[lab], index=X.index, name="segment")
    profile = X.groupby(labels.values).mean().assign(n=labels.value_counts().sort_index().values)
    return labels, profile, float(np.mean(aris))


# ----------------------------------------------------------- simulation ----
def simulate_segments(n=1200, seed=2026, share=0.5, contrast=1.0):
    """Tricot rankings of eight concepts from two segments with a known answer.

    Segment "market" prefers concepts C1 to C3 and segment "home" prefers C6
    to C8. Their preferences partly cancel in the pooled data, so the pooled
    worths are closer together and less repeatable at a given n. "noise" is a
    label unrelated to preference, a segmentation that should gain nothing.
    contrast scales how far the segments differ. With contrast = 0 the two
    segments are identical and segmenting should gain nothing either.
    """
    rng = np.random.default_rng(seed)
    items = [f"C{i}" for i in range(1, 9)]
    common = np.array([0.4, 0.2, 0.0, 0.0, 0.0, 0.0, -0.2, -0.4])
    spec = np.array([0.8, 0.6, 0.4, 0.0, 0.0, -0.4, -0.6, -0.8]) * contrast
    worth = {"market": common + spec, "home": common - spec}
    rows = []
    for i in range(n):
        s = "market" if rng.uniform() < share else "home"
        show = rng.choice(8, 3, replace=False)
        u = worth[s][show] - np.log(-np.log(rng.uniform(size=3)))
        order = np.argsort(-u)
        pos = np.array(["A", "B", "C"])
        rows.append(dict(id=i + 1, item_1=items[show[0]], item_2=items[show[1]], item_3=items[show[2]],
                         best=pos[order[0]], worst=pos[order[2]], segment=s,
                         noise="x" if rng.uniform() < 0.5 else "y"))
    dat = pd.DataFrame(rows)
    dat.attrs["worth"] = pd.DataFrame(worth, index=items)
    return dat
