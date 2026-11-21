# Checks of the Python port of the user-groups workflow on synthetic data with known answers,
# mirroring tests/ of the use case. Run with: python3 script/other-language/test_user_groups.py
import numpy as np
import pandas as pd
import user_groups as ug


def _rank(rng, lw, alloc):
    u = lw[alloc] + rng.gumbel(size=alloc.shape)
    o = np.argsort(-u, axis=1)
    return np.take_along_axis(alloc, o, axis=1)


def simulate_tricot(n=800, n_items=8, heterogeneity=True, seed=2026):
    """Two segments (40% market, 60% subsistence) with different covariates and opposite
    preferences, plus household head unrelated to the segment."""
    rng = np.random.default_rng(seed)
    m = rng.random(n) < 0.4
    X = pd.DataFrame({"farm_size": rng.normal(np.where(m, 1.0, 0.0), 0.5),          # already on the log scale
                      "share_sold": 1 / (1 + np.exp(-rng.normal(np.where(m, 1.2, -1.2), 0.8))),
                      "market_distance": rng.normal(np.where(m, 0.5, 1.7), 0.5),
                      "household_head": rng.choice(["woman", "man"], n)})
    lw = np.linspace(1.2, -1.2, n_items)
    alloc = np.array([rng.choice(n_items, 3, replace=False) for _ in range(n)])
    R = np.where(m[:, None], _rank(rng, lw, alloc), _rank(rng, lw[::-1] if heterogeneity else lw, alloc))
    return R, X, np.where(m, "market", "subsistence")


def simulate_known_groups(n_per_site=300, n_items=8, gender_effect=False, shift=1.5, seed=2026):
    """Two sites with opposite preferences, women concentrated in the south."""
    rng = np.random.default_rng(seed)
    site = np.repeat(["north", "south"], n_per_site)
    gender = np.where(rng.random(len(site)) < np.where(site == "north", 0.15, 0.75), "woman", "man")
    lw = np.linspace(1.2, -1.2, n_items)
    bonus = np.zeros(n_items)
    if gender_effect:
        bonus[[3, 5]] = shift
    alloc = np.array([rng.choice(n_items, 3, replace=False) for _ in range(len(site))])
    base = np.where((site == "north")[:, None], lw[alloc], lw[::-1][alloc])
    u = base + np.where((gender == "woman")[:, None], bonus[alloc], 0) + rng.gumbel(size=alloc.shape)
    return np.take_along_axis(alloc, np.argsort(-u, axis=1), axis=1), site, gender


def test_part_a():
    R, site, gender = simulate_known_groups()
    assert ug.confounding_check(gender, site)["confounded"]
    assert ug.preference_heterogeneity(R, 8, gender, nperm=200)["p_permutation"] < 0.05     # pooled, a site effect
    s = ug.stratified_heterogeneity(R, 8, gender, site, nperm=200)
    assert s["p_permutation"] > 0.05 and s["strata_dropped"] == ""                           # gone within sites
    R, site, gender = simulate_known_groups(gender_effect=True)
    assert ug.stratified_heterogeneity(R, 8, gender, site, nperm=200)["p_permutation"] < 0.05
    lev = [f"v{i}" for i in range(8)]
    w = ug.worth_with_intervals(R, lev, gender, "v0")
    assert w.quasi_se.notna().all() and (w.upper > w.lower).all()
    b = ug.balanced_ranking(R, lev, gender)
    assert abs(b.worth_pooled.sum() - 100) < 1e-6 and abs(b.worth_balanced.sum() - 100) < 1e-6
    e = ug.group_eligibility(["a"] * 30 + ["b"] * 5 + ["Not specified"] * 3)
    assert e.set_index("group").reason.to_dict() == {"a": "ok", "b": "too_few", "Not specified": "no_label"}
    print("part A ok", round(s["p_permutation"], 3))


def test_part_b():
    R, X, seg = simulate_tricot()
    vs = ug.cluster_varsel(X, k=(2, 3, 4, 5))
    ari = ug.adjusted_rand(vs["labels"], seg)
    assert vs["k"] == 2, vs["criteria"]
    assert ari > 0.8, ari
    assert "household_head" in vs["irrelevant"], vs["relevant"]
    assert ug.adjusted_rand(vs["labels"], X.household_head) < 0.05
    h = ug.preference_heterogeneity(R, 8, vs["labels"], nperm=200)
    assert h["p_permutation"] < 0.01
    R0, X0, _ = simulate_tricot(heterogeneity=False)
    assert ug.preference_heterogeneity(R0, 8, ug.cluster_varsel(X0, k=(2,))["labels"], nperm=200)["p_permutation"] > 0.05
    inf = ug.variable_influence(X, vs["labels"], lambda Z: ug.cluster_varsel(Z, k=(2,))["labels"])
    assert not inf.drives_partition[inf.variable == "household_head"].any()
    Xm = X.copy()
    Xm.loc[Xm.sample(80, random_state=1).index, "share_sold"] = np.nan                     # missing values are tolerated
    assert ug.adjusted_rand(ug.cluster_varsel(Xm, k=(2,))["labels"], seg) > 0.75
    Xc = X.assign(n_crops=np.random.default_rng(3).poisson(np.where(seg == "market", 2, 6)))    # counts are Poisson
    vc = ug.cluster_varsel(Xc, k=(1, 2, 3))
    assert vc["k"] == 2 and "n_crops count" in vc["criteria"].family[0] and "n_crops" in vc["relevant"]
    noise = pd.DataFrame(np.random.default_rng(4).normal(size=(400, 3)), columns=list("abc"))        # no structure, k = 1
    assert ug.cluster_varsel(noise, k=(1, 2, 3))["k"] == 1
    print("part B ok, k =", vs["k"], "ARI =", round(ari, 2), "irrelevant =", vs["irrelevant"])
    print(vs["criteria"].round(1).to_string())


def test_quasi_variances():
    rng = np.random.default_rng(1)
    q = rng.uniform(0.02, 0.2, 6)                       # independent estimates have exact quasi-variances
    est, err = ug.quasi_variances(np.diag(q))
    assert np.allclose(est, q, rtol=1e-3) and err < 1e-6
    print("quasi-variances ok")


if __name__ == "__main__":
    test_quasi_variances()
    test_part_a()
    test_part_b()
