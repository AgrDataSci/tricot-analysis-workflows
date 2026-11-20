# Checks of the Python reference, the same cases as tests/test-*.R
# Run from the use-case folder:  python3 script/other-language/run_checks.py
import os
import sys
import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from repeatability import (simulate_segments, as_rankings, _encode, pl_fit, repeatability, group_repeatability,
                           repeatability_curve, equal_n, segment_test, cv_gain, benchmark_reliability, worth_table, individual_fit, build_segments, adjusted_rand,
                           czekanowski, cross_dataset_similarity)
from envirotypes import climate_indices, screen_indices, envirotypes, simulate_climate

pd.set_option("display.width", 250, "display.max_columns", 40)
passed = []


def check(name, cond):
    assert cond, name
    passed.append(name)
    print("ok  ", name)


dat = simulate_segments()
rank = as_rankings(dat)
truth = dat.attrs["worth"]

# 1. the model recovers the simulated worth
big = simulate_segments(n=20000, seed=3)
wt = worth_table(as_rankings(big), big.segment)
tc = big.attrs["worth"] - big.attrs["worth"].mean()
check("log-worth within 0.1 of the simulated values in a large sample", (wt[["market", "home"]] - tc).abs().max().max() < 0.1)

# 2. bad answers are dropped and counted
bad = dat.copy()
bad.loc[0, "worst"] = bad.loc[0, "best"]
bad.loc[1, "best"] = np.nan
check("inconsistent and missing answers are dropped and counted", as_rankings(bad).attrs["dropped"] == 2 and len(as_rankings(bad)) == len(dat) - 2)

# 3. variances shrink with sample size, and repeatability rises
R, lev = _encode(rank)
v_small = pl_fit(R[:300], len(lev))[1].mean()
v_all = pl_fit(R, len(lev))[1].mean()
check("variances of the log-worths shrink with more participants", v_all < v_small)
curve = repeatability_curve(rank, sizes=(50, 100, 200, 400, 800), B=100)
print(curve.round(3).to_string(index=False))
check("repeatability rises with the number of participants", curve.repeatability.is_monotonic_increasing)

# 4. repeatability is the share of real variance
check("no difference between items gives zero repeatability", repeatability(np.zeros(8), np.full(8, 0.05))[0] == 0.0)
check("large differences with small variances give repeatability near one", repeatability(np.linspace(-2, 2, 8), np.full(8, 0.001))[0] > 0.99)
est, var = pl_fit(R, len(lev))
check("the Cullis form has a ceiling near 1 - 1/k with few items", abs(repeatability(np.linspace(-2, 2, 8), np.full(8, 0.001), cullis=True)[0] - 0.875) < 0.01)
check("more error variance lowers repeatability", repeatability(est, var * 10)[0] < repeatability(est, var)[0])

# 5. the fair test, on real and on meaningless segments
st = segment_test(rank, dat.segment, B=300)
sn = segment_test(rank, dat.noise, B=300)
print(pd.DataFrame({"true segments": st, "noise label": sn}).round(3).to_string())
check("true segments differ in preference", st["p_deviance"] < 0.01)
check("true segments are more repeatable than random segments", st["p_repeatability"] < 0.01)
check("a noise label does not differ in preference", sn["p_deviance"] > 0.05)
check("a noise label is not more repeatable than random segments", sn["p_repeatability"] > 0.05)
check("random segments are less repeatable than the pooled data, the cost of a smaller sample", st["repeatability_random"] < st["repeatability_pooled"])

# 6. at equal n, true segments beat the pooled data and a noise label does not
eq = equal_n(rank, dat.segment, m=200, B=150).set_index("group")
en = equal_n(rank, dat.noise, m=200, B=150).set_index("group")
print(eq.round(3).to_string())
print(en.round(3).to_string())
check("true segments are more repeatable than the pooled data at the same n", (eq.loc[["home", "market"], "repeatability"] > eq.loc["pooled", "repeatability"]).all())
check("split-half correlation is higher within true segments", (eq.loc[["home", "market"], "split_half_r"] > eq.loc["pooled", "split_half_r"]).all())
# single groups move by chance, here 0.46 and 0.78 around a pooled 0.66, so the
# equal-n table describes and segment_test() decides
check("a noise label gains nothing on average at the same n", abs(en.loc[["x", "y"], "repeatability"].mean() - en.loc["pooled", "repeatability"]) < 0.1)

# 7. identical segments gain nothing
same = simulate_segments(contrast=0)
ss = segment_test(as_rankings(same), same.segment, B=300)
check("segments with identical preferences are not more repeatable", ss["p_repeatability"] > 0.05)

# 8. prediction of held-out rankings
cg, cn = cv_gain(rank, dat.segment), cv_gain(rank, dat.noise)
print("cv gain, true segments", round(cg["gain_per_ranking"], 4), "noise label", round(cn["gain_per_ranking"], 4))
check("true segments predict held-out rankings better", cg["gain_per_ranking"] > 0.02)
check("a noise label does not predict better", cn["gain_per_ranking"] < 0.005)

# 9. reliability against a reference item
br = benchmark_reliability(rank, "C4", dat.segment, B=100)
p = br.set_index(["group", "item"]).reliability
check("the reference changes sides between segments", p[("market", "C1")] > 0.6 and p[("home", "C1")] < 0.5)
check("reliability lies between 0 and 1 with ordered intervals", ((br.lower <= br.reliability) & (br.reliability <= br.upper)).all())

# 10. single answers are harder to predict than item means are to repeat
fi = individual_fit(rank, dat.segment)
check("pseudo R2 of single answers is far below the repeatability of the item means", fi["pseudo_r2_pooled"] < 0.2 < group_repeatability(R, len(lev))[0])
check("true segments raise the pseudo R2 of single answers", fi["pseudo_r2_segments"] > fi["pseudo_r2_pooled"] + 0.02)

# 11. blocked folds are used when blocks are given
blk = np.arange(len(rank)) % 6
check("cross-validation by blocks reports its folds", cv_gain(rank, dat.segment, blocks=blk)["folds"] == "blocks")

# 12. segments built from covariates alone
rng = np.random.default_rng(5)
cov = pd.DataFrame({"sold": np.where(dat.segment == "market", rng.normal(70, 10, len(dat)), rng.normal(15, 10, len(dat))).clip(0, 100),
                    "land": np.where(dat.segment == "market", rng.lognormal(1.2, 0.4, len(dat)), rng.lognormal(0.2, 0.4, len(dat)))})
lab, prof, stab = build_segments(cov, 2, log=["land"])
check("covariates that follow the true segments recover them", adjusted_rand(lab, dat.segment) > 0.8)
check("a clear partition is stable", stab > 0.9)
noise_cov = pd.DataFrame(rng.normal(size=(len(dat), 3)))
check("a partition of pure noise is unstable", build_segments(noise_cov, 3)[2] < 0.7)
check("adjusted Rand is 1 for identical partitions and near 0 for unrelated ones", adjusted_rand(lab, lab) == 1.0 and abs(adjusted_rand(lab, dat.noise)) < 0.05)
check("segments built from covariates pass the fair test", segment_test(rank, lab.values, B=200)["p_repeatability"] < 0.01)

# 13. envirotypes from daily climate
pts = [f"P{i:02d}" for i in range(40)]
zone = np.where(np.arange(40) < 20, "wet", "dry")
rain, temp = simulate_climate(pts, zone)
ind = climate_indices(rain, temp)
z = pd.Series(zone, index=pts)
check("wet points have more rain, cooler days and shorter dry spells", ind.rain_total[z == "wet"].min() > ind.rain_total[z == "dry"].max() and ind.tmax_mean[z == "wet"].max() < ind.tmax_mean[z == "dry"].min() and ind.max_dry_spell[z == "wet"].mean() < ind.max_dry_spell[z == "dry"].mean())
check("every point rests on the five simulated years", (ind.years_rain == 5).all() and (ind.years_temp == 5).all())
env, eprof, estab, used = envirotypes(ind, 2)
check("two envirotypes recover the two simulated zones", adjusted_rand(env.loc[pts], zone) == 1.0 and estab > 0.9)
short = rain[(pd.to_datetime(rain.date).dt.year < 2011) | (pd.to_datetime(rain.date).dt.month <= 6)]
check("an incomplete year is left out", (climate_indices(short).years_rain == 4).all())
flat = ind.copy()
flat["hot_days"] = 100.0
check("an index that does not vary is dropped", "hot_days" not in screen_indices(flat)[0])
check("strongly correlated indices are reported", len(screen_indices(ind)[2]) > 0)
# preferences that follow the zone: the envirotype of a participant's point passes the fair test
part_pt = rng.choice(pts, len(dat))
tied = simulate_segments(seed=11)
tied_seg = np.where(tied.segment == "market", "wet", "dry")
wet_pts, dry_pts = [p for p in pts if z[p] == "wet"], [p for p in pts if z[p] == "dry"]
tied_pt = np.where(tied_seg == "wet", rng.choice(wet_pts, len(tied)), rng.choice(dry_pts, len(tied)))
check("envirotypes pass the fair test when preference follows climate", segment_test(as_rankings(tied), env.loc[tied_pt].values, B=200)["p_repeatability"] < 0.01)
check("envirotypes fail it when preference ignores climate", segment_test(rank, env.loc[part_pt].values, B=200)["p_repeatability"] > 0.05)

# 14. Czekanowski similarity between data sets
check("Czekanowski is 1 for equal shares and 0 for disjoint ones", czekanowski([1, 2, 3], [2, 4, 6]) == 1.0 and czekanowski([1, 0], [0, 1]) == 0.0)
same = cross_dataset_similarity(rank, dat.noise.values, sizes=(50, 200), B=100)
check("two samples of one population are not perfectly similar, less so when small", same.within_x[0] < same.within_x[1] < 0.99)
check("data sets from one population are as similar between as within", abs(same.between[1] - same.within_x[1]) < 0.02 and same.noise_share[1] > 0.8)
diff = cross_dataset_similarity(rank, dat.segment.values, sizes=(50, 200), B=100)
check("data sets from different populations fall below their noise floor", diff.between[1] < diff.within_home[1] - 0.1 and diff.p_below_within[1] > 0.95)
check("the noise share shrinks as n grows when the data sets differ", diff.noise_share[1] < diff.noise_share[0] < 1)

print(f"\n{len(passed)} checks passed")
