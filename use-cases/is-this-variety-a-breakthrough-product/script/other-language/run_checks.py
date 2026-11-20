# Checks of the Python reference, the same cases as tests/test-*.R
# Run from the use-case folder:  python3 script/other-language/run_checks.py
import os
import sys
import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from breakthrough import (simulate_breakthrough, breakthrough_table, trial_contrasts, pool_trials,
                          reliability_normal, direct_reliability, preference_reliability, trait_profile, STATION_NOTE)

pd.set_option("display.width", 250, "display.max_columns", 40)
passed = []


def check(name, cond):
    assert cond, name
    passed.append(name)
    print("ok  ", name)


# 1. the simulated gain is recovered, averaged over five simulated networks
# without variety-by-trial interaction and with many farms, so the check is on
# the estimator and not on the luck of a few trials
d = simulate_breakthrough(farms_per_trial=2000, sd_gxt=0, seed=1)
est = np.exp(pool_trials(trial_contrasts(d, "Check")).set_index("genotype").log_gain)
truth = simulate_breakthrough().attrs["truth"].set_index("genotype")
check("gain ratio within 0.03 of the simulated value in a large network", (est - truth.gain_ratio).abs().max() < 0.03)

# with interaction, the 95% interval covers the simulated gain in most networks
cover = []
for s in range(1, 41):
    p = pool_trials(trial_contrasts(simulate_breakthrough(seed=s), "Check")).set_index("genotype")
    lg = np.log(truth.gain_ratio)
    cover.append(((p.log_gain - 1.96 * p.se <= lg) & (lg <= p.log_gain + 1.96 * p.se)).mean())
print("interval coverage over 40 simulated networks", round(float(np.mean(cover)), 3))
check("95% interval covers the simulated gain in at least 85% of cases", np.mean(cover) >= 0.85)

# 2. verdicts on the default data
dat = simulate_breakthrough()
tab = breakthrough_table(dat, "Check", nboot=200)
v = tab.set_index("genotype").verdict
check("the 40% variety is supported", v["Variety 01"] == "supported")
check("varieties close to the check are not supported", all(v[g] == "not supported" for g in ["Variety 05", "Variety 06", "Variety 07"]))
check("the variety on the threshold is not called supported", v["Variety 02"] != "supported")

# 3. reliability behaves as in Eskridge and Mumm
check("reliability is 0.5 when the mean gain equals the threshold", abs(reliability_normal(np.log(1.25), 0.5, 0.25) - 0.5) < 1e-12)
check("reliability at the threshold is below reliability at zero", (tab.rel_threshold < tab.rel).all())
t = tab.set_index("genotype")
check("model reliability at the threshold within 0.08 of the simulated value", (t.rel_threshold - truth.rel_threshold).abs().max() < 0.08)
big = direct_reliability(simulate_breakthrough(farms_per_trial=1500, p_fail=0), "Check").set_index("genotype")
check("direct win share agrees with the simulated reliability in a large network", abs(big.direct_rel["Variety 01"] - truth.rel["Variety 01"]) < 0.05)

# 4. a less stable variety has the same mean gain and a lower reliability
check("larger variance of the differences lowers reliability", reliability_normal(np.log(1.4), 0.8) < reliability_normal(np.log(1.4), 0.4))

# 5. a trial without the check is skipped and reported
d2 = simulate_breakthrough(check_in_all=False)
c2 = trial_contrasts(d2, "Check")
check("trial without the check is skipped and named", c2.attrs["skipped"] == [("T1", "check not grown")] and "T1" not in set(c2.trial))

# 6. zero yields are counted, not silently dropped
check("zero yields are reported", tab.attrs["plots_zero"] == int((dat["yield"] == 0).sum()) and tab.attrs["plots_zero"] > 0)
check("failure share per variety is reported", tab.share_failed.between(0, 0.15).all())

# 7. too few plots gives no verdict
small = breakthrough_table(simulate_breakthrough(n_trials=1, farms_per_trial=30), "Check", nboot=20)
check("fewer than 30 plots is insufficient data", (small.loc[small.n_plots < 30, "verdict"] == "insufficient data").all() and (small.n_plots < 30).any())

# 8. the check matters
other = breakthrough_table(dat, "Variety 07", nboot=20).set_index("genotype")
check("a weaker check raises the gain", other.gain_ratio["Variety 01"] > t.gain_ratio["Variety 01"])

# 9. the trait profile, beyond yield
rk = dat.attrs["rankings"]
prof = trait_profile(rk, "Check", nboot=200)
pv = prof.pivot(index="genotype", columns="trait", values="p_preferred")
tcols = ["marketability", "maturity", "disease_resistance", "pest_resistance", "overall_performance"]
err = (pv[tcols] - truth[["p_" + c for c in tcols]].set_axis(tcols, axis=1)).abs().max().max()
check("trait probabilities within 0.08 of the simulated values", err < 0.08)
rd = prof.set_index(["genotype", "trait"]).reads
check("the yield breakthrough reads below the check on marketability", rd[("Variety 01", "marketability")] == "below the check")
check("the yield breakthrough reads above the check on disease resistance", rd[("Variety 01", "disease_resistance")] == "above the check")
check("a modest yielder is preferred overall more than the yield breakthrough", pv.loc["Variety 03", "overall_performance"] > pv.loc["Variety 01", "overall_performance"])
check("a trait without difference reads no clear difference", rd[("Variety 04", "marketability")] == "no clear difference")
few = trait_profile(simulate_breakthrough(n_trials=1, farms_per_trial=30).attrs["rankings"], "Check", nboot=20)
check("few rankings read insufficient data", (few.reads == "insufficient data").any())
nochk = trait_profile(rk[rk.genotype != "Check"], "Check", nboot=0)
check("a trait never ranked beside the check returns no rows", len(nochk) == 0)
check("the station note is part of the output", "station" in STATION_NOTE)
pr = prof[prof.trait == "overall_performance"].set_index("genotype")

print(f"\n{len(passed)} checks passed\n")
cols = ["genotype", "gain_ratio", "lower", "upper", "p_mean_gain", "rel", "rel_threshold", "rel_threshold_lower",
        "rel_threshold_upper", "n_plots", "n_trials", "n_pairs", "direct_rel", "direct_rel_threshold", "share_failed", "verdict"]
print(tab[cols].round(2).to_string(index=False))
print("\ntruth\n", truth.round(2).to_string())
print("\ntrait profile, probability of being ranked above the check\n", pv.round(2).to_string())
print("\nVariety 01 and 03\n", prof[prof.genotype.isin(["Variety 01", "Variety 03"])].round(2).to_string(index=False))
print("\n" + STATION_NOTE)
print("\nplots", tab.attrs["plots_in"], "zero", tab.attrs["plots_zero"], "residual variance", round(tab.attrs["sigma2"], 3))
os.makedirs("output", exist_ok=True)
tab.to_csv("output/breakthrough-table-python.csv", index=False)
prof.to_csv("output/trait-profile-python.csv", index=False)
