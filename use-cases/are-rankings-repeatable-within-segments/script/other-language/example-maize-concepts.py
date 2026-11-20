# Worked example on open data. Video-based concept testing of eight maize
# seed product concepts with 2,400 farmers in Kenya and Uganda (Donovan et
# al. 2025), replication data at https://doi.org/10.5281/zenodo.15063837,
# CC BY 4.0, https://github.com/AgrDataSci/maize-concept-testing
# Run:  python3 script/other-language/example-maize-concepts.py <path to a clone of maize-concept-testing>
import os
import sys
import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from repeatability import (as_rankings, repeatability_curve, equal_n, segment_test, cv_gain,
                           benchmark_reliability, worth_table, individual_fit)

root = sys.argv[1]
B = int(sys.argv[2]) if len(sys.argv) > 2 else 500
pd.set_option("display.width", 250, "display.max_columns", 40, "display.max_rows", 300)
os.makedirs("output", exist_ok=True)

# concept names as harmonised in the published replication script
NAMES = {"drought avoidance": "Drought escape", "green maize": "Green maize", "home use": "Home use",
         "home use & family consumption": "Home use", "intercropping": "Intercropping",
         "nutritious": "Family nutrition", "resilient": "Resilient (benchmark)", "resilience": "Resilient (benchmark)",
         "feed": "Livestock feed", "food & fodder": "Food and fodder", "feed & fodder": "Food and fodder"}
REF = "Resilient (benchmark)"


def read(country):
    d = pd.read_csv(os.path.join(root, "data", country.lower() + ".csv"), low_memory=False)
    d["country"] = country
    d["area"] = d["subcountyid"]  # six sub-counties per country
    for v in ("video1", "video2", "video3"):
        d[v] = d[v].str.lower().str.replace(" variety", "", regex=False).str.strip().map(NAMES)
    keep = ["country", "area", "video1", "video2", "video3", "variety_preferred", "variety_lesspreferred",
            "resp_gender", "X_elevation", "maize_bags", "grain_sold"]
    return d[keep]


dat = pd.concat([read("Kenya"), read("Uganda")], ignore_index=True)
n_in = len(dat)
dat = dat[dat.resp_gender.notna()].reset_index(drop=True)
dat["area"] = dat.country + " " + dat.area.astype(int).astype(str)

# segments, all defined without looking at the rankings
share = dat.grain_sold / dat.maize_bags.replace(0, np.nan)
dat["market"] = pd.cut(share, [-0.01, 0, 0.4999, 1], labels=["sells none", "sells under half", "sells half or more"]).astype(object)
dat["sells"] = np.where(share.isna(), None, np.where(share > 0, "sells maize", "sells none"))
dat["elevation"] = dat.groupby("country").X_elevation.transform(
    lambda x: pd.qcut(x, 3, labels=["low", "middle", "high"]).astype(object))
dat["gender"] = dat.resp_gender
dat["country_gender"] = dat.country + ", " + dat.gender
dat["country_sells"] = np.where(dat.sells.isna(), None, dat.country + ", " + dat.sells.fillna(""))

rank = as_rankings(dat, items=("video1", "video2", "video3"), best="variety_preferred", worst="variety_lesspreferred")
dat = dat.loc[rank.index].reset_index(drop=True)
rank = rank.reset_index(drop=True)
print("rows read", n_in, "| with an interview", len(dat) + rank.attrs["dropped"], "| valid rankings", len(rank))
print(dat.groupby("country")[["gender", "market", "elevation"]].agg(lambda x: x.value_counts(dropna=False).to_dict()).T.to_string())
print(dat.groupby("country").X_elevation.describe().round(0).to_string())

fit = individual_fit(rank, dat.country)
print("pseudo R2 of single answers, pooled", round(fit["pseudo_r2_pooled"], 3), "| by country", round(fit["pseudo_r2_segments"], 3))

# 1. how repeatable is the unsegmented ranking, and at what n
print("\n== repeatability of the unsegmented data against n")
for name, idx in [("both countries", dat.index), ("Kenya", dat.index[dat.country == "Kenya"]), ("Uganda", dat.index[dat.country == "Uganda"])]:
    c = repeatability_curve(rank.loc[idx].reset_index(drop=True), sizes=(50, 100, 200, 400, 800), B=200)
    c.insert(0, "data", name)
    print(c.round(3).to_string(index=False))
    c.to_csv(f"output/maize-curve-{name.lower().replace(' ', '-')}.csv", index=False)

# 2. the fair test of each segmentation
print("\n== segment tests,", B, "permutations")
tests = []
scopes = [("both countries", dat.index, ["country", "gender", "market", "sells", "country_gender", "country_sells"]),
          ("Kenya", dat.index[dat.country == "Kenya"], ["gender", "market", "sells", "elevation", "area"]),
          ("Uganda", dat.index[dat.country == "Uganda"], ["gender", "market", "sells", "elevation", "area"])]
for scope, idx, segs in scopes:
    r, d = rank.loc[idx].reset_index(drop=True), dat.loc[idx].reset_index(drop=True)
    for sg in segs:
        t = segment_test(r, d[sg], B=B)
        cv = cv_gain(r, d[sg], blocks=d["area"] if sg != "area" else None)
        t.update(scope=scope, segmentation=sg, cv_gain=cv["gain_per_ranking"], cv_folds=cv["folds"])
        tests.append(t)
tests = pd.DataFrame(tests)
cols = ["scope", "segmentation", "n", "segments", "smallest", "p_deviance", "repeatability_pooled",
        "repeatability_segments", "repeatability_random", "random_lower", "random_upper", "p_repeatability", "cv_gain"]
print(tests[cols].round(3).to_string(index=False))
tests.to_csv("output/maize-segment-tests.csv", index=False)

# 3. equal n, descriptive
print("\n== repeatability at the same n (m = 150)")
for sg in ["country", "country_gender", "country_sells"]:
    e = equal_n(rank, dat[sg], m=150, B=200)
    e.insert(0, "segmentation", sg)
    print(e.round(3).to_string(index=False))
    e.to_csv(f"output/maize-equal-n-{sg}.csv", index=False)

# 3b. do results hold across places. Halves made of whole sub-counties, so the
# two halves share no village. All ten ways of splitting six sub-counties 3 + 3
from itertools import combinations
print("\n== split-half between groups of sub-counties (n about 600 per half)")
for c in ("Kenya", "Uganda"):
    d = dat[dat.country == c]
    areas = sorted(d.area.unique())
    lev = sorted(pd.unique(rank[["first", "second", "third"]].values.ravel()))
    rs = []
    for half in combinations(areas, 3):
        if areas[0] not in half:
            continue
        i1, i2 = d.index[d.area.isin(half)], d.index[~d.area.isin(half)]
        w1 = worth_table(rank.loc[i1].reset_index(drop=True)).reindex(lev).pooled
        w2 = worth_table(rank.loc[i2].reset_index(drop=True)).reindex(lev).pooled
        rs.append(np.corrcoef(w1, w2)[0, 1])
    print(c, "splits", len(rs), "mean r", round(float(np.mean(rs)), 3), "min", round(float(np.min(rs)), 3), "max", round(float(np.max(rs)), 3))

# 4. worths and reliability against the benchmark concept
w = worth_table(rank, dat.country)
print("\n== centred log-worth\n", w.round(2).to_string())
print("correlation of log-worth between Kenya and Uganda", round(float(np.corrcoef(w.Kenya, w.Uganda)[0, 1]), 2))
br = benchmark_reliability(rank, REF, dat.country, B=300)
print("\n== probability of being ranked above", REF)
print(br.pivot(index="item", columns="group", values="reliability").round(2).to_string())
br.to_csv("output/maize-reliability-benchmark.csv", index=False)
w.to_csv("output/maize-worth.csv")
