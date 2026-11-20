# Worked example on open data. Video-based concept testing of eight maize
# seed product concepts with 2,400 farmers in Kenya and Uganda (Donovan et
# al. 2025), from tricot-data, files maize-KEN-6834d470d4cv.json and
# maize-UGA-862ed6d6adfv.json (CC BY 4.0). The files hold the rankings, 30
# household variables and the interview point rounded to two decimals.
# Run:  python3 script/other-language/example-maize-concepts.py <path to a clone of tricot-data> [permutations]
import json
import os
import sys
from itertools import combinations

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from repeatability import (repeatability_curve, equal_n, segment_test, cv_gain, build_segments,
                           benchmark_reliability, worth_table, individual_fit)

root = sys.argv[1]
B = int(sys.argv[2]) if len(sys.argv) > 2 else 500
pd.set_option("display.width", 250, "display.max_columns", 40, "display.max_rows", 300)
os.makedirs("output", exist_ok=True)
REF = "Resilient"  # the benchmark concept of the study


def read(trial_id):
    idx = pd.read_csv(os.path.join(root, "datasets.csv"))
    row = idx[idx.trial_id == trial_id].iloc[0]
    assert row["type"] == "concept testing"
    j = json.load(open(os.path.join(root, row["path"])))
    site = pd.DataFrame(j["site_data"])
    p = pd.DataFrame(j["plot_data"])
    p = p[(p.trait_original == "concept_preference") & (p.value_type == "rank")]
    p["rank"] = pd.to_numeric(p.value)
    w = p.pivot(index="site_id", columns="rank", values="genotype_name")
    ok = w[[1, 2, 3]].notna().all(axis=1)
    rank = w.loc[ok, [1, 2, 3]].set_axis(["first", "second", "third"], axis=1)
    site = site.set_index("site_id").loc[rank.index]
    print(row.country_name, "| sites", len(j["site_data"]), "| complete rankings", len(rank), "| licence", row.license)
    return site.reset_index(), rank.reset_index(drop=True)


parts = [read("6834d470d4cv"), read("862ed6d6adfv")]
dat = pd.concat([p[0] for p in parts], ignore_index=True)
rank = pd.concat([p[1] for p in parts], ignore_index=True)
num = lambda c: pd.to_numeric(dat[c], errors="coerce")

# ---- segments, all defined without looking at the rankings ----
share = num("grain_sold") / num("farm_harvest_quantity").replace(0, np.nan)
dat["share_sold"] = pd.cut(share, [-0.01, 0, 0.4999, 1], labels=["sells none", "sells under half", "sells half or more"]).astype(object)
role = dat.crop_farming_role.fillna("")
dat["decides_variety"] = np.where(role.str.contains("decision-making power"), "decides on the variety", "does not decide")
dat["sells_role"] = np.where(role.str.contains("selling the maize"), "takes part in selling", "does not sell")
dat["income"] = dat.farm_income_share.map({"almost all from farm": "mostly farm", "most from farm": "mostly farm",
                                           "half from off-farm": "half or more off-farm",
                                           "most from off-farm": "half or more off-farm",
                                           "all or almost all from off-farm": "half or more off-farm"})
dat["seed_buying"] = np.where(dat.seed_purchase_frequency.isin(["every season", "once every year"]),
                              "buys seed every year", "buys seed less often")
dat.loc[dat.seed_purchase_frequency.isna(), "seed_buying"] = None
dat["cropping"] = np.where(dat.cropping_arrangement.fillna("").str.startswith("monocropping"), "sole crop", "intercrop or both")
dat["farm_size_class"] = dat.groupby("country").farm_size.transform(
    lambda x: pd.qcut(pd.to_numeric(x), 3, labels=["small", "middle", "large"], duplicates="drop").astype(object))
dat["ranking_ease_class"] = np.where(dat.ranking_ease.isin(["easy to rank", "very easy to rank"]), "found ranking easy",
                                     "neutral or difficult")
dat["gender"] = dat.farmer_gender
dat["presenter"] = dat.video_presenter_gender
dat["sub_area"] = dat.country + " " + dat.adm2
dat["point"] = dat.longitude.astype(str) + "," + dat.latitude.astype(str)
SEGS = ["gender", "share_sold", "decides_variety", "sells_role", "income", "seed_buying", "cropping",
        "farm_size_class", "ranking_ease_class", "presenter", "sub_area"]
print(dat.groupby("country")[SEGS[:-1]].agg(lambda x: x.value_counts(dropna=False).to_dict()).T.to_string())
print("distinct rounded points", dat.groupby("country").point.nunique().to_dict())

fit = individual_fit(rank, dat.country)
print("\npseudo R2 of single answers, pooled", round(fit["pseudo_r2_pooled"], 3), "| by country", round(fit["pseudo_r2_segments"], 3))

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
scopes = [("both countries", dat.index, ["country", "gender", "share_sold"])] + \
         [(c, dat.index[dat.country == c], SEGS) for c in ("Kenya", "Uganda")]
for scope, idx, segs in scopes:
    r, d = rank.loc[idx].reset_index(drop=True), dat.loc[idx].reset_index(drop=True)
    for sg in segs:
        t = segment_test(r, d[sg], B=B)
        cv = cv_gain(r, d[sg], blocks=d["sub_area"] if sg != "sub_area" else None)
        t.update(scope=scope, segmentation=sg, cv_gain=cv["gain_per_ranking"], cv_folds=cv["folds"])
        tests.append(t)
tests = pd.DataFrame(tests)
cols = ["scope", "segmentation", "n", "segments", "smallest", "p_deviance", "repeatability_pooled",
        "repeatability_segments", "repeatability_random", "random_lower", "random_upper", "p_repeatability", "cv_gain"]
print(tests[cols].round(3).to_string(index=False))
print("tests", len(tests), "| Bonferroni threshold at 0.05", round(0.05 / len(tests), 4),
      "| p_repeatability below it", int((tests.p_repeatability < 0.05 / len(tests)).sum()),
      "| p_deviance below it", int((tests.p_deviance < 0.05 / len(tests)).sum()))
tests.to_csv("output/maize-segment-tests.csv", index=False)

# 2b. household segments from several survey variables at once, built within each country
# without the rankings: household size, age, farm size, maize area, number of crops and of
# kinds of livestock, harvest, and the shares of the harvest sold and kept for the household
print("\n== household segments from the survey variables (k-means, within country)")
count = lambda c: dat[c].fillna("").map(lambda v: len([x for x in v.split("; ") if x]))
hh = pd.DataFrame({
    "household_size": num("household_size"), "age": num("age"), "farm_size": num("farm_size"),
    "crop_area": num("crop_area"), "n_crops": count("crops_grown"), "n_livestock": count("livestock_kept"),
    "harvest": num("farm_harvest_quantity"),
    "share_sold": ((num("grain_sold") + num("grain_stored_for_sale")) / num("farm_harvest_quantity").replace(0, np.nan)).clip(0, 1),
    "share_household": (num("grain_for_household") / num("farm_harvest_quantity").replace(0, np.nan)).clip(0, 1)})
seg_rows = []
for c in ("Kenya", "Uganda"):
    idx = dat.index[dat.country == c]
    r, d = rank.loc[idx].reset_index(drop=True), dat.loc[idx].reset_index(drop=True)
    for k in (2, 3, 4):
        lab, prof, stab = build_segments(hh.loc[idx].reset_index(drop=True), k, log=["farm_size", "crop_area", "harvest"])
        t = segment_test(r, lab.values, B=B)
        cv = cv_gain(r, lab.values, blocks=d["sub_area"])
        seg_rows.append(dict(scope=c, k=k, stability=stab, smallest=t["smallest"], p_deviance=t["p_deviance"],
                             repeatability_segments=t["repeatability_segments"], repeatability_random=t["repeatability_random"],
                             random_lower=t["random_lower"], random_upper=t["random_upper"],
                             p_repeatability=t["p_repeatability"], cv_gain=cv["gain_per_ranking"]))
        if k == 3:
            print(c, "profile of three segments"); print(prof.round(2).to_string())
seg_rows = pd.DataFrame(seg_rows)
print(seg_rows.round(3).to_string(index=False))
seg_rows.to_csv("output/maize-household-segments.csv", index=False)

# 3. equal n, descriptive
print("\n== repeatability at the same n (m = 150)")
e = equal_n(rank, dat.country, m=150, B=200)
print(e.round(3).to_string(index=False))
e.to_csv("output/maize-equal-n-country.csv", index=False)

# 3b. do results hold across places. Halves made of whole sub-areas, so the
# two halves share no village. All ten ways of splitting six sub-areas 3 + 3
print("\n== split-half between groups of sub-areas (n about 600 per half)")
lev = sorted(pd.unique(rank[["first", "second", "third"]].values.ravel()))
for c in ("Kenya", "Uganda"):
    d = dat[dat.country == c]
    areas = sorted(d.sub_area.unique())
    rs = []
    for half in combinations(areas, 3):
        if areas[0] not in half:
            continue
        i1, i2 = d.index[d.sub_area.isin(half)], d.index[~d.sub_area.isin(half)]
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
print(br[br.item.isin(["Drought escape", "Intercropping"])].round(2).to_string(index=False))
br.to_csv("output/maize-reliability-benchmark.csv", index=False)
w.to_csv("output/maize-worth.csv")

# points for a climate fetch with the environment-data skills, one row per rounded point
pts = dat.groupby(["country", "longitude", "latitude"]).size().reset_index(name="participants")
pts.insert(0, "id", [f"{c[:2].upper()}{i + 1:03d}" for i, c in enumerate(pts.country)])
pts.rename(columns={"latitude": "lat", "longitude": "lon"})[["id", "lat", "lon", "country", "participants"]].to_csv(
    "output/maize-concept-points.csv", index=False)
print("\npoints for climate", len(pts))
