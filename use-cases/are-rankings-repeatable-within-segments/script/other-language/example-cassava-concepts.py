# Second worked example: video-based concept testing of seven cassava product concepts with
# 398 farmers in the Caribbean region of Colombia (CIAT and CIRAD, 2024), from tricot-data.
# Run:  python3 script/other-language/example-cassava-concepts.py <tricot-data> [climate.csv] [permutations]
# The climate file holds daily id, date, precip_mm, tmax_c, tmin_c for the points written to
# output/cassava-concept-points.csv, as the chirps-rainfall and chirts-temperature skills give.
import json
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from repeatability import (repeatability_curve, segment_test, cv_gain, worth_table, individual_fit,
                           benchmark_reliability, _encode, worth_shares, czekanowski, group_repeatability)
from envirotypes import climate_indices, screen_indices, envirotypes

root = sys.argv[1]
clim_file = sys.argv[2] if len(sys.argv) > 2 and not sys.argv[2].isdigit() else None
B = int(sys.argv[-1]) if sys.argv[-1].isdigit() else 500
YEARS = (2007, 2021)  # the same years as the maize example
pd.set_option("display.width", 250, "display.max_columns", 40, "display.max_rows", 300)
os.makedirs("output", exist_ok=True)

idx = pd.read_csv(os.path.join(root, "datasets.csv"))
j = json.load(open(os.path.join(root, idx[idx.trial_id == "26ad22f40f6v"].path.iloc[0])))
s = pd.DataFrame(j["site_data"]).set_index("site_id")
p = pd.DataFrame(j["plot_data"])
p = p[(p.trait_original == "concept_preference") & (p.value_type == "rank")]
w = p.assign(rank=pd.to_numeric(p.value)).pivot(index="site_id", columns="rank", values="genotype_name")
w = w[w[[1, 2, 3]].notna().all(axis=1)]
dat = s.loc[w.index].reset_index()
rank = w[[1, 2, 3]].set_axis(["first", "second", "third"], axis=1).reset_index(drop=True)
print("farmers", len(s), "| complete rankings", len(rank), "| concepts", sorted(pd.unique(rank.values.ravel())))

pts = s.groupby(["longitude", "latitude"]).size().reset_index(name="participants")
pts.insert(0, "id", [f"CO{i + 1:03d}" for i in range(len(pts))])
pts.rename(columns={"longitude": "lon", "latitude": "lat"})[["id", "lat", "lon", "participants"]].assign(country="Colombia").to_csv("output/cassava-concept-points.csv", index=False)
dat = dat.merge(pts[["id", "longitude", "latitude"]], on=["longitude", "latitude"], how="left")
blocks = dat.adm2.fillna("unknown")

# 1. the unsegmented ranking
R, lev = _encode(rank)
print("\n== repeatability of the unsegmented ranking")
curve = repeatability_curve(rank, sizes=(50, 100, 200), B=300)
print(curve.round(3).to_string(index=False))
curve.to_csv("output/cassava-curve.csv", index=False)
print("single answers", individual_fit(rank))

# 2. segments defined without the rankings
num = lambda c: pd.to_numeric(dat[c], errors="coerce")
half = lambda x: pd.qcut(x, 2, labels=["lower half", "upper half"]).astype(object)
market = np.where(dat.crop_type == "sweet", "sweet only", "industrial or both")
seg = {"market type, sweet only or industrial": market,
       "department": dat.adm1.values,
       "gender": dat.farmer_gender.values,
       "age, 2 classes": half(num("age")).values,
       "experience with cassava, 2 classes": half(num("crop_experience")).values,
       "sweet cassava area in ha, 2 classes": half(num("sweet_cassava_area").where(dat.sweet_cassava_area_unit == "ha")).values}
print(pd.crosstab(dat.adm1, dat.crop_type).to_string())


def run(name, labels, r=rank, b=blocks, scope="all"):
    g = pd.Series(labels).reset_index(drop=True)
    ok = g.notna().values
    rr, gg = r[ok].reset_index(drop=True), g[ok].values
    t = segment_test(rr, gg, B=B)
    cv = cv_gain(rr, gg, blocks=None if name == "department" else b[ok].reset_index(drop=True))
    return dict(scope=scope, segment=name, n=t["n"], smallest=t["smallest"], p_deviance=t["p_deviance"],
                repeatability_pooled=t["repeatability_pooled"], repeatability_segments=t["repeatability_segments"],
                repeatability_random=t["repeatability_random"], random_lower=t["random_lower"], random_upper=t["random_upper"],
                p_repeatability=t["p_repeatability"], cv_gain=cv["gain_per_ranking"], folds=cv["folds"])


rows = [run(k, v) for k, v in seg.items()]
print("\n== worth by market type"); print(worth_table(rank, market).round(2).to_string())
worth_table(rank, market).to_csv("output/cassava-worth-market.csv")

# similarity of the two markets against a random split of the same sizes
a, b = np.where(market == "sweet only")[0], np.where(market != "sweet only")[0]
obs = czekanowski(worth_shares(R[a], len(lev)), worth_shares(R[b], len(lev)))
rng, ref = np.random.default_rng(1), []
for _ in range(B):
    q = rng.permutation(len(R))
    ref.append(czekanowski(worth_shares(R[q[:len(a)]], len(lev)), worth_shares(R[q[len(a):]], len(lev))))
print("\nCzekanowski similarity, sweet only against industrial or both", round(obs, 3), "| random split of the same sizes",
      round(float(np.mean(ref)), 3), np.quantile(ref, [0.025, 0.975]).round(3), "| p", float(np.mean(np.array(ref) <= obs)))

# 3. envirotypes
if clim_file:
    cl = pd.read_csv(clim_file, usecols=["id", "date", "precip_mm", "tmax_c", "tmin_c"])
    cl = cl[(cl.date.str[:4].astype(int) >= YEARS[0]) & (cl.date.str[:4].astype(int) <= YEARS[1])]
    print("\nclimate", cl.date.min(), "to", cl.date.max(), "| points", cl.id.nunique(), "| points without climate", len(set(pts.id) - set(cl.id)))
    ind = climate_indices(cl, cl)
    ind.to_csv("output/cassava-climate-indices.csv")
    keep, cvs, pairs = screen_indices(ind)
    print("indices", ind.drop(columns=["years_rain", "years_temp"]).agg(["min", "median", "max"]).round(1).T.to_string())
    print("kept", keep, "| coefficient of variation", cvs, "| strongly correlated", pairs)
    sweet = market == "sweet only"
    for k in (2, 3, 4):
        env, prof, stab, used = envirotypes(ind, k)
        e = dat.id.map(env).values
        r_ = run(f"envirotypes, {k}", e); r_["stability"] = stab; rows.append(r_)
        # climate apart from market: the same envirotypes among sweet growers only
        r_ = run(f"envirotypes, {k}", e[sweet], rank[sweet].reset_index(drop=True), blocks[sweet].reset_index(drop=True), "sweet only"); r_["stability"] = stab; rows.append(r_)
        if k == 2:
            dry = e == pd.Series(e)[~sweet].mode()[0]  # the envirotype that holds the industrial growers
            rows.append(run("market type, sweet only or industrial", market[dry], rank[dry].reset_index(drop=True), blocks[dry].reset_index(drop=True), "envirotype of the industrial growers"))
            print("\n== two envirotypes"); print(prof.round(1).to_string())
            print(pd.crosstab(e, dat.adm1).to_string()); print(pd.crosstab(e, market).to_string())
            print(worth_table(rank, e).round(2).to_string())
    rows.append(run("department", dat.adm1.values[sweet], rank[sweet].reset_index(drop=True), blocks[sweet].reset_index(drop=True), "sweet only"))

out = pd.DataFrame(rows)
print("\n== segment tests,", B, "permutations"); print(out.round(3).to_string(index=False))
out.to_csv("output/cassava-segment-tests.csv", index=False)
