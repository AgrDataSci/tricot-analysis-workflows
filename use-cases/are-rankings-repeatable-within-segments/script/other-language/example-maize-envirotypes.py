# Envirotype test on the maize concept tests of Kenya and Uganda.
# Needs daily rainfall and temperature for the interview points, fetched with the
# chirps-rainfall and chirts-temperature skills of AgrDataSci/environment-data for
# the points in output/maize-concept-points.csv (written by example-maize-concepts.py).
# Run:  python3 script/other-language/example-maize-envirotypes.py <tricot-data> <rain.csv[.gz]> <temp.csv[.gz]> [permutations]
import json
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from repeatability import (segment_test, cv_gain, worth_table, _encode, worth_shares, czekanowski,
                           cross_dataset_similarity)
from envirotypes import climate_indices, screen_indices, envirotypes

root, rain_file, temp_file = sys.argv[1:4]
B = int(sys.argv[4]) if len(sys.argv) > 4 else 500
pd.set_option("display.width", 250, "display.max_columns", 40, "display.max_rows", 300)
os.makedirs("output", exist_ok=True)

idx = pd.read_csv(os.path.join(root, "datasets.csv"))
sites, ranks = [], []
for tid in ("6834d470d4cv", "862ed6d6adfv"):
    j = json.load(open(os.path.join(root, idx[idx.trial_id == tid].path.iloc[0])))
    s = pd.DataFrame(j["site_data"]).set_index("site_id")
    p = pd.DataFrame(j["plot_data"])
    p = p[(p.trait_original == "concept_preference") & (p.value_type == "rank")]
    w = p.assign(rank=pd.to_numeric(p.value)).pivot(index="site_id", columns="rank", values="genotype_name")
    w = w[w[[1, 2, 3]].notna().all(axis=1)]
    sites.append(s.loc[w.index].reset_index())
    ranks.append(w[[1, 2, 3]].set_axis(["first", "second", "third"], axis=1).reset_index(drop=True))
dat, rank = pd.concat(sites, ignore_index=True), pd.concat(ranks, ignore_index=True)
dat["sub_area"] = dat.country + " " + dat.adm2

# the same point ids as output/maize-concept-points.csv
pts = dat.groupby(["country", "longitude", "latitude"]).size().reset_index(name="participants")
pts.insert(0, "id", [f"{c[:2].upper()}{i + 1:03d}" for i, c in enumerate(pts.country)])
dat = dat.merge(pts[["id", "country", "longitude", "latitude"]], on=["country", "longitude", "latitude"], how="left")

rain, temp = pd.read_csv(rain_file), pd.read_csv(temp_file)
print("rain", rain.date.min(), "to", rain.date.max(), "| points", rain.id.nunique(), "| missing days", int(rain.precip_mm.isna().sum()))
print("temp", temp.date.min(), "to", temp.date.max(), "| points", temp.id.nunique(), "| missing days", int(temp[["tmax_c", "tmin_c"]].isna().any(axis=1).sum()))
missing = sorted(set(pts.id) - set(rain.id))
print("points without rainfall", len(missing), missing[:10])

ind = climate_indices(rain, temp)
ind.to_csv("output/maize-climate-indices.csv")
print("\n== climate indices by country")
print(ind.join(pts.set_index("id").country).groupby("country").agg(["min", "median", "max"]).round(1).T.to_string())

rows = []
for scope in ("Kenya", "Uganda", "both countries"):
    sel = dat.index if scope == "both countries" else dat.index[dat.country == scope]
    d, r = dat.loc[sel].reset_index(drop=True), rank.loc[sel].reset_index(drop=True)
    sub = ind.loc[[i for i in d.id.unique() if i in ind.index]]
    keep, cv, pairs = screen_indices(sub)
    print(f"\n== {scope}: {len(sub)} points | indices kept {keep}")
    print("coefficient of variation", cv, "| strongly correlated pairs", pairs)
    for k in (2, 3, 4):
        env, prof, stab, used = envirotypes(sub, k)
        seg = d.id.map(env)
        t = segment_test(r, seg.values, B=B)
        cv_ = cv_gain(r, seg.values, blocks=d.sub_area)
        rows.append(dict(scope=scope, k=k, stability=stab, smallest=t["smallest"], p_deviance=t["p_deviance"],
                         repeatability_pooled=t["repeatability_pooled"], repeatability_segments=t["repeatability_segments"],
                         repeatability_random=t["repeatability_random"], random_lower=t["random_lower"],
                         random_upper=t["random_upper"], p_repeatability=t["p_repeatability"], cv_gain=cv_["gain_per_ranking"]))
        if k == 2:
            print(prof.round(1).to_string())
            print(worth_table(r, seg.values).round(2).to_string())
out = pd.DataFrame(rows)
print("\n== envirotype tests,", B, "permutations")
print(out.round(3).to_string(index=False))
out.to_csv("output/maize-envirotype-tests.csv", index=False)

# what the envirotypes coincide with
print("\n== envirotypes against country and district")
env2 = envirotypes(ind, 2)[0]
print(pd.crosstab(dat.id.map(env2), dat.country).to_string())
for c in ("Kenya", "Uganda"):
    d = dat[dat.country == c]
    e = envirotypes(ind.loc[d.id.unique()], 2)[0]
    print(pd.crosstab(d.id.map(e), d.adm1).to_string())

# Czekanowski similarity between the two countries against its noise floor
print("\n== Czekanowski similarity between Kenya and Uganda")
R, lev = _encode(rank)
ke, ug = np.where(dat.country == "Kenya")[0], np.where(dat.country == "Uganda")[0]
print("all 1,200 against all 1,200:", round(czekanowski(worth_shares(R[ke], len(lev)), worth_shares(R[ug], len(lev))), 3))
cz = cross_dataset_similarity(rank, dat.country.values, sizes=(50, 100, 200, 400, 600))
print(cz.round(3).to_string(index=False))
cz.to_csv("output/maize-czekanowski.csv", index=False)

# country or climate: Kenyan farmers whose climate groups with Uganda
kk = np.where((dat.country == "Kenya") & (dat.id.map(env2) == env2[dat.id[ug[0]]]))[0]
if 50 <= len(kk) <= len(ke) - 50:
    sh = lambda i: worth_shares(R[i], len(lev))
    obs = czekanowski(sh(kk), sh(ug)), czekanowski(sh(kk), sh(np.setdiff1d(ke, kk)))
    rng, ref = np.random.default_rng(3), []
    for _ in range(B):
        a = rng.permutation(ke)
        ref.append((czekanowski(sh(a[:len(kk)]), sh(ug)), czekanowski(sh(a[:len(kk)]), sh(a[len(kk):]))))
    ref = np.array(ref)
    print(f"\n== {len(kk)} Kenyan farmers in the climate group of Uganda")
    print("similarity to Uganda", round(obs[0], 3), "| random Kenyan groups of that size", ref[:, 0].mean().round(3), np.quantile(ref[:, 0], [0.025, 0.975]).round(3), "| p", float(np.mean(ref[:, 0] >= obs[0])))
    print("similarity to the other Kenyans", round(obs[1], 3), "| random", ref[:, 1].mean().round(3), np.quantile(ref[:, 1], [0.025, 0.975]).round(3), "| p", float(np.mean(ref[:, 1] <= obs[1])))
