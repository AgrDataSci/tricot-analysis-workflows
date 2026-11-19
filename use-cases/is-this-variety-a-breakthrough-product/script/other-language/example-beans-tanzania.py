# Worked example on open data. CIAT common bean tricot trials in Tanzania that
# started in 2021 or 2022, from tricot-data (https://doi.org/10.5281/zenodo.17112491).
# On-farm data only. See "Worked example" in the README for what that limits.
# Run:  python3 script/other-language/example-beans-tanzania.py <path to a clone of tricot-data>
import json
import os
import sys
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from breakthrough import breakthrough_table, preference_reliability, trial_contrasts

root = sys.argv[1]
pd.set_option("display.width", 250, "display.max_columns", 40, "display.max_rows", 200)
idx = pd.read_csv(os.path.join(root, "datasets.csv"))
sel = idx[(idx.country_iso3 == "TZA") & (idx.crop_name == "commonbean") &
          idx.data_producer.str.contains("CIAT") & idx.start_date.str[:4].isin(["2021", "2022"])]
Y, O = [], []
for _, r in sel.iterrows():
    p = pd.DataFrame(json.load(open(os.path.join(root, r.path)))["plot_data"])
    p = p.reindex(columns=list(p.columns) + [c for c in ("value_standard", "flag") if c not in p.columns])
    p["trial"], p["year"], p["farm"] = r.trial_id, int(r.start_date[:4]), p.site_id
    y = p[(p.trait == "yield") & (p.value_type == "numeric")].copy()
    y["yield"] = pd.to_numeric(y.value_standard, errors="coerce")
    Y.append(y[["trial", "year", "farm", "genotype_name", "yield", "flag"]])
    o = p[(p.trait == "overall_performance") & (p.value_type == "rank") & (p.collection_moment == "Post-harvest")].copy()
    o["rank"] = pd.to_numeric(o.value, errors="coerce")
    O.append(o[["trial", "year", "farm", "genotype_name", "rank"]])
Y = pd.concat(Y).rename(columns={"genotype_name": "genotype"}).dropna(subset=["yield"])
O = pd.concat(O).rename(columns={"genotype_name": "genotype"})
print("trials selected", len(sel), "| with yield", Y.trial.nunique(), "| with post-harvest overall ranking", O.trial.nunique())
print("yields resting on an assumed plot area", int(Y.flag.fillna("").str.contains("plot_area_assumed").sum()), "of", len(Y))

cols = ["genotype", "gain_ratio", "lower", "upper", "p_mean_gain", "rel", "rel_threshold", "rel_threshold_lower",
        "rel_threshold_upper", "n_plots", "n_trials", "n_pairs", "wins", "wins_threshold", "share_failed", "verdict"]
os.makedirs("output", exist_ok=True)
for check in ("Uyole 03", "CALIMA UYOLE"):
    for label, yrs in (("2021", [2021]), ("2022", [2022]), ("2021-2022", [2021, 2022])):
        d = Y[Y.year.isin(yrs)]
        t = breakthrough_table(d, check, nboot=200)
        pr = preference_reliability(O[O.year.isin(yrs)], check)
        t = t.merge(pr, on="genotype", how="left")
        skipped = trial_contrasts(d, check).attrs["skipped"]
        print(f"\n===== check {check} | trials started {label} =====")
        print("plots", len(d), "| zero yield", int((d["yield"] <= 0).sum()),
              "| trials used", d.trial.nunique() - len(skipped), "| skipped", skipped)
        print(t[cols + ["p_preferred", "n_rankings"]].round(2).to_string(index=False))
        t.insert(0, "check", check), t.insert(1, "trials_started", label)
        t.to_csv(f"output/beans-tanzania-{check.replace(' ', '-').lower()}-{label}.csv", index=False)
