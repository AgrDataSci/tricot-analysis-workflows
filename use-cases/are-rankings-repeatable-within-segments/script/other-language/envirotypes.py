# Envirotypes for the use case "Are rankings repeatable within segments?"
# Decision records: decisions/repeatability.md and decisions/climate-environment.md
#
# Scientific intent
#   Describe the climate of each location from daily rainfall and temperature,
#   group the locations into envirotypes without looking at the rankings, and
#   hand the groups to segment_test().
# Why annual indices
#   The climate record links a plot to the weather between its planting date
#   and a crop stage. A concept or consumer test has no plot and no planting
#   date. What can shape a stated preference is the climate of the place, so
#   the indices here are annual and averaged over many years. For a variety
#   trial use the seasonal indices of the use case how-does-climate-shape-
#   performance instead.
# Input
#   The files written by the chirps-rainfall and chirts-temperature skills of
#   AgrDataSci/environment-data: id, date, precip_mm and id, date, tmax_c,
#   tmin_c, one row per point and day.
import numpy as np
import pandas as pd

from repeatability import build_segments


def _max_run(x):
    """Longest run of True in a boolean array."""
    best = run = 0
    for v in x:
        run = run + 1 if v else 0
        best = max(best, run)
    return best


def climate_indices(rain, temp=None, wet_day=1.0, heavy_rain=20.0, hot_day=30.0, warm_night=20.0, min_days=350):
    """Annual climate indices per point, averaged over the years of the files.

    rain_total          rainfall per year, mm
    rain_days           days per year with at least wet_day mm
    heavy_rain_days     days per year with at least heavy_rain mm (R20mm)
    max_dry_spell       longest run of days below wet_day mm in a year (MLDS)
    rain_seasonality    coefficient of variation of the mean monthly rainfall
    tmax_mean, tmin_mean, dtr_mean   mean day and night temperature and their range, degrees C
    hot_days            days per year with Tmax at or above hot_day
    warm_nights         days per year with Tmin at or above warm_night

    A year with fewer than min_days valid days is left out for that point and
    variable. years_rain and years_temp say how many years each point rests on.
    The thresholds are arguments. State them, and the years, with any result.
    """
    r = rain[["id", "date", "precip_mm"]].copy()
    r["date"] = pd.to_datetime(r["date"])
    r = r.dropna(subset=["precip_mm"]).sort_values(["id", "date"])
    r["year"], r["month"] = r.date.dt.year, r.date.dt.month
    full = r.groupby(["id", "year"]).precip_mm.transform("size") >= min_days
    r = r[full]
    g = r.groupby(["id", "year"])
    yr = pd.DataFrame({
        "rain_total": g.precip_mm.sum(),
        "rain_days": g.precip_mm.apply(lambda x: int((x >= wet_day).sum())),
        "heavy_rain_days": g.precip_mm.apply(lambda x: int((x >= heavy_rain).sum())),
        "max_dry_spell": g.precip_mm.apply(lambda x: _max_run((x < wet_day).values)),
    })
    out = yr.groupby("id").mean()
    out["years_rain"] = yr.groupby("id").size()
    monthly = r.groupby(["id", "year", "month"]).precip_mm.sum().groupby(["id", "month"]).mean()
    out["rain_seasonality"] = monthly.groupby("id").std(ddof=0) / monthly.groupby("id").mean()
    if temp is not None:
        t = temp[["id", "date", "tmax_c", "tmin_c"]].copy()
        t["date"] = pd.to_datetime(t["date"])
        t = t.dropna(subset=["tmax_c", "tmin_c"])
        t["year"] = t.date.dt.year
        t = t[t.groupby(["id", "year"]).tmax_c.transform("size") >= min_days]
        g = t.groupby(["id", "year"])
        ty = pd.DataFrame({
            "tmax_mean": g.tmax_c.mean(), "tmin_mean": g.tmin_c.mean(),
            "dtr_mean": g.apply(lambda x: float((x.tmax_c - x.tmin_c).mean())),
            "hot_days": g.tmax_c.apply(lambda x: int((x >= hot_day).sum())),
            "warm_nights": g.tmin_c.apply(lambda x: int((x >= warm_night).sum())),
        })
        out = out.join(ty.groupby("id").mean(), how="outer")
        out["years_temp"] = ty.groupby("id").size()
    return out


INDEX_COLUMNS = ["rain_total", "rain_days", "heavy_rain_days", "max_dry_spell", "rain_seasonality",
                 "tmax_mean", "tmin_mean", "dtr_mean", "hot_days", "warm_nights"]


def screen_indices(indices, min_cv=0.01):
    """Indices that vary across the points, and the pairs that are strongly correlated.

    An index that hardly varies cannot define an envirotype, so it is dropped
    (near-zero variance, as the climate record asks). Correlated indices are
    reported, not removed.
    """
    X = indices[[c for c in INDEX_COLUMNS if c in indices.columns]]
    cv = (X.std(ddof=0) / X.mean().abs().replace(0, np.nan)).fillna(0)
    keep = [c for c in X.columns if cv[c] >= min_cv]
    cor = X[keep].corr()
    pairs = [(a, b, round(float(cor.loc[a, b]), 2)) for i, a in enumerate(keep) for b in keep[i + 1:] if abs(cor.loc[a, b]) >= 0.9]
    return keep, cv.round(3).to_dict(), pairs


def envirotypes(indices, k, seed=1):
    """Group the points into k envirotypes from their climate indices alone.

    Returns the label of each point, the mean indices of each envirotype, the
    stability of the partition (see build_segments) and the indices used.
    """
    keep, _, _ = screen_indices(indices)
    labels, profile, stability = build_segments(indices[keep], k, seed=seed)
    return labels.map(lambda x: f"envirotype {x}"), profile, stability, keep


def simulate_climate(points, zone, years=range(2007, 2012), seed=1):
    """Daily rainfall and temperature for points in known climate zones, for tests.

    zone is a label per point, "wet" or "dry". Wet points get more rain days,
    cooler days and cooler nights. The output has the columns of the
    environment-data skills.
    """
    rng = np.random.default_rng(seed)
    days = pd.date_range(f"{min(years)}-01-01", f"{max(years)}-12-31", freq="D")
    doy = days.dayofyear.values
    season = 0.5 + 0.5 * np.sin(2 * np.pi * (doy - 80) / 365)  # one rainy season
    rain, temp = [], []
    for pid, z in zip(points, zone):
        p_wet, amount, tmax, tmin = (0.45, 9.0, 26.0, 15.0) if z == "wet" else (0.22, 6.0, 31.0, 19.0)
        wet = rng.uniform(size=len(days)) < p_wet * (0.4 + season)
        mm = np.where(wet, rng.gamma(0.8, amount / 0.8, len(days)), 0.0)
        tx = tmax + 2 * np.cos(2 * np.pi * (doy - 60) / 365) + rng.normal(0, 1.2, len(days)) + rng.normal(0, 0.3)
        tn = tmin + 1.5 * np.cos(2 * np.pi * (doy - 60) / 365) + rng.normal(0, 1.0, len(days)) + rng.normal(0, 0.3)
        rain.append(pd.DataFrame({"id": pid, "date": days.date, "precip_mm": mm}))
        temp.append(pd.DataFrame({"id": pid, "date": days.date, "tmax_c": tx, "tmin_c": tn}))
    return pd.concat(rain, ignore_index=True), pd.concat(temp, ignore_index=True)
