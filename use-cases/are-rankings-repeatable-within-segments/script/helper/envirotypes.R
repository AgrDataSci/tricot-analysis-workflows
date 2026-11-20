# Envirotypes for the use case "Are rankings repeatable within segments?"
# Status: reviewed-not-executed. Translated from script/other-language/envirotypes.py,
#   which was run on simulated climate. Run tests/run-tests.R before use.
# Decision records: decisions/repeatability.md and decisions/climate-environment.md
#
# Scientific intent
#   Describe the climate of each location from daily rainfall and temperature,
#   group the locations into envirotypes without looking at the rankings, and
#   hand the groups to segment_test().
#
# Why annual indices
#   The climate record links a plot to the weather between its planting date
#   and a crop stage. A concept or consumer test has no plot and no planting
#   date. What can shape a stated preference is the climate of the place, so
#   the indices here are annual and averaged over many years. For a variety
#   trial use the seasonal indices of the use case
#   how-does-climate-shape-performance, computed with climatrends.
#
# Input
#   The files written by the chirps-rainfall and chirts-temperature skills of
#   AgrDataSci/environment-data. Rain has id, date, precip_mm. Temperature has
#   id, date, tmax_c, tmin_c. One row per point and day.
#
# Limits
#   - The thresholds are choices. State them, and the years, with any result.
#   - Points a few kilometres apart share a grid cell and so share a climate.
#   - Envirotypes from long-term climate say nothing on the season of a trial.


#' Annual climate indices per point, averaged over the years of the files
#'
#' rain_total rainfall per year (mm), rain_days days with at least wet_day mm,
#' heavy_rain_days days with at least heavy_rain mm (R20mm), max_dry_spell
#' longest run of days below wet_day mm (MLDS), rain_seasonality coefficient
#' of variation of the mean monthly rainfall, tmax_mean, tmin_mean and
#' dtr_mean in degrees C, hot_days days with Tmax at or above hot_day,
#' warm_nights days with Tmin at or above warm_night.
#'
#' @param rain data.frame with id, date, precip_mm
#' @param temp data.frame with id, date, tmax_c, tmin_c, or NULL
#' @param wet_day,heavy_rain numeric, mm
#' @param hot_day,warm_night numeric, degrees C
#' @param min_days integer, a year with fewer valid days is left out
#' @return data.frame, one row per id, with years_rain and years_temp
climate_indices = function(rain, temp = NULL, wet_day = 1, heavy_rain = 20,
                           hot_day = 30, warm_night = 20, min_days = 350) {

  max_run = function(x) {
    r = rle(x)
    if (any(r$values)) max(r$lengths[r$values]) else 0
  }

  r = rain[!is.na(rain$precip_mm), c("id", "date", "precip_mm")]
  r$date = as.Date(r$date)
  r = r[order(r$id, r$date), ]
  r$year = as.integer(format(r$date, "%Y"))
  r$month = as.integer(format(r$date, "%m"))
  r$key = paste(r$id, r$year)
  r = r[r$key %in% names(which(table(r$key) >= min_days)), ]

  by_year = lapply(split(r, r$key), function(d) {
    data.frame(id = d$id[1],
               rain_total = sum(d$precip_mm),
               rain_days = sum(d$precip_mm >= wet_day),
               heavy_rain_days = sum(d$precip_mm >= heavy_rain),
               max_dry_spell = max_run(d$precip_mm < wet_day),
               stringsAsFactors = FALSE)
  })
  by_year = do.call(rbind, by_year)

  out = stats::aggregate(. ~ id, by_year, mean)
  out$years_rain = as.vector(table(by_year$id)[as.character(out$id)])

  monthly = stats::aggregate(precip_mm ~ id + year + month, r, sum)
  monthly = stats::aggregate(precip_mm ~ id + month, monthly, mean)
  pop_sd = function(x) sqrt(mean((x - mean(x))^2))
  seas = tapply(monthly$precip_mm, monthly$id, function(x) pop_sd(x) / mean(x))
  out$rain_seasonality = as.vector(seas[as.character(out$id)])

  if (!is.null(temp)) {

    t = temp[!is.na(temp$tmax_c) & !is.na(temp$tmin_c), c("id", "date", "tmax_c", "tmin_c")]
    t$year = as.integer(format(as.Date(t$date), "%Y"))
    t$key = paste(t$id, t$year)
    t = t[t$key %in% names(which(table(t$key) >= min_days)), ]

    ty = lapply(split(t, t$key), function(d) {
      data.frame(id = d$id[1],
                 tmax_mean = mean(d$tmax_c),
                 tmin_mean = mean(d$tmin_c),
                 dtr_mean = mean(d$tmax_c - d$tmin_c),
                 hot_days = sum(d$tmax_c >= hot_day),
                 warm_nights = sum(d$tmin_c >= warm_night),
                 stringsAsFactors = FALSE)
    })
    ty = do.call(rbind, ty)

    tm = stats::aggregate(. ~ id, ty, mean)
    tm$years_temp = as.vector(table(ty$id)[as.character(tm$id)])
    out = merge(out, tm, by = "id", all = TRUE)

  }

  rownames(out) = NULL

  out

}


index_columns = c("rain_total", "rain_days", "heavy_rain_days", "max_dry_spell",
                  "rain_seasonality", "tmax_mean", "tmin_mean", "dtr_mean",
                  "hot_days", "warm_nights")


#' Indices that vary across the points, and the strongly correlated pairs
#'
#' An index that hardly varies cannot define an envirotype, so it is dropped
#' (near-zero variance, as the climate record asks). Correlated indices are
#' reported, not removed.
#'
#' @param indices data.frame from climate_indices()
#' @param min_cv numeric, smallest coefficient of variation to keep an index
#' @return list with keep, cv and pairs (data.frame of pairs with |r| >= 0.9)
screen_indices = function(indices, min_cv = 0.01) {

  x = indices[, intersect(index_columns, names(indices)), drop = FALSE]
  pop_sd = function(v) sqrt(mean((v - mean(v, na.rm = TRUE))^2, na.rm = TRUE))
  cv = vapply(x, function(v) {
    m = abs(mean(v, na.rm = TRUE))
    if (m == 0) 0 else pop_sd(v) / m
  }, numeric(1))

  keep = names(cv)[cv >= min_cv]

  r = stats::cor(x[, keep, drop = FALSE], use = "pairwise.complete.obs")
  w = which(abs(r) >= 0.9 & upper.tri(r), arr.ind = TRUE)
  pairs = data.frame(a = keep[w[, 1]], b = keep[w[, 2]], r = round(r[w], 2),
                     stringsAsFactors = FALSE)

  list(keep = keep, cv = round(cv, 3), pairs = pairs)

}


#' Group the points into k envirotypes from their climate indices alone
#'
#' @param indices data.frame from climate_indices()
#' @param k integer, number of envirotypes
#' @param seed integer
#' @return list with envirotype (named character, one per id), profile,
#'   stability (see build_segments()) and the indices used
envirotypes = function(indices, k, seed = 1) {

  keep = screen_indices(indices)$keep
  seg = build_segments(indices[, keep, drop = FALSE], k, seed = seed)

  env = paste("envirotype", seg$segment)
  names(env) = indices$id

  list(envirotype = env, profile = seg$profile, stability = seg$stability, used = keep)

}


#' Simulate daily rainfall and temperature for points in known zones, for tests
#'
#' @param points character, point ids
#' @param zone character, "wet" or "dry" per point. Wet points get more rain
#'   days, cooler days and cooler nights
#' @param years integer vector
#' @param seed integer
#' @return list with rain and temp, with the columns of the environment-data skills
simulate_climate = function(points, zone, years = 2007:2011, seed = 1) {

  set.seed(seed)

  days = seq(as.Date(paste0(min(years), "-01-01")), as.Date(paste0(max(years), "-12-31")), by = "day")
  doy = as.integer(format(days, "%j"))
  season = 0.5 + 0.5 * sin(2 * pi * (doy - 80) / 365)
  n = length(days)

  sim = lapply(seq_along(points), function(i) {
    p = if (zone[i] == "wet") c(0.45, 9, 26, 15) else c(0.22, 6, 31, 19)
    wet = stats::runif(n) < p[1] * (0.4 + season)
    mm = ifelse(wet, stats::rgamma(n, shape = 0.8, scale = p[2] / 0.8), 0)
    tx = p[3] + 2 * cos(2 * pi * (doy - 60) / 365) + stats::rnorm(n, 0, 1.2) + stats::rnorm(1, 0, 0.3)
    tn = p[4] + 1.5 * cos(2 * pi * (doy - 60) / 365) + stats::rnorm(n, 0, 1) + stats::rnorm(1, 0, 0.3)
    list(rain = data.frame(id = points[i], date = days, precip_mm = mm, stringsAsFactors = FALSE),
         temp = data.frame(id = points[i], date = days, tmax_c = tx, tmin_c = tn, stringsAsFactors = FALSE))
  })

  list(rain = do.call(rbind, lapply(sim, `[[`, "rain")),
       temp = do.call(rbind, lapply(sim, `[[`, "temp")))

}
