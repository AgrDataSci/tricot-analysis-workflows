# Functions for the use case "How do climate and environment shape variety performance?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/climate-environment.md
#
# Scientific intent
#   Relate variety preference to the climate each plot experienced during the
#   crop stage that matters, and say where the results can and cannot be
#   applied.
#
# Approach
#   1. Define the climate window of each plot from its planting date and the
#      crop phenology. The default ends the window at flowering, estimated as
#      the day the accumulated growing degree days reach a documented
#      threshold (climatrends::GDD).
#   2. Compute temperature and rainfall indices over that window
#      (climatrends::temperature, climatrends::rainfall).
#   3. Fit a Plackett-Luce tree (PlackettLuce::pltree) with the indices as
#      partitioning variables.
#   4. Refit with alternative windows to show how sensitive the conclusion is
#      to the window definition.
#   5. Before applying the results to new sites, flag sites whose climate lies
#      outside the conditions observed in the trial.
#
# Assumptions
#   - Phenology parameters (base temperature, degree days to the stage) come
#     from a documented source for this crop and these varieties. They are
#     required arguments, there are no defaults, because a guessed window
#     can create or hide an effect.
#   - Daily climate data are available for the full window of every plot.
#   - Planting dates are recorded per plot.
#
# Alternatives
#   - Fixed windows (for example 60 days after planting) when phenology is
#     unknown, reported as a sensitivity analysis.
#   - Plackett-Luce models with continuous covariates on worth, when a smooth
#     response is expected rather than thresholds.
#   - Environmental similarity (analogues) to match trial sites to target
#     sites, instead of predicting from a model.
#
# Limits
#   - A tree split shows that preference differs between climate conditions.
#     It is not proof that climate causes the difference, since climate is
#     confounded with soils, management and who the farmers are.
#   - Predictions outside the observed climate range are extrapolation and
#     must not be used as recommendations.


#' Climate window per plot from planting date and phenology
#'
#' @param temp array [plot, day, 2] of daily maximum and minimum temperature,
#'   with dates (YYYY-MM-DD) as the names of the second dimension
#' @param planting Date vector, one per plot
#' @param tbase numeric, base temperature of the crop
#' @param degree_days numeric, degree days from planting to the target stage
#' @param max_days integer, the longest window to search
#' @return integer vector with the number of days from planting to the stage,
#'   NA where the stage is not reached within max_days
phenology_window = function(temp, planting, tbase, degree_days, max_days = 150) {

  if (missing(tbase) || missing(degree_days)) {
    stop("phenology is required, provide 'tbase' and 'degree_days' from a ",
         "documented source for this crop and these varieties", call. = FALSE)
  }

  ndays = climatrends::GDD(temp,
                           day.one = planting,
                           span = max_days,
                           degree.days = degree_days,
                           tbase = tbase,
                           return.as = "ndays")$gdd

  # GDD() returns the full span when the threshold is not reached,
  # which would silently produce a window of max_days
  not_reached = ndays >= max_days
  if (any(not_reached)) {
    warning(sum(not_reached), " plots did not reach ", degree_days,
            " degree days within ", max_days, " days, their window is set to NA",
            call. = FALSE)
    ndays[not_reached] = NA
  }

  ndays

}


#' Temperature and rainfall indices over a window per plot
#'
#' @param temp array of temperature as in phenology_window
#' @param rain matrix [plot, day] of daily rainfall with dates as column names
#' @param planting Date vector
#' @param span integer vector of window lengths, one per plot
#' @return data.frame with maxDT, DTR, SU (days above 30 C) and Rtotal per plot
climate_indices = function(temp, rain, planting, span) {

  ok = !is.na(span)

  out = data.frame(maxDT = NA_real_, DTR = NA_real_, SU = NA_real_,
                   Rtotal = NA_real_)[rep(1, length(span)), ]
  rownames(out) = NULL

  if (!any(ok)) return(out)

  tt = climatrends::temperature(temp[ok, , , drop = FALSE],
                                day.one = planting[ok],
                                span = span[ok])
  rr = climatrends::rainfall(rain[ok, , drop = FALSE],
                             day.one = planting[ok],
                             span = span[ok])

  out$maxDT[ok] = tt$maxDT
  out$DTR[ok] = tt$DTR
  out$SU[ok] = tt$SU
  out$Rtotal[ok] = rr$Rtotal

  out

}


#' Fit a Plackett-Luce tree on climate indices
#'
#' @param R rankings object
#' @param covariates data.frame of partitioning variables, one row per ranking
#' @param alpha significance level for splitting
#' @param minsize minimum number of rankings per node
#' @details PlackettLuce must be attached with library("PlackettLuce"),
#'   because pltree() looks up its fitting functions on the search path
fit_climate_tree = function(R, covariates, alpha = 0.01, minsize = 100) {

  G = PlackettLuce::group(R, seq_len(nrow(R)))

  d = cbind(G, covariates)

  f = stats::as.formula(paste("G ~", paste(names(covariates), collapse = " + ")))

  # pltree() re-evaluates its control arguments by name, so pass the values
  # themselves through do.call() rather than variables of this function
  do.call(PlackettLuce::pltree,
          list(formula = f, data = d, alpha = alpha, minsize = minsize))

}


#' Variables used in the splits of a tree, in order of the nodes
split_variables = function(tree) {

  ids = partykit::nodeids(tree)
  inner = ids[!ids %in% partykit::nodeids(tree, terminal = TRUE)]

  if (length(inner) == 0) return(character(0))

  vars = names(tree$data)

  unlist(partykit::nodeapply(tree, ids = inner, function(n) {
    vars[partykit::split_node(n)$varid]
  }))

}


#' Sensitivity of the conclusion to the climate window
#'
#' @param R rankings
#' @param windows named list of window lengths, one integer vector per window
#' @param temp, rain, planting as in climate_indices
#' @return data.frame with one row per window, the first split variable, the
#'   number of terminal nodes and the log-likelihood
window_sensitivity = function(R, windows, temp, rain, planting, ...) {

  do.call("rbind", lapply(names(windows), function(w) {
    ci = climate_indices(temp, rain, planting, windows[[w]])
    keep = stats::complete.cases(ci)
    tree = fit_climate_tree(R[keep, ], ci[keep, ], ...)
    sv = split_variables(tree)
    data.frame(window = w,
               plots = sum(keep),
               first_split = if (length(sv)) sv[1] else "none",
               terminal_nodes = length(partykit::nodeids(tree, terminal = TRUE)),
               loglik = as.numeric(stats::logLik(tree)))
  }))

}


#' Flag target sites outside the climate observed in the trial
#'
#' Two checks. A site is outside the range when any index is below the
#' minimum or above the maximum seen in the trial. A site is unusual when its
#' Mahalanobis distance to the trial conditions exceeds the chosen quantile of
#' the chi-squared distribution, which catches combinations never observed
#' even when each index is within range.
#'
#' @param train data.frame of indices in the trial
#' @param target data.frame of the same indices at target sites
#' @param quantile numeric, chi-squared quantile for the distance check
extrapolation_check = function(train, target, quantile = 0.99) {

  train = train[stats::complete.cases(train), , drop = FALSE]
  target = target[, names(train), drop = FALSE]

  lo = vapply(train, min, numeric(1))
  hi = vapply(train, max, numeric(1))

  out_range = apply(target, 1, function(x) any(x < lo | x > hi))

  md = stats::mahalanobis(target, colMeans(train), stats::cov(train))
  unusual = md > stats::qchisq(quantile, df = ncol(train))

  data.frame(target, outside_range = out_range,
             mahalanobis = md, unusual = unusual,
             extrapolation = out_range | unusual,
             row.names = NULL)

}
