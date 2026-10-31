# Functions for the use case "Which product concept do consumers prefer?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/consumer-concept-testing.md
#
# Scientific intent
#   Product concepts (or food products) are described by attributes, such
#   as price, cooking time, packaging or nutrition. Consumers rank three
#   concepts each. The aim is to learn which attributes drive preference, by
#   how much, and whether consumer groups weigh them differently, to inform
#   product development.
#
# Approach
#   1. Build tricot rankings of the concepts (gosset::rank_tricot).
#   2. Estimate a worth per concept with a Plackett-Luce model, as a
#      description that makes no assumption about attributes.
#   3. Model log-worth as a linear function of the attributes
#      (PlackettLuce::pladmm), giving one coefficient per attribute.
#   4. Test whether the attributes explain the concept worths adequately,
#      comparing the attribute model with the free Plackett-Luce model.
#   5. Fit the attribute model by consumer group and test whether the
#      coefficients differ.
#   6. Predict the relative preference of untested attribute combinations,
#      only within the levels that were tested.
#
# Assumptions
#   - The attribute table describes every concept, with one row per concept,
#     and it is matched to the rankings by concept name, not by position.
#   - Attribute effects add up on the log-worth scale. Interactions must be
#     added explicitly to the formula.
#   - The task design is comparable to tricot, single-stage ranking of three
#     concepts with few attributes evaluated together.
#
# Alternatives
#   - Plackett-Luce trees with the attribute model in the nodes
#     (PlackettLuce::pltree with worth ~ attributes), to find consumer groups
#     from covariates rather than defining them in advance.
#   - Choice-based conjoint designs and mixed logit models, when the
#     attributes are designed orthogonally and many choice tasks are run.
#
# Limits
#   - Relative preference among the concepts tested is not adoption, market
#     share or willingness to pay. Those need additional evidence.
#   - Concepts shown as descriptions or videos are not the same as products
#     handled and tasted. Record the presentation mode.
#   - Predictions for attribute levels outside those tested are not
#     supported.


#' Build tricot rankings, keeping valid rows only
build_rankings = function(data, items, input) {

  R = gosset::rank_tricot(data = data,
                          items = items,
                          input = input,
                          validate.rankings = TRUE)

  valid = rowSums(unclass(R) > 0) >= 2

  list(rankings = R[valid, ], valid = valid)

}


#' Match the attribute table to the order of items in the rankings
#'
#' pladmm() takes the rows of its data in the order of the columns of the
#' rankings. rank_tricot() orders items by first appearance, which rarely
#' matches the attribute table, so the table must be matched by name.
#'
#' @param R rankings
#' @param features data.frame with one row per concept
#' @param id character, the column in features with the concept names
align_features = function(R, features, id = "concept") {

  items = colnames(unclass(R))

  missing_items = setdiff(items, features[[id]])
  if (length(missing_items) > 0) {
    stop("concepts without attributes: ", paste(missing_items, collapse = ", "),
         call. = FALSE)
  }

  features[match(items, features[[id]]), , drop = FALSE]

}


#' Coefficients of the attribute model with standard errors
attribute_effects = function(mod) {

  s = summary(mod)$coefficients

  data.frame(term = rownames(s),
             estimate = s[, "Estimate"],
             se = s[, "Std. Error"],
             lower = s[, "Estimate"] - 1.96 * s[, "Std. Error"],
             upper = s[, "Estimate"] + 1.96 * s[, "Std. Error"],
             p_value = s[, "Pr(>|z|)"],
             row.names = NULL)

}


#' Fit the attribute model
fit_attribute_model = function(R, formula, features, id = "concept", rho = 8) {

  f = align_features(R, features, id)

  PlackettLuce::pladmm(R, formula, data = f, rho = rho)

}


#' Do the attributes explain the concept worths?
#'
#' Likelihood-ratio test of the attribute model against the free
#' Plackett-Luce model, which estimates one worth per concept. Both are fitted
#' without pseudo-rankings so their likelihoods are comparable. A small
#' p-value means the attributes in the formula leave systematic differences
#' between concepts unexplained, such as an interaction or a missing
#' attribute.
attribute_adequacy = function(R, mod) {

  free = PlackettLuce::PlackettLuce(R, npseudo = 0)

  ll_free = free$loglik
  ll_attr = as.numeric(stats::logLik(mod))

  df = (ncol(unclass(R)) - 1) - mod$rank
  dev = 2 * (ll_free - ll_attr)

  data.frame(loglik_free = ll_free,
             loglik_attributes = ll_attr,
             deviance = dev,
             df = df,
             p_value = stats::pchisq(dev, df, lower.tail = FALSE))

}


#' Attribute effects by consumer group, with a test of difference
#'
#' @param R rankings
#' @param group vector of group labels, one per ranking
#' @inheritParams fit_attribute_model
attribute_effects_by_group = function(R, group, formula, features,
                                      id = "concept", rho = 8) {

  pooled = fit_attribute_model(R, formula, features, id, rho)

  idx = split(seq_len(nrow(R)), group)

  fits = lapply(idx, function(i) {
    fit_attribute_model(R[i, ], formula, features, id, rho)
  })

  effects = do.call("rbind", lapply(names(fits), function(g) {
    e = attribute_effects(fits[[g]])
    e$group = g
    e
  }))

  ll_groups = sum(vapply(fits, function(m) as.numeric(stats::logLik(m)), numeric(1)))
  dev = 2 * (ll_groups - as.numeric(stats::logLik(pooled)))
  df = (length(fits) - 1) * pooled$rank

  list(effects = effects,
       test = data.frame(deviance = dev, df = df,
                         p_value = stats::pchisq(dev, df, lower.tail = FALSE)))

}


#' Predicted relative preference of new attribute combinations
#'
#' Returns the probability that each new concept is preferred over a
#' reference concept. Refuses combinations with numeric values outside the
#' tested range or factor levels that were not tested.
#'
#' @param mod a PLADMM model
#' @param features the attribute table used to fit the model
#' @param newdata data.frame of new concepts with the same attribute columns
#' @param reference character, name of a tested concept
#' @param id character, the concept name column
predict_concepts = function(mod, features, newdata, reference, id = "concept") {

  vars = setdiff(names(features), id)

  for (v in vars) {
    x = features[[v]]
    y = newdata[[v]]
    bad = if (is.numeric(x)) y < min(x) | y > max(x) else !y %in% unique(x)
    if (any(bad)) {
      stop("attribute '", v, "' of concepts ",
           paste(newdata[[id]][bad], collapse = ", "),
           " is outside the tested levels, prediction is not supported",
           call. = FALSE)
    }
  }

  tt = stats::delete.response(mod$terms)
  all = rbind(features[c(id, vars)], newdata[c(id, vars)])
  X = stats::model.matrix(tt, data = all, xlev = mod$xlevels)
  lw = drop(X %*% stats::coef(mod))
  names(lw) = all[[id]]

  ref = lw[reference]

  data.frame(concept = newdata[[id]],
             log_worth_vs_reference = unname(lw[newdata[[id]]] - ref),
             p_preferred_over_reference = unname(stats::plogis(lw[newdata[[id]]] - ref)))

}
