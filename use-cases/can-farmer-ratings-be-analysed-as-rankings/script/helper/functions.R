# Functions for the use case "Can farmer ratings be analysed as rankings?"
# Status: reviewed-not-executed (see tests/)
# Decision record: decisions/data-synthesis.md, section "Ratings to rankings"
#
# Scientific intent
#   Farmers in participatory variety selection often score each plot on a
#   short scale, for example overall appreciation from 1 (poor) to 5
#   (excellent). Scores from different farmers are not comparable, since each
#   farmer uses the scale in their own way, and a score is a rating, not a
#   ranking. The approach of Gesesse et al. (2023, PNAS) and Woldeyohannes et
#   al. (2022, eLife) keeps only the order within each farmer. Every pair of
#   genotypes a farmer scored becomes a paired comparison, ties become no
#   information, and the comparisons feed a Plackett-Luce model, trees on
#   farmer covariates (gender, location) and, in the companion use case
#   which-measured-traits-explain-preference, item covariates from station
#   data.
#
# Approach
#   1. Account for the information in the ratings before converting them,
#      the share of tied pairs per farmer and the agreement between farmers.
#   2. Build the paired comparisons per farmer. A higher score wins, a tie is
#      NA. Report how much is lost.
#   3. When the item set is too large for the model, aggregate items to
#      groups (families, in the paper) by the mean score per farmer, and say
#      so. The loss is the within-group ordering.
#   4. Convert to grouped rankings (psychotools::paircomp, then
#      PlackettLuce::as.grouped_rankings), fit the Plackett-Luce model and
#      reliability against a reference.
#   5. Fit a Plackett-Luce tree on farmer covariates to see whether groups of
#      farmers order the genotypes differently.
#
# Assumptions
#   - A farmer's scores are comparable with each other. Each farmer scored
#     all plots in the same session, so the order within farmer is meaningful.
#   - A tie carries no preference. Two plots with the same score are treated
#     as not compared, not as equal in worth.
#   - Replicates of a farmer at a location are separate sets of comparisons.
#
# Alternatives
#   - Rankings with ties (rank within farmer, ties kept) when the scale is
#     fine enough that tie orders stay small. PlackettLuce handles ties up to
#     order four, and a five-point scale on dozens of plots gives much larger
#     tie sets, which is why the paper used paired comparisons.
#   - Mixed models on the scores with farmer as a random effect, which keep
#     the magnitude but assume scores are interval data.
#   - BLUPs of the scores (the heritability route in the papers), which show
#     that ratings carry genetic signal, and then rank the BLUPs.
#
# Limits
#   - Ties are dropped, so a coarse scale discards most pairs. Report the
#     share kept. With few distinct scores per farmer the data cannot support
#     fine differences between genotypes.
#   - Worth from paired comparisons is relative to the genotypes each farmer
#     saw. Genotypes rarely scored have wide uncertainty.
#   - A split on gender or location shows that the order differs. It does not
#     say why.


#' Summary of the information in the ratings before conversion
#'
#' @param ratings data.frame with columns rater, item and score
#' @return data.frame with one row per rater, the number of items scored, the
#'   number of distinct scores used, the share of pairs that are tied and the
#'   number of untied pairs
rating_information = function(ratings) {

  do.call("rbind", lapply(split(ratings, ratings$rater), function(x) {
    x = x[!is.na(x$score), ]
    n = nrow(x)
    pairs = n * (n - 1) / 2
    tab = table(x$score)
    tied = sum(tab * (tab - 1) / 2)
    data.frame(rater = x$rater[1], items = n,
               distinct_scores = length(tab),
               pairs = pairs,
               tied_share = if (pairs > 0) tied / pairs else NA_real_,
               untied_pairs = pairs - tied,
               row.names = NULL)
  }))

}


#' Agreement between raters on the items they both scored
#'
#' Spearman correlation between the scores of every two raters over their
#' shared items. Low agreement with high tied shares means the ratings carry
#' little order to recover.
#'
#' @param ratings data.frame with columns rater, item and score
#' @param min_shared minimum number of shared items for a pair of raters
#' @return data.frame with rater_a, rater_b, shared items and rho
rater_agreement = function(ratings, min_shared = 5) {

  w = stats::reshape(ratings[, c("rater", "item", "score")],
                     idvar = "item", timevar = "rater", direction = "wide")
  rownames(w) = w$item
  w = as.matrix(w[, -1, drop = FALSE])
  colnames(w) = sub("^score\\.", "", colnames(w))

  pairs = utils::combn(colnames(w), 2)
  out = apply(pairs, 2, function(p) {
    ok = stats::complete.cases(w[, p])
    rho = if (sum(ok) >= min_shared) {
      suppressWarnings(stats::cor(w[ok, p[1]], w[ok, p[2]], method = "spearman"))
    } else NA_real_
    data.frame(rater_a = p[1], rater_b = p[2], shared = sum(ok), rho = rho)
  })
  out = do.call("rbind", out)
  rownames(out) = NULL
  out

}


#' Aggregate items to groups by the mean score per rater
#'
#' Used when the model cannot handle the number of items, as in the paper,
#' where lines were aggregated to their NAM family. The ordering of items
#' within a group is lost, and a group is represented by the mean of
#' whatever its members scored. State this when reporting.
#'
#' @param ratings data.frame with columns rater, item and score
#' @param groups named character vector, item to group
#' @return ratings at the group level, with the number of items behind each
#'   mean in column n
aggregate_items = function(ratings, groups) {

  ratings$group = groups[ratings$item]
  if (any(is.na(ratings$group))) {
    stop("every item needs a group", call. = FALSE)
  }
  ratings = ratings[!is.na(ratings$score), ]
  agg = stats::aggregate(score ~ rater + group, data = ratings, FUN = mean)
  n = stats::aggregate(score ~ rater + group, data = ratings, FUN = length)
  agg$n = n$score
  names(agg)[names(agg) == "group"] = "item"
  agg

}


#' Paired comparisons from ratings, one row per rater
#'
#' For every pair of items, 1 when the first item has the higher score, -1
#' when the second has, NA when they are tied or one was not scored. Pairs
#' are ordered as psychotools::paircomp() expects them, column by column of
#' the upper triangle, so (1,2), (1,3), (2,3), (1,4), (2,4), (3,4), ...
#'
#' @param ratings data.frame with columns rater, item and score
#' @param items character, the items in the order to use
#' @return integer matrix [rater, pair] with raters as row names
paired_comparisons = function(ratings, items = sort(unique(ratings$item))) {

  idx = which(upper.tri(diag(length(items))), arr.ind = TRUE)

  by_rater = split(ratings, ratings$rater)
  M = t(vapply(by_rater, function(x) {
    s = stats::setNames(rep(NA_real_, length(items)), items)
    s[x$item] = x$score
    a = s[idx[, 1]]
    b = s[idx[, 2]]
    out = ifelse(a > b, 1L, ifelse(a < b, -1L, NA_integer_))
    as.integer(out)
  }, integer(nrow(idx))))
  if (nrow(idx) == 1) M = matrix(M, ncol = 1)
  rownames(M) = names(by_rater)
  colnames(M) = paste(items[idx[, 1]], items[idx[, 2]], sep = " vs ")
  attr(M, "items") = items
  M

}


#' Grouped rankings from the paired comparisons
#'
#' Raters with no untied pair are dropped and reported, because they give no
#' ranking.
#'
#' @param M matrix from paired_comparisons
#' @return list with the grouped_rankings (one group per rater), the raters
#'   kept, in the order of the groups, and the raters dropped
grouped_from_pairs = function(M) {

  items = attr(M, "items")
  keep = rowSums(!is.na(M)) > 0
  dropped = rownames(M)[!keep]
  M = M[keep, , drop = FALSE]
  pc = psychotools::paircomp(M, labels = items)
  G = PlackettLuce::as.grouped_rankings(pc)
  list(G = G, raters = rownames(M), dropped = dropped)

}


#' Rankings with ties kept, for comparison with the paired route
#'
#' Rank within rater with ties sharing a rank. Reports the largest tie set
#' per rater, which PlackettLuce can only handle up to order four.
#'
#' @param ratings data.frame with columns rater, item and score
#' @return list with the rank matrix (0 for items not scored) and the
#'   largest tie order per rater
rankings_with_ties = function(ratings) {

  items = sort(unique(ratings$item))
  by_rater = split(ratings, ratings$rater)
  R = t(vapply(by_rater, function(x) {
    x = x[!is.na(x$score), ]
    r = stats::setNames(rep(0L, length(items)), items)
    r[x$item] = as.integer(rank(-x$score, ties.method = "min"))
    r
  }, integer(length(items))))
  rownames(R) = names(by_rater)
  tie_order = vapply(by_rater, function(x) {
    as.numeric(max(table(x$score[!is.na(x$score)])))
  }, numeric(1))
  list(R = R, max_tie_order = tie_order)

}


#' Plackett-Luce tree on rater covariates
#'
#' @param G grouped_rankings, one group per rater
#' @param covariates data.frame, one row per rater in the order of G
#' @param alpha significance level for a split
#' @param minsize minimum number of raters per node
#' @details PlackettLuce must be attached with library("PlackettLuce"),
#'   because pltree() looks up its fitting functions on the search path
fit_rater_tree = function(G, covariates, alpha = 0.01, minsize = 10) {

  d = cbind(G, covariates)
  f = stats::as.formula(paste("G ~", paste(names(covariates), collapse = " + ")))
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


#' Log worth per node relative to a reference item
worth_by_node = function(tree, ref) {

  nodes = partykit::nodeids(tree, terminal = TRUE)
  w = t(stats::coef(tree, ref = ref, log = TRUE))
  colnames(w) = paste0("node_", nodes)
  data.frame(item = rownames(w), w, row.names = NULL)

}
