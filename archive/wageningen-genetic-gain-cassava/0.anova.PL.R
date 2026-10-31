anova.PL <- function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett-Luce")
  }
 
  LLs = round(c(model$null.loglik, model$loglik), 3)
  dfs = c(model$df.null, model$df.residual)
  df_diff = (-1) * diff(dfs)
  df_LL = round((-1) * diff(LLs), 3)
  p = round(1 - pchisq(-2 * df_LL, df_diff), 4)
  stars = gtools::stars.pval(p)
 
  x = data.frame(Model = c("NULL", deparse(substitute(model))),
                  "logLik" = LLs,
                  DF = dfs,
                  "Statistic" = c(NA, -2 * df_LL),
                  "Pr(>Chisq)" = c(NA, p),
                  " " = c("", stars),
                  check.names = FALSE,
                  stringsAsFactors = FALSE)
 
  return(x)
 
}
 