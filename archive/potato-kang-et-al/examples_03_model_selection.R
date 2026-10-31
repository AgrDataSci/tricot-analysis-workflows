library("PlackettLuce")
library("gosset")
library("caret")
library("ClimMobTools")
library("abind")
library("foreach")
library("parallel")

#' Combine parallel outputs
#' @param ... arguments passed to methods
#' @noRd
.comb <- function(...) {
  abind::abind(..., along = 1, force.array = TRUE)
}

#' Model call for parallel
#' @param formula a symbolic description of the model, if set as   
#'   \eqn{ y ~ . } all variables in \code{data} are used 
#' @param args a list with model call arguments 
#' @param estimator the name of estimators to select
#' @noRd
.forward_dopar <- function(formula, args){
  
  args <- c(formula, args)
  
  m <- do.call(gosset::crossvalidation, args)
  
  result <- m$raw$estimators
  
  nfold <- m$raw$k
  
  result  <- array(unlist(result), c(1, nfold, ncol(result)))
  
  return(result)
}



# Here you some some data organization to prepare the rankings and 
dat <- read.csv("data/potato-tricot-data-with-covariate_t_r_s_RHoMis.csv")

names(dat)

R <- dat[,c("package_item_A", "package_item_B", "package_item_C",
            "postharvest1_yield_pos","postharvest1_tubersize_neg")]


keep1 <- R[,4] != R[,5] & !is.na(R[,4]) & !is.na(R[,5])

table(keep1)

# covariates
names(dat)
covar <- dat[,c(137:148)] #dat[,c("days_range", "maxDT", "minDT", "maxNT", "minNT")]

keep2 <- apply(covar, 1, function(x) {sum(is.na(x))}) == 0

# join both keep vectors
keep <- keep1 & keep2

R <- R[keep,]
covar <- covar[keep,]

# rankings 
G <- rankTricot(R, 1:3, 4:5, group = TRUE)
  
#remove covariates with near zero variance
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[, -out_r]

colnames(covar)

pld <- cbind(G, covar)

head(pld)

# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree

plot(tree)

library("ggparty")
library("patchwork")
gosset:::plot_tree(tree)
