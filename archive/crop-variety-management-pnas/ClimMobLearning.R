# #FUNCTIONS FOR DATA LEARNING AND PORTFOLIOS CONTRUCTIONS USING CROWDSOURCING DATA
# #Kaue de Sousa, Jacob van Etten, Heather Turner 
# #First run 31Nov2017
# #Updated in 20Nov2018
# 
# library(partykit)
# library(psychotree)
# library(qvcalc)
# library(PlackettLuce)
# library(foreach)
# library(doParallel)
# library(abind)
# library(dismo)
# library(rgeos)
# library(gstat)
# library(raster)
# library(alphahull)

# Prepare the data frame for analysis with PlackettLuce ####
# x = a tibble with tricot data
# local = add comparison with local item 
# additional.rank = comparison with local item will be done as addional rank comparing each of tested items with local individually
# function return a PlackettLuce rank (see PlackettLuce::as.rankings)
as.PL <- function(x, local = FALSE, additional.rank = TRUE) {
  #get nrow in x
  n <- nrow(x)
  
  #First we consider the best and worst rankings. 
  #These give the item the observer though was best or worst, coded as A, B or C for the first, second or third variety assigned to the farmer respectively.
  #Convert these to numeric values, allowing us to impute the middle-ranked variety (a strict ranking is assumed here, so the sum of each row should be 6)
  x <- within(x, {
    best <- match(best, c("A", "B", "C")) 
    worst <- match(worst, c("A", "B", "C"))
    middle <- 6 - best - worst
  })
  
  #if there is any NA in item names add a pseudo-item which will be removed later
  if(sum(is.na(x [c("variety_a", "variety_b", "variety_c")])) > 0)  x[is.na(x)] <- "ZZZ"
  
  #This gives an ordering of the three items each observer was given.
  #The names of these items are stored in separate columns
  items <- as.matrix( x [c("variety_a", "variety_b", "variety_c")])
  
  #So we can convert the itemsIDs to the item names
  x <- within(x, {
    best <- items[cbind(seq_len(n), best)]
    worst <- items[cbind(seq_len(n), worst)]
    middle <- items[cbind(seq_len(n), middle)]
  })
  
  #get vector with item names
  itemnames = c(sort(unique(as.vector(unlist(x[c("variety_a", "variety_b", "variety_c")])))))
  #Convert the orderings of the items given to each observer to sub-rankings of the full set of varieties:
  R <- PlackettLuce::as.rankings(x[c("best","middle","worst")], input = "ordering", labels = itemnames)
  #Remove pseudo-item 
  if(any(grepl("ZZZ", itemnames))) R <- R[,-match("ZZZ", itemnames)]
  
  if(local){
    if(additional.rank){
      #Treat the paired comparisons as additional rankings.
      #First we can convert the orderings of the trial varieties to sub-rankings of the full set of items
      #including the local as an additional item, so that we can add the paired comparisons shortly
      #The comparisons with the local item are stored in another set of columns
      #add local to itemnames
      itemnames <- c("Local", itemnames)
      paired <- list()
      for (id in c("a", "b", "c")){
        ordering <- matrix("Local", nrow = n, ncol = 2)
        worse <- x[[paste0("var_", id)]] == "Worse"
        ## name of winner
        ordering[!worse, 1] <- x[[paste0("variety_", id)]][!worse]
        ## name of loser
        ordering[worse, 2] <- x[[paste0("variety_", id)]][worse]
        paired[[id]] <- ordering
      }
      #Again we convert these orderings to sub-rankings of the full set of items and combine them with the rankings of order three:
      paired <- lapply(paired, as.rankings, input = "ordering", labels = itemnames)
      R <- rbind(R, paired[["a"]], paired[["b"]], paired[["c"]])
      
    }
    if(!additional.rank){
      #incorporate the local item into the ranking to capture the full preference information from each observer. 
      #First we count the number of trial items that the observers ranks as better than local item to infer the rank of the local item
      nbetter <- rowSums(x[c("var_a","var_b","var_c")] == "Better")
      add_local <- nbetter + 1
      #Then we adjust the ranks of the trial items that the observer ranks as worse than the local and add the local to the rankings
      worse <- R >= add_local
      R[worse] <- R[worse] + 1
      R <- as.rankings(cbind(R, Local = add_local))
    }
    
  }
  return(R)
}

# Compute log likelihood of a null model ####
logLikNull <- function(object){
  
  #This function assumes that all coefficients are equal to get a true null estimates 
  coeff <- rep(0, dim(object)[[2]])
  
  LL <- apply(object, 1, function(x){
    # Put coefficients in the right order
    v <- as.vector(na.omit(coeff[order(x, na.last = NA)])) 
    l <- 0
    # From Hunter MM(2004) The Annals of Statistics, Vol. 32, No. 1, 384-406, page 397
    for(i in 1:(length(v)-1)) l <- l + v[i] - log(sum(exp(v[(i):(length(v))])))
    
    return(l)
    
  })
  
  sum(LL)
  
}

# Compute deviance ####
deviance_plt <- function(object, newdata){
  AIC <- AIC(object , newdata = newdata)
  df <- attr(logLik(object), "df")
  deviance <- AIC - (2*df)
  deviance
}

# Compute Kendall Tau correlation coefficient of two ranking objects ####
kendallTau_plt <- function(observedrank, predictedcoef){
  
  nr1 <- nrow(predictedcoef)
  nr2 <- nrow(observedrank)
  if(nr1 != nr2){stop("predictedrank and observedrank do not have equal number of rows")}
  
  nc1 <- ncol(predictedcoef)
  nc2 <- ncol(observedrank)
  if(nc1 != nc2){stop("predictedrank and observedrank do not have equal number of columns")}
  
  nc <- nc1
  # Function bellow produces a matrix with two rows
  # First row: Kendall tau value for that row
  # Second row: how many pairs entered the comparison as a weight
  # To make the predicted probability of winning match the ranks (1 = best), we take the negative probability
  # No need to convert the probability to ranks -- the cor() function does that internally
  tau_N <- apply(cbind(observedrank, predictedcoef), 1, 
                 function(X) {
                   
                   obr <- X[1:nc]
                   prr <- X[(nc+1):(2*nc)]
                   
                   keep <- !is.na(obr)
                   obr <- obr[keep]
                   prr <- prr[keep]
                   
                   tau_cor <- cor(-prr, obr, method = "kendall", use = "pairwise.complete.obs")
                   n <- sum(obr > 0, na.rm=TRUE)
                   weight <- n*(n-1)/2
                   
                   return(c(tau_cor, weight))
                   
                 }
  )
  
  # Extract the values from the matrix
  tau <- tau_N[1,]
  N <- tau_N[2,]
  
  # N are very small per observer, so simple averaging will have very little bias
  # No need for z transformation at this stage
  # And z tranformation would always give -Inf or Inf with N=2
  tau_average <- sum(tau * N) / sum(N)
  
  # Effective N is the equivalent N needed if all were compared to all
  # N_comparisons = ((N_effective - 1) * N_effective) / 2
  # This is used for significance testing later
  N_effective <- 0.5 + sqrt(.25 + 2 * sum(N)) 
  
  return(c(tau_average, N_effective))
  
}

# Compute Akaike weights/relative likelihoods/delta-AICs ####
AkaikeWeights <- function(object){
  
  object <- object[!is.na(object)]
  
  delta_aic <- object - min(object, na.rm = TRUE) #the delta(AIC) value
  
  rel_LL <- exp(-0.5 * delta_aic) #the relative likelihood
  
  sum_LL <- sum(rel_LL, na.rm = TRUE) 
  
  weights_aic <- rel_LL/sum_LL #the Akaike weigh
  
  return(list(deltaAIC = delta_aic, rel_LL = rel_LL, weights = weights_aic))
  
}

# Compute pseudo R squared of a Plackett-Luce tree ####
pseudoR2_plt <- function(object, newdata = NULL){
  
  if(class(object)[1] != "pltree") stop("input is not an object of class pltree")
  
  #identify the name of response variable
  pltrank <- all.vars(formula(object))[1]
  
  #Without predictions
  if(is.null(newdata)) {
    #log likelihood of input object
    ll <- logLik(object)[1]
    #identify the position of response variable within newdata
    pos_pltrank <- match(pltrank, names(object[[1]]$data))
    #get response variable as rankings matrix
    R <- object[[1]]$data
    R <- R[,pos_pltrank][1:length(R[,pos_pltrank]), , as.grouped_rankings = FALSE]
    #get number of rows in newdata
    N <- nrow(object[[1]]$data)
  }
  
  #With predictions
  if(!is.null(newdata)){
    #get predicted logLik from object coefficients on newdata
    ll <- deviance_plt(object, newdata) / -2
    #identify the position of response variable within newdata
    pos_pltrank <- match(pltrank, names(newdata))
    #get response variable as rankings matrix
    R <- newdata[,pos_pltrank][1:length(newdata[,pos_pltrank]) , , as.grouped_rankings = FALSE]
    #get number of rows in newdata
    N <- nrow(newdata)
  }
  
  #zeros into NA
  R[R==0] <- NA
  
  #logLik null model using observed rankings and no coefficients
  llNull <- logLikNull(R)
  
  McFadden <- 1 - ll/llNull
  G2 <- -2*(llNull-ll)
  r2ML <- 1 - exp(-G2/N)
  r2ML_max <- 1 - exp(llNull*2/N)
  r2CU <- r2ML/r2ML_max
  
  output <- cbind(McFadden = McFadden, G2 = G2, r2ML = r2ML, r2CU = r2CU)
  
  return(output)
  
}

# Compute Stouffer weighted mean ####
stouffer_mean <- function(object, folds, method = "stouffer1") {
  
  N <- length(folds)
  
  max_folds <- max(folds)
  
  foldsize <- table(folds)
  
  wfold <- sqrt(as.vector(foldsize)/N)
  
  wfold <- wfold/sum(wfold)
  
  if(method=="stouffer2") { wfold <- (max_folds*wfold)/sum(wfold) }
  
  stouffer <- object * wfold
  
  stouffer <- sum(stouffer)
  
  return(stouffer)
  
}

# Compute mean weighted by fold size in blocked cross-validation ####
foldsize_mean <- function(object, folds) {
  
  N <- length(folds)
  
  foldsize <- table(folds)
  
  foldsize <- as.vector(foldsize)
  
  mean_foldsize <- sum(object * foldsize, na.rm = TRUE) / sum(foldsize, na.rm = TRUE)
  
  return(mean_foldsize)
  
}

# Cross validation with Plackett-Luce trees ####

# example("beans", package = "PlackettLuce")
# G <- grouped_rankings(R, rep(seq_len(nrow(beans)), 4))
# formula <- as.formula("G ~ maxTN")
# d <- cbind(G, beans)
# d$P1 <- 1
# n <- nrow(d)
# folds <- as.integer(d$season)
# k <- max(folds)
# minsize <- round((n*0.3), -1)
# alpha <- 0.01
# bonferroni <- TRUE
# verbose <- TRUE
# npseudo = 1.5
# mean.method = "foldsize"


crossvalidation_PLT <- function(formula, d, k = 10, folds = NULL, minsize = 50, 
                                alpha = 0.05, mean.method = NULL, 
                                bonferroni = TRUE, npseudo = 0.5, verbose = TRUE, ...)
{
  #Create folds if needed, check length if given
  n <- nrow(d)
  
  if(is.null(folds)){ folds <- sample(rep(1:k, times = ceiling(n/k), length.out = n), replace=FALSE) }
  
  if(length(folds) != n){ stop("folds and nrow(data) has different length") }
  
  #validate mean.method 
  mean.opt <- c("stouffer1", "stouffer2", "foldsize","equal")
  
  if(is.null(mean.method)) {mean.method = "equal"}
  
  if(!mean.method %in% mean.opt) { stop("unknown mean method, should be one of: \"stouffer1\", \"stouffer2\", \"foldsize\", \"equal\"") }
  
  #Prepare matrix with observations to later compare with predictions for each fold
  observed <- d$G[1:length(d$G),, as.grouped_rankings = TRUE]
  index_observed <- attr(observed, "index")
  observed <- observed[1:length(observed),, as.grouped_rankings = FALSE]
  observed[observed == 0] <- NA
  
  #Split data into lists with training and test set
  train <- list()
  for(i in 1:k){  train[[i]] <- d[folds != i , ]}
  test <- list()
  for(i in 1:k){  test[[i]] <- d[folds == i  , ]}

  #Fit Plackett-Luce trees for each of the training sets
  PLtrees <- lapply(train, function(X) {do.call(pltree,
                                                list(formula = formula,
                                                     data = X, minsize = minsize, alpha = alpha,
                                                     bonferroni = bonferroni, npseudo = npseudo))})
  
  #Get number of terminal nodes in each of the Plackett-Luce trees (for control)
  inernodes <- unlist(lapply(PLtrees, function(X){length(nodeids(X, terminal=TRUE))}))
  
  #Use Plackett-Luce trees fitted on train data to calculate AIC and Deviance and to create a prediction of the test part of the data
  #Get AIC using fitted trees and test data
  AIC <- mapply(function(X,Y) {
    AIC(X, newdata = Y)
    }, X = PLtrees, Y = test[])
  
  #Get deviance using fitted trees and test data
  Deviance <- mapply(function(X,Y) {
    deviance_plt(X, newdata = Y)
    }, X = PLtrees, Y = test[])
  
  #Get pseudo R squared
  pR2 <- lapply(PLtrees, function(X) pseudoR2_plt(X))
  pR2 <- do.call("rbind", pR2)
  
  #Get predictions for these datasets 
  preds <- mapply(function(X,Y) {
    predict(X , newdata = Y)
  }, X = PLtrees, Y = test[])
  
  #Output is a matrix with predictions with cases in the rownames
  #Some rows with lacking data may have dropped, so we spend some effort on reformatting
  #observed and predicted matrices to make them match
  preds_index <- lapply(preds, function(X){ as.integer(rownames(X)) })
  
  #"shrunk" observed ranks
  obs_index <- lapply(preds_index, function(X){ index_observed[index_observed %in% X   ] }) #inside i-th fold and inside preds
  
  observed_shrunk <- lapply(preds_index, function(X){ observed[index_observed %in% X , ]})
  
  #"expand" predicted coef 
  match_index <- mapply(function(X,Y) {
    match(X , Y)}, 
    X = obs_index, Y = preds_index)
  
  predicted_expanded <- mapply(function(X,Y) {
    X[ Y , ] },
    X = preds, Y = match_index[])

  #Calculate Kendall tau correlation by comparing predicted test data with observed test data
  kendallTau <- mapply(function(X,Y) {
     kendallTau_plt(X,Y)[1] }, 
     X =  observed_shrunk , Y = predicted_expanded )
  
  # Indices are then averaged weighted by number of predicted cases
  # This is equivalent to calculating tau over the whole set (tallying discordant and concordant pairs)
  # Since each sample is small (2 or 3 varieties), this is without much bias, not warrantin a z transformation
  # before averaging.
  # In blocked cross-validation, folds can be of unequal size
  # In k-fold cross-validation, this will slightly correct if n/k is not a round number
  # It will not affect the tau value if n/k is a round number
  if(mean.method=="equal"){
    means <- apply(cbind(AIC, Deviance, pR2, kendallTau), 2, function(x) mean(x, na.rm=TRUE))
  }
  
  if(mean.method=="foldsize"){
    means <- apply(cbind(AIC, Deviance, pR2, kendallTau), 2, function(x) foldsize_mean(x, folds))
  }
  
  if(mean.method=="stouffer1" | mean.method=="stouffer2"){
    means <- apply(cbind(AIC, Deviance, pR2, kendallTau), 2, function(x) stouffer_mean(x, folds, method = mean.method))
  }
  
  result <- list(call = deparse(formula, width.cutoff = 500),
                 AIC = as.numeric(means[1]), Deviance = as.numeric(means[2]),
                 McFadden = as.numeric(means[3]), r2ML = as.numeric(means[5]),
                 r2CU = as.numeric(means[6]), kendallTau = as.numeric(means[7]),
                 estimators = cbind(AIC, Deviance, pR2, kendallTau),
                 folds = folds, PLtrees = PLtrees, inernodes = inernodes)
  
  return(result)
  
}

#Create function to combine prediction matrices into 3-dimensional array
#Idea obtained from here:
#https://stackoverflow.com/questions/17570806/parallel-for-loop-with-an-array-as-output
acomb <- function(...) abind::abind(..., along = 1, force.array = FALSE)

#Function that creates a prediction matrix with 
f1 <- function(formula, d, k, folds, minsize, alpha, bonferroni, npseudo, mean.method) {
  
  m <- do.call("crossvalidation_PLT", list(formula = formula,
                                            d = mydata,
                                            k = k, folds = folds, minsize = minsize, 
                                            alpha = alpha, bonferroni = bonferroni,
                                            npseudo = npseudo, mean.method = mean.method))
  
  result <- t(c(m$call, m$estimators[,2], m$AIC, m$McFadden, m$r2ML, m$r2CU, m$kendallTau))
  
  return( result )
  
}




#Predict function that works for a PL tree ensemble
#Aggregating is done by taking the mean
#x is a list of PL trees (ensemble)
#newdata is a dataframe with the variables used to predict for new cases
#... passes arguments to the predict function
predict_ensemble <- function(x, newdata, aggregate_function = mean, ...)
{
  
  ntrees <- length(x)
  vars <- names(coef(x[[1]]))
  nvars <- length(vars)
  array_result <- array(NA, dim= c(nrow(newdata), nvars, ntrees))
  
  for(i in 1:ntrees){
    tree <- x[[i]]
    pr <- predict(tree, newdata = newdata, ...)
    array_result[,,i] <- pr
    
  }
  
  result <- t(apply(array_result, c(1,2), aggregate_function))
  rownames(result) <- vars
  colnames(result) <- rownames(newdata)
  return(result)
  
}


# Tools for portfolios construction ####

#Compute a worst regret matrix from Plackett-Luce trees
worstRegret <- function(object, avg.probs = NULL){
  
  #get node ids
  nodes <- partykit::nodeids(object, terminal = TRUE)
  
  probs <- list()
  for(i in seq_along(nodes)) probs[[i]] <- object[[ nodes[i] ]]$node$info$object
  
  #get estimates from each node
  probs <- lapply(probs, function(X) {
    
    parameters <- psychotools::itempar(X, vcov = TRUE, alias = TRUE)
    parameters <- qvcalc::qvcalc.itempar(parameters)
    parameters <- parameters[[2]]
    parameters <- as.data.frame(as.matrix(parameters))
    
    
  } )
  
  #get names of items
  items <- unique(row.names(probs[[1]]))
  
  #rbind these probabilites
  probs <- do.call("rbind", probs)
  
  #add node to the data.frame
  probs$node <- rep(nodes, each = length(items))
  
  #add item names
  probs$items <- rep(items, times = length(nodes))
  
  #change row names to numeric
  row.names(probs) <- 1:nrow(probs)

  #Regret is difference with the best variety in each node
  probs$regret <- unlist(tapply(probs$estimate, probs$node, function(x) {max(x) - x}))
  
  #Worst regret is the highest regret across the nodes
  WR <- tapply(probs$regret, probs$items, max)
  
  #Average probability of winning of all observers
  if(is.null(avg.probs)) { winProbs <- predict(object)  }
  
  winProbs <- apply(avg.probs, 2, mean)
  
  w <- as.data.frame(cbind(win.probs = winProbs , worst.regret = WR ))
  
  w$items <- row.names(w)
  
  row.names(w) <- 1:nrow(w)
  
  w <- w[,c(3,1:2)]
  
  return(w)
  
}


#Compute reliability, relative gains compared to a reference object
#return a raster object with relative gains ranging from 0 to 1
reliability <- function(object, reference = NULL, alternative = NULL, xy = NULL, avg.probs = NULL, 
                        resolution = c(0.04166667, 0.04166667), projection = "+proj=longlat +datum=WGS84",
                        convexhull = TRUE){
  
  #if average probabilities from cross-validation is not provided then use a simple matrix from "predict" function
  if(is.null(avg.probs))  {winProbs <- predict(object)  }
  if(!is.null(avg.probs)) {winProbs <- avg.probs}
  
  #get number os rows and item names
  n <- dim(winProbs)[1]
  items <- dimnames(winProbs)[[2]]
  
  #check if the provided reference item exists within the item names
  if(!reference %in% items) {stop(paste0("unkown reference item, choose one among: ", toString(items), "\n"))}
  
  #if no alternative item is given, pick one using "worstRegret" function.
  if(is.null(alternative)) {
    WR <- worstRegret(tree)
    alternative <- WR[which.min(WR[,3]), 1]
    }
  #check if provided alternative item exists within the item names
  if(!alternative %in% items) stop(paste0("unkown alternative item, choose one among: ", toString(items), "\n"))
  
  #calculate the probability of the alternative item beat the reference item
  reliab <- winProbs[,alternative] /  (winProbs[,reference] + winProbs[,alternative])

  #Make the raster with reliability
  #geostats analysis
  #change names in given coordinates
  names(xy) <- c("x","y")
  myproj <- projection
  
  #Make a convex hull using coordinates
  #If dataset is larger than 5000 observations
  #take a sample to avoid problems in RAM memory
  if(n > 5000){
    #calculate largest distance between coordinates
    largedist <- xy[ sample(row.names(xy), 4000) , ]
    largedist <- raster::pointDistance(largedist, longlat = FALSE)
    largedist <- max(largedist, na.rm = TRUE)
    
  }
  if(n < 5000){
    #calculate largest distance between coordinates
    largedist <- raster::pointDistance(xy, longlat = FALSE)
    largedist <- max(largedist, na.rm = TRUE)
  }
  
  #convex hull
  hull <- dismo::convHull(xy, lonlat=TRUE)
  hull <- rgeos::gBuffer(hull@polygons, width=0.1*largedist)
  
  #set projection
  crs(hull) <- myproj
  #define extention for interpolation
  myext <- raster::extent(hull)
  
  #Add realiability values to cooordinates
  xy$reliab <- reliab
  
  #Define raster
  r <- raster::raster(hull)
  #Set the resolution of the cells
  raster::res(r) <- resolution

  #Resample coordinates and remove possible points within the same grid
  s <- dismo::gridSample(xy[,c("x","y")], r, n=1)
  xy <- xy[rownames(s),]

  #convert to coordinates
  sp::coordinates(xy) <- ~x+y
  sp::proj4string(xy) <- CRS(myproj)
  
  #Define the grid extent 
  x_range <- c(myext[1],myext[2])
  y_range <- c(myext[3],myext[4])
  
  #Create an empty grid where n is the total number of cells
  grd <- expand.grid(x = seq(x_range[1], x_range[2], by = 0.01),
                     y = seq(y_range[1], y_range[2], by = 0.01))
  
  sp::coordinates(grd) <- ~x+y
  sp::gridded(grd)     <- TRUE
  sp::proj4string(grd) <- proj4string(xy)
  
  #interpolate the grid cells
  p_idw <- gstat::idw(reliab ~ 1, locations = xy, newdata = grd)
  #convert to raster object then clip
  r <- raster::raster(p_idw)
  
  #Crop raster to fit the convexhull
  if(convexhull) {r <- raster::mask(r, hull)}
  
  r <- raster::crop(r, myext)

  return(r)
  
}


#Compute minimum regret
minRegretn <- function(loss, n)
{
  
  regretn <- function(x, loss){
    
    n <- length(x)
    for(i in 1:n){loss[i,] <- loss[i,] * x[i]}
    loss <- apply(loss, 2, sum)
    result <- max(loss) + (sum(x) - 1)^2 * 1e10 #penalty if the contributions don?t add up to 1.
    return(result)
    
  }
  
  #optim can sometimes get stuck in a local optimum
  #so we give it random starting values ten times for each n
  #and take the best solution
  for(i in 1:(n*10)){
    
    v <- 1
    r1 <- optim(runif(n), 
                regretn, 
                method="L-BFGS-B", 
                lower=rep(0,3), 
                upper=rep(1,3), 
                loss=loss)
    if(r1$value < v){r2 <- r1; v <- r2$value}
    
  }
  result <- NULL
  result$loss <- r2$value
  result$contributions <- r2$par
  return(result)
  
}

create_portfolio <- function(ensemble, newdata, n, digits=5)
{
  
  #Predict variety probability of winning
  pred <- predict_ensemble(ensemble, newdata)
  
  #For the loss matrix, get the winner varieties first
  winners <- apply(pred, 2, max)
  loss <- t(1 - t(pred) / winners) #Varieties in rows, seasonal climates in columns
  
  vars <- names(coef(x$trees[[1]]))
  nvars <- length(vars)
  
  if(n == 1){
    
    #Prepare results dataframes
    result <- data.frame(vars = vars, loss = NA)
    
    #Create "portfolios" of one variety
    for(i in 1:nvars){result$loss[i] <- round(max(loss[i,]), digits=digits)}
    colnames(result) <- c("Var", "loss_portfolio")
    
    return(result)
    
  } else {
    
    cc <- combn(1:nvars, n)
    loss_portfolio <- NULL
    contributions <- matrix(NA, nrow=ncol(cc), ncol=n)
    colnames(contributions) <- paste("Contr_Var", 1:n, sep="_")
    
    for(i in 1:ncol(cc)){
      
      s <- minRegretn(loss=loss[cc[,i],], n=n)
      loss_portfolio[i] <- round(s$loss, digits=digits)
      contributions[i,] <- round(s$contributions, digits=digits)
      
    }
    
    varieties <- matrix(NA, nrow=ncol(cc), ncol=n)
    for(i in 1:n){varieties[,i] = vars[cc[i,]]}
    colnames(varieties) <- paste("Var", 1:n, sep="_")
    
    result <- data.frame(varieties, contributions, loss_portfolio)
    return(result)
    
  }
  
}