#Predict planting dates using observed planting dates in Tricot trials and environmental data 
#Kaue de Sousa
#Bioversity International
#k.desousa (at) cgiar org
#This script run using the globalenv() of AddCovariatesClimMob.R
#First run 07Aug2017
#Updated 04Jun2018

cat("Fitting model to predict planting dates \n")

mydata = read.csv("data/tricot_data.csv")

#Predict planting dates 
if(crop[m] == "commonbean") years <- c(2003:2016)
if(crop[m] == "durumwheat")  years <- c(2003:2014)
if(crop[m] == "wheat")  years <- c(2003:2014)

#Select covariates for modelling exercise
input <- mydata[,c("planting_date","season","lon","lat")]

#Add week, year and day of the year 
input %<>% 
  mutate(status = rep(1, n),
         season = gsub("[0-9]+", "", .$season),
         week = as.integer(format(.$planting_date,"%V")),
         day = as.integer(format(.$planting_date, "%j")))

#Calculate indices for the first XX days before planting date
ts <- 15
db <- ts
precip <- get.timespan(chirps, mydata$planting_date, ts, days.before = db)
precip <- rainfall.index(precip, ts)

#temperature for the first XX days before planting date
temp <- array(NA, c(nrow(mydata), ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
temp[,,1] <- get.timespan(modis_approx[,,1], mydata$planting_date, ts, days.before = db)
temp[,,2] <- get.timespan(modis_approx[,,2], mydata$planting_date, ts, days.before = db)
temp <- temp.index(temp, ts, index = c("maxDT","minDT","maxNT","minNT"))

#Add indices
input <- bind_cols(input, precip, temp)

#Generate pseudo-absence data for planting dates
set.seed(10)
rep_absc <- 5 #times
min_year <- as.Date(paste0(min(mydata$year),"-01-01"), "%Y-%m-%d")
max_year <- as.Date(paste0(max(mydata$year)-1,"-12-30"), "%Y-%m-%d")

absence <- data.frame(planting_date = as.Date(runif((n*rep_absc), min_year, max_year ), origin = "1970-01-01"),
                      status= 0,
                      lon=rep(mydata$lon, rep_absc),
                      lat=rep(mydata$lat, rep_absc),
                      season="No")

folds <- rep(1:rep_absc, each = n)

#Calculate indices for these dates
input_a <- NULL

for(i in seq_len(rep_absc)){

  tf_i <- absence[folds==i, ]

  precip <- get.timespan(chirps, tf_i$planting_date, ts, days.before = db)
  precip <- rainfall.index(precip, ts)

  #temperature for the first XX days before planting date
  temp <- array(NA, c(nrow(mydata), ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], tf_i$planting_date, ts, days.before = db)
  temp[,,2] <- get.timespan(modis_approx[,,2], tf_i$planting_date, ts, days.before = db)
  temp <- temp.index(temp, ts, index = c("maxDT","minDT","maxNT","minNT"))

  tf_i <- bind_cols(tf_i, precip, temp)

  input_a <- bind_rows(input_a, tf_i)

  rm(tf_i)

}


#Remove pseudo-dates within observed plating months
keep <- !as.integer(format(input_a$planting_date, "%m")) %in% unique(as.integer(format(input$planting_date, "%m")))

input_a <- input_a[keep, ]


#Merge both datasets
input_a %<>% 
  mutate(week = as.integer(format(planting_date,"%V")),
         day = as.integer(format(planting_date,"%j"))) %>%
  bind_rows(., input)

input <- input_a

rm(input_a, absence)

names(input)

# #formula for commom bean includes the different seasons within the year
# if(crop[m] == "commonbean") survformula <- as.formula(Surv(day, status) ~ season + maxDT + minDT + MLDS + MLWS + R10mm + R5mm + Rx5day + Rx1day + week)
# 
# #formula for the other crops includes night temperature, do not include season
# if(crop[m] == "wheat") survformula <- as.formula(Surv(day, status) ~ maxDT + minDT + maxNT + minNT + MLDS + MLWS + R10mm + R5mm + Rx5day + Rx1day)
# 
# #formula for the other crops includes night temperature, do not include season
# if(crop[m] == "rice") survformula <- as.formula(Surv(day, status) ~ maxDT + minDT + maxNT + minNT + MLDS + MLWS + R10mm + R5mm + Rx5day + Rx1day)
# 
# #formula for the other crops includes night temperature, do not include season
# if(crop[m] == "durumwheat") survformula <- as.formula(Surv(day, status) ~ maxDT + minDT + maxNT + minNT + MLDS + MLWS + R10mm + R5mm + Rx5day + Rx1day)
# 
# #fit Cox Proportional Hazards Model
# fit <- coxph(survformula,  data = input )
# # summary(fit)
# # cox_fit <- survfit(fit)
# # autoplot(cox_fit)
# # predict(fit, type="risk", se.fit = F)

# correlations <- cor(input[,c(6:20)])
# corrplot(correlations, method="circle")
# 
# x <- input[,c(6:20)]
# y <- as.factor(input$status)
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)

pd_formula <- as.formula(status ~ day + week + maxDT + minDT + MLDS + MLWS + R20mm + R10mm + R5mm + Rx5day + Rx1day + Rtotal)


fit <- glm(pd_formula, data = input, family = binomial)

# Check model accuracy 
test <- input[ sample(1:nrow(input), nrow(input)*0.3), ]
# 
# # Predictions on the test set
predictTest <- predict(fit, newdata=test, type="response")
# 
# #plot ROC curve 
ROCRpred <- ROCR::prediction(predictTest, test$status)
# ROCRperf <- ROCR::performance(ROCRpred, measure= "tpr", x.measure="fpr")
# plot(ROCRperf, colorize = T)
cat("Model performance:", ROCR::performance(ROCRpred, measure = "auc")@y.values[[1]], "\n")

rm(test, predictTest, ROCRpred)

cat("Predicting planting dates \n")

#array for predicted dates per years
#pdates <- array(NA, dim = c(n, 11, length(years) ))

pdates <- list()

pb <- txtProgressBar(min = 1, max = length(years), style = 3)

for(i in 1:length(years)){
  
  #Expected period farmers could start plant each crop
  if(crop[m] == "commonbean") s <- seq(as.Date(paste0(years[i], "-08-01"), "%Y-%m-%d"), as.Date(paste0(years[i]+1, "-01-31"), "%Y-%m-%d"), 1)
  
  if(crop[m] == "wheat") s <- seq(as.Date(paste0(years[i], "-10-15"), "%Y-%m-%d"), as.Date(paste0(years[i], "-12-31"), "%Y-%m-%d"), 1)
  
  if(crop[m] == "rice") s <- seq(as.Date(paste0(years[i], "-05-15"), "%Y-%m-%d"), as.Date(paste0(years[i], "-08-30"), "%Y-%m-%d"), 1)
  
  if(crop[m] == "durumwheat")  s <- seq(as.Date(paste0(years[i], "-07-15"), "%Y-%m-%d"), as.Date(paste0(years[i], "-09-30"), "%Y-%m-%d"), 1)
  
  #generate a data.frame with expected start.dates 
  #here, each row is a crowdsourced trial with the same sequency of dates in cols 
  s %<>%
    matrix(., nrow = length(.), ncol = n) %>% 
    t(.) %>%
    as.data.frame() %>%
    mutate_at(. , vars(1:length(s)), funs(as.Date( . , origin="1970-01-01")))
  
  #get the precipitation for each of these trials and start.date (cols)
  #this will return a list with the precipitation for the dates
  #each node of the list reprent the precipitation starting in the expected dates (cols) in the data.frame "s"
  precip <- lapply(s[1:ncol(s)], function(X) get.timespan(chirps, start.date = X, ts = ts, days.before = db) )
  
  #get the rainfall indices
  #this will return an array where rows are the trails, cols the indices and layers the start.date in "s"
  precip %<>%
    lapply( . , function(X) rainfall.index(X , ts = ts)) %>%
    bind_rows() %>%
    mutate(id = rep(1:n, times = ncol (s)) )
  
  
  #get temperature indices
  #as temperature works in two dimensions, I create an array for day and night temperature 
  temp <- array(NA, dim = c( (n*ncol(s)), ts , 2 ) )
  #get day and night temperature, return a list with data.frames
  #each row is a trial, coluns are the days after the start.day and nodes are each start.day in "s"
  day <- lapply(s[1:ncol(s)], function(X) as.data.frame( get.timespan( modis_approx[,,1] , start.date = X, ts = ts, days.before = db) ))
  
  night <- lapply(s[1:ncol(s)], function(X) as.data.frame( get.timespan( modis_approx[,,2] , start.date = X, ts = ts, days.before = db) ))
  
  #add this to the array
  #first bind all rows and convert into a matrix 
  #trials will be identified as rep(1:n, each = ncol(s))
  temp[,,1] <- day   %>% bind_rows(.) %>% as.matrix(.)
  temp[,,2] <- night %>% bind_rows(.) %>% as.matrix(.)
  
  #calculate temperature indices 
  temp %<>%
    temp.index( . , ts = ts, index = c("maxDT","minDT","minNT","maxNT"))
  
  #dataframe as input for predict
  #first get a large vector with date, day and week
  date <- unlist(s)
  day <- s %>% unlist( . ) %>% as.Date(. , origin = "1970-01-01") %>% format(. , "%j") %>% as.integer( . )
  week <- s %>% unlist( . ) %>% as.Date(. , origin = "1970-01-01") %>% format(. , "%V") %>% as.integer( . )
  
  #this is a list of data.frames with predictors (indices) for prediction per trial
  pred <- 
    precip %>%
    bind_cols( . , temp) %>%
    mutate(date = date,
           day  = day,
           week = week) %>%
    split(. , .$id)
  
  
  #predic over list o predictors 
  pred <- lapply(pred, function(X) predict(fit , newdata = X , type = "response" )) 
  
  pred %<>%
    map( ~ifelse(is_weakly_greater_than( . , 0.8), 0, NA)) %>% #keep only high response if > 0.8, 0 else NA
    bind_cols() %>% #bind this list
    t() %>% #transpose to fit in the same dimensions as "s"
    add( s + .) %>% #sum with "s" to keep only valid dates for each trial
    mutate_at(. , vars(1:length(s)),  as.integer  ) %>%
    as_tibble( . )
  
  #return a data.frame with quantiles for each trial
  pred %<>% 
    apply(., 1, function(X) quantile( X , probs = seq(0, 1, 0.1), na.rm = TRUE ) ) %>%
    t() %>%
    round()
  
  #if there is NA in pred (model not capable to predict planting dates) use the median of other trials
  if(sum(is.na(pred)) > 0) {
    pred <- apply( pred, 2, function(X) ifelse(is.na(X), median(X, na.rm=TRUE), X) )
    cat("\n Note: median used to impute missing values \n")
  }
  
  #add predictions to list pdates
  pdates[[i]] <- pred
  
  setTxtProgressBar(pb, i)
  
}
  

close(pb)








