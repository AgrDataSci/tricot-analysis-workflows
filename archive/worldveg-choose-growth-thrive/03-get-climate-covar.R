library("climatrends")

dat = read.csv("data/block-data.csv")

load("data/climate-data-matrices2.rda")

dat$plantingdate = as.Date(dat$plantingdate)

n = nrow(dat)

ndays = 70

temp_indices = temperature(temp, 
                           day.one = dat$plantingdate, 
                           span = ndays)


rain = as.matrix(rain)

rain_indices = rainfall(rain, 
                        day.one = dat$plantingdate,
                        span = ndays)


# temp = array(c(tempmax, tempmin),
#              dim = c(nrow(tempmax), ncol(tempmax), 2),
#              dimnames = list(rownames(tempmax),
#                              colnames(tempmax),
#                              c("tmax", "tmin")))

