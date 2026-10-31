# .......................................
# .......................................
# This script infers the worst ranking based on 
# the other traits
# list.files("data")
# dat <- read.csv("data/nextgen_cassava_tricot_with_covar.csv")
# 
# names(dat)
# 
# d <- dat[, 1:22]

# we will infer the worst variety based on the 
# other traits evaluated by each moment
# lets do the count of how many times an option 
# is ranked as the worst
index_worst <- grep("_worst", names(d))

d[index_worst]

overall_worst <- apply(d[index_worst], 1, function(x){
  tbl_worst <- table(as.vector(unlist(x)))
  which_worst <- which.max(tbl_worst)
  names(tbl_worst)[which_worst]
})

dat$first_month  <- overall_worst




