# organize traits and assessments from read.csv("data/nextgen_cassava_tricot_with_covar.csv")
# this script assumes that a file named dat is loaded in the environment
# dat <- read.csv("data/nextgen_cassava_tricot_with_covar.csv")

# organize processor data and genders
dat <- dat[!is.na(dat$sex), ]

dat$processor <- ifelse(is.na(dat$processor), "Other Person",  dat$processor)

dat$gender_processor <- ifelse(dat$processor  == "Self", dat$sex, dat$other_processor_gender)

table(dat$gender_processor)

dat$gender_processor <- ifelse(is.na(dat$gender_processor), "Woman",  dat$gender_processor)

dat$other_processor_from <- ifelse(dat$processor  == "Self", "", dat$other_processor_from)

table(paste(dat$processor, dat$other_processor_from))

dat <- dat[!is.na(dat$other_processor_from), ]

dat$group <- paste(dat$sex, paste(dat$processor, dat$other_processor_from) , sep = " - ")

groups <- c(sort(unique(dat$group)))

# split into data collection moments
moments <- c("first_month", "third_month", "sixth_month", 
             "ninth_month", "harvesting_fresh_roots", "gari_processing",
             "gari_quality","eba_quality_after_preparation", "eba_quality_after_storage", 
             "final_evaluation")

itemnames <- paste0("variety", letters[1:3])

# get the number of traits
# and make a matrix with the data collection moments
# trait name, trait index 
traits <- names(dat)[grepl("_best", names(dat))]
traits <- traits[!grepl("__best", traits)]

traits_desc <- matrix(NA, nrow = length(traits), ncol = 6,
                      dimnames = list(1:length(traits),
                                      c("moment_tag", "trait_tag", 
                                        "index_pos", "index_neg", 
                                        "trait", "moment")))
for (i in seq_along(traits)) {
  index <- traits[i]
  for (j in seq_along(moments)) {
    
    if (grepl(moments[j], index)) {
      moment <- moments[j]
      break
    }
  }
  
  trait_tag <- gsub(paste0(moment, "_" ), "", index)
  trait_tag <- gsub("_best", "", trait_tag)
  
  index_neg <- gsub("_best", "_worst", index)
  
  moment2 <- gsub("_", " ", moment)
  moment2 <- ClimMobTools:::.title_case(moment2)
  
  traits_desc[i, 1] <- moment
  traits_desc[i, 2] <- trait_tag
  traits_desc[i, 3] <- index
  traits_desc[i, 4] <- index_neg
  traits_desc[i, 6] <- moment2
  
}

traits <- traits_desc

# add the clean names for each trait, this should be done manually
traits[,5] <- c("Overall impression", "Germination sprouting", "Plant growth",
                "Weed competitiveness and canopy formation", "Suitability to soil and environment",
                "Overall impression", "Survival sprouting", "Plant growth",
                "Weed competitiveness and canopy formation", "Branching habit architecture of the plant",
                "Disease resistance", "Overall impression", "Survival sprouting and number of plants left",
                "Plant growth", "Weed competitiveness and canopy formation", "Branching habit architecture of the plant",
                "Disease resistance", "Overall impression", "Plant growth", "Weed competitiveness and canopy formation",
                "Branching habit architecture of the plant", "Suitability to soil and environment", "Fresh roots yield",
                "Density water content", "Root colour", "Root size", "Root shape", "Root rot", "Woody filament",
                "Overall impression", "Easy of peeling", "Mash pulp discolor", "Water release during pressing",
                "Swelling of mash", "Gari yield", "Overall impression", 
                "Gari quality dryness", "Colour", "Heaviness",
                "Smoothness", "Taste", "Woody filament",
                "Overall impression", "Mouldability", "Softness",
                "Draw ability", "Colour", "Not stickiness",
                "Swelling of gari", "Taste", "Overall impression",
                "Mouldability", "Softness", "Draw ability",
                "Smoothness", "Not stickiness to hand and mouth", "Taste",
                "Colour", "Overall impression", "Overall impression", "Choice for replant")


trait_names <- paste0(traits[,1], "_" , traits[,2])

# this list will keep the logical vectors to filter the data per 
# remaining trait, the ones that passed the evaluation on minimal 
# amount of missing data 
trait_list <- list()

# make a vector to register any possible trait to be dropped due to few 
# data availability
trait_dropped <- character()

# run over traits to filter NAs/ties and prepare for analysis
for(i in seq_along(trait_names)){
  
  result <- list()
  
  # get the question full name
  trait_i <- paste0(trait_names[i], c("_best", "_worst"))
  
  dat[,trait_i] <- lapply(dat[,trait_i], function(x) {
    x <- unlist(lapply(strsplit(x, "/"), function(y) y[1]))
    x
  })
  
  # check for data completeness in this trait
  # should return a vector with TRUE, FALSE, 
  # where TRUE = complete, FALSE = missing
  keep2 <- apply(dat[trait_i], 1, is.na)
  keep2 <- as.vector(colSums(keep2) == 0)
  
  # no ties
  keep3 <- apply(dat[trait_i], 1, function(x) {
    x[1] != x[2]
  })
  
  keep3 <- keep3 == 1
  
  # only letters A, B, C
  keep4 <- dat[,trait_i[1]] %in% LETTERS[1:3]
  
  keep5 <- dat[,trait_i[2]] %in% LETTERS[1:3]
  
  keep <- keep2 & keep3 & keep4 & keep5
  
  # check if number of missing data is lower than the threshold
  # at least that the number of valid entries is higher than missper (5)
  cond1 <- sum(keep) > 5
  
  keepit <- all(cond1)
  
  # if lower than missper it will be dropped
  if (isFALSE(keepit)) {
    trait_dropped <- c(trait_dropped, trait_names[i])
    next
  } 
  
  # logical vector to subset the data 
  result <- as.list(traits[i,])
  
  result[["keep"]]       <- keep  
  
  trait_list[[trait_names[i]]] <- result
  
}

trait_dropped

rm(moment2, index, moment, trait_tag, traits_desc, 
   index_neg, trait_i, keepit, cond1, keep3, keep2, keep, result, i, j)