library("PlackettLuce")
library("gosset")
library("partykit")
library("gtools")
library("patchwork")
library("ggplot2")

# function to extract pladmm coeffs as data.frame 
pladmm_coeffs <- function(object, ...) {
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  # get models from each node
  nodes <- list()
  for (i in seq_along(node_id)) {
    obj_i <- object[[ node_id[i] ]]$node$info$object
    coefs <- coef(obj_i)
    coefficients <- matrix(NA, nrow = length(coefs), ncol = 4L, 
                           dimnames = list(names(coefs), c("Estimate", "Std. Error", 
                                                           "z value", "Pr(>|z|)")))
    coefficients[, 1L] <- coefs
    se <- sqrt(diag(vcov(obj_i)))
    coefficients[names(se), 2L] <- se
    coefficients[, 3L] <- coefficients[, 1L]/coefficients[, 2L]
    coefficients[, 4L] <- 2L * pnorm(-abs(coefficients[, 3L]))
    
    coefficients <- as.data.frame(coefficients)
    
    coefficients[, 5] <- gtools::stars.pval(coefficients[, 4])
    
    coefficients[, 4] <- formatC(coefficients[, 4], format = "e", digits = 2)
    
    coefficients[, 6] <- node_id[i]
    
    coefficients[, 7] <- rownames(coefficients)
    
    rownames(coefficients) <- 1:nrow(coefficients)
    
    coefficients <- coefficients[,c(6, 7, 1:5)]
    
    names(coefficients)[1] <- "Node"
    
    names(coefficients)[c(2, 7)] <- ""
    
    nodes[[i]] <- coefficients
    
  }
  
  result <- do.call("rbind", nodes)
  
  rownames(result) <- 1:nrow(result)
  
  result
  
}

# make PlackettLuce rankings
list.files("data", full.names = TRUE)

dat <- read.csv("data/tricot-consumer-cassava-data.csv")

features <- read.csv("data/blups-cassava.csv")

names(dat)

pack <- c("option_a", "option_b", "option_c")

trait <- c("taste", "mouldability", "smoothness")

R <- list()

for(i in seq_along(trait)){
  R[[i]] <- rank_tricot(dat, 
                        pack, 
                        paste0(c("best_", "worst_"), trait[i]),
                        validate.rankings = TRUE, 
                        group = TRUE)
}


genotype <- rbind(R[[1]], R[[2]], R[[3]])

genotype <- group(genotype, index = rep(1:length(R[[1]]), 3))

# run the model
names(features)

sel <- c("genotype", "percent_soluble", 'color_l', 'color_a', 'color_b',
         'cohesiveness', 'gumminess', 'chewiness', 'hardness_g', 'adhesiveness_g_sec_1', 
         'springiness_percent')

features <- features[sel]

names(features)[-1] <- c("Soluble", "L", "a", "b", 
                         "Cohesiveness", "Gumminess", "Chewiness",
                         "Hardness", "Adhesiveness", "Springiness")

form <- paste0("~ ", paste(names(features)[2:10], collapse = " + ")) #[c(3, 10:17)]
form <- as.formula(form)
form

covar <- data.frame(one = rep(1, length(R[[1]])))

pld <- cbind(genotype, covar)

head(pld)
# fit the pladmm using the group rankings of the three 
# traits assessed by participants
mod <- pltree(genotype ~ one,
              worth = form,
              data = list(pld, 
                          features))

summary(mod)


# do it only for taste only 
genotype <- R[[1]]
mod2 <- pladmm(genotype, 
               form,
               data = features)

summary(mod2)


# do it combining with covariates
names(dat)

covar <- c("region", "consumers_gender", "ethnic_group", "how_often_do_you_consume_eba", "age")

covar <- dat[, covar]

head(covar)

unique(covar$consumers_gender)
table(covar$consumers_gender)
hist(covar$age)
boxplot(age ~ consumers_gender, data = covar)
table(covar$ethnic_group)

table(covar$how_often_do_you_consume_eba)

covar[1:4] <- lapply(covar[1:4], as.factor)

str(covar)

names(covar) <- c("Region", "Gender", "EthnicGroup", "Consumption", "Age")

# get the ranking as grouped rankings
genotype <- rbind(R[[1]], R[[2]], R[[3]])

genotype <- group(genotype, index = rep(1:length(R[[1]]), 3))

pld <- cbind(genotype, covar)

head(pld)

# fit the pladmm using the group rankings of the three 
# traits assessed by participants
mod3 <- pltree(genotype ~ .,
              alpha = 0.1,
              worth = form,
              data = list(pld, 
                          features))

mod3

pladmm_coeffs(mod3)

node_rules(mod3)

# extract the node models to build the tree using 
# qvcalc, ggparty and patchwork

reference <- "Akpu"

nodes <- predict(mod3, type = "node")

node_ids <- sort(unique(nodes))

models <- list()
nobs <- integer()

for (i in seq_along(node_ids)) {
  x <- pld[nodes == node_ids[i], ]
  nobs <- cbind(nobs, nrow(x))
  x <- PlackettLuce(x$genotype)
  models[[i]] <- x
}

branch <- gosset:::build_tree_branches(mod3)
node <- gosset:::build_tree_nodes(models,
                                  ref = reference,
                                  log = TRUE,
                                  node.ids = node_ids, 
                                  n.obs = nobs,
                                  ci.level = 0.9)

tree <- branch / node

tree


mod3_coef <- pladmm_coeffs(mod3)

mod3_coef

mod3_coef

rules <- node_rules(mod3)
names(rules) <- c("Node", "Group")

mod3_coef <- merge(rules, mod3_coef, by = "Node", all.y = TRUE)

mod3_coef

mod3_coef

names(mod3_coef)[3] <- ""
names(mod3_coef)[8] <- ""

mod3_coef$Estimate[mod3_coef[,3] == "(Intercept)"] <- 0

dir.create("output", showWarnings = FALSE)

write.csv(mod3_coef, file = "output/pltree-consumer-choices.csv", row.names = FALSE)

ggsave("output/pltree-consumer-choices.png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 600)



mod_coef <- pladmm_coeffs(mod)

mod_coef <- mod_coef[-1]

mod_coef[1,2] <- 0

names(mod_coef)[6] <- ""

write.csv(mod_coef, 
          file = "output/consumer-choices-overall.csv", row.names = FALSE)



