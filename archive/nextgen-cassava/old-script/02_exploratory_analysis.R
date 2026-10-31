# .......................................
# .......................................
# Analyse the NextGen Cassava tricot data 
library("gosset")
library("sf")
library("ClimMobTools")
library("PlackettLuce")
library("ggplot2")
library("ggparty")
library("tidyverse")
library("patchwork")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/02_exploratory_analysis.txt")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
dat <- read.csv("data/nextgen_cassava_tricot_with_covar.csv", na.strings = c("-", "NA"))

nga <- readRDS("data/gadm36_NGA_1_sf.rds")

class(nga)

nga

# read zones 
zones <- read.csv("data/nga_zones.csv")

sum(nga$NAME_1 %in% zones$NAME_1)

nga <- merge(nga, zones, by = "NAME_1")

ngaz <- 
  nga %>% 
  group_by(Zone) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
  
plot(ngaz)

# create a group to split the data by gender and region
dat <- dat[!is.na(dat$sex), ]

dat$processor <- ifelse(is.na(dat$processor), "Other Person",  dat$processor)

dat$gender_processor <- ifelse(dat$processor  == "Self", dat$sex, dat$other_processor_gender)

table(dat$gender_processor)

dat$gender_processor <- ifelse(is.na(dat$gender_processor), "Woman",  dat$gender_processor)

dat$other_processor_from <- ifelse(dat$processor  == "Self", "", dat$other_processor_from)

table(paste(dat$processor, dat$other_processor_from))

dat <- dat[!is.na(dat$other_processor_from), ]

dat$group <- paste(dat$sex, dat$agroregion , sep = " - ") #paste(dat$processor, dat$other_processor_from)

table(dat$group)

groups <- c("All", sort(unique(dat$group)))

colrs <- c('#7f7f7f','#8c510a','#bf812d', '#35978f','#80cdc1') #'#dfc27d','#f6e8c3') ,'#01665e')

length(groups) == length(colrs)


# .......................................
# .......................................
# split into data collection moments ####
names(dat)

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
                                      c("moment_tag", "trait_tag", "index_pos", "index_neg", "trait", "moment")))
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

rm(moment2, index, moment, trait_tag, traits_desc, index_neg)

as.vector(traits[,2])

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
  keep <- apply(dat[trait_i], 1, is.na)
  keep <- as.vector(colSums(keep) == 0)
  
  # no ties
  keep2 <- apply(dat[trait_i], 1, function(x) {
    x[1] != x[2]
  })
  keep2 <- keep2 == 1
  
  keep <- keep & keep2
  
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

# .......................................
# .......................................
# Prepare summary tables and overall participation ####
nparticipant <- nrow(dat)

# summary table with genotypes tested by gender
genotypes <- dat[, grepl("variety", names(dat))]

infotable <- data.frame(table(unlist(genotypes)))

infotable$x <- with(infotable,
                    round((Freq / nparticipant), 1))

names(infotable) <- c("Genotype", "Freq", "Relative freq")

genotypes <- unlist(genotypes)

gender <- dat[, "sex"]
gender <- ifelse(is.na(gender), "Woman", gender)

gendrs <- unique(gender)

ngendrs <- table(gender)

gendertbl <- c()

for (i in seq_along(gendrs)) {
  gendertbl <- cbind(gendertbl, 
                     tapply(rep(gender, 3), genotypes, function(x) sum(x == gendrs[i], na.rm = TRUE)))
}

gendertbl <- as.data.frame(gendertbl)

names(gendertbl) <- paste0(gendrs, " (n=", ngendrs, ")")

infotable <- cbind(infotable, gendertbl)

# write file in the output
dir.create("output", showWarnings = FALSE)

write.csv(infotable, "output/summary_table_genotypes_tested_by_gender.csv", row.names = FALSE)

# .......................................
# .......................................
# Participation (response rate) ####
overall_indices <- which(grepl("overallimpression", names(trait_list)))

participation <- data.frame()

for(i in seq_along(overall_indices)) {
  
  n_total <- sum(trait_list[[overall_indices[i]]]$keep)
  n_man <- sum(trait_list[[overall_indices[i]]]$keep & dat$sex == "Man")
  n_woman <- sum(trait_list[[overall_indices[i]]]$keep & dat$sex == "Woman")
  
  part <- data.frame(value = c(n_total, n_man, n_woman),
                     group = c("Both", "Man", "Woman"),
                     dc = trait_list[[overall_indices[i]]]$moment)
  
   participation <- rbind(participation, part)
  
}

participation

# transform into proportion to make it easier to visualize
participation$value_perc <- participation$value / rep(c(nrow(dat), 
                                                        sum(dat$sex == "Man", na.rm = TRUE),
                                                        sum(dat$sex == "Woman", na.rm = TRUE)),
                                                      times = length(overall_indices))

participation

participation$dc <- factor(participation$dc, levels = unique(participation$dc))

partiplot <- 
  ggplot(participation, aes(x = dc, y = value_perc, 
              group = group, color = group, linetype = group)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.5, 1)) +
  scale_colour_manual(values = c("grey50", "#8c510a", "#5ab4ac"), name = "", guide = FALSE) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash"), guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(color = c("grey50", "#8c510a", "#5ab4ac"), 
                                                   linetype = c("solid", "dashed", "twodash")))) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20")) +
  labs(x = "Crop stage", y = "Rate of response")

partiplot

ggsave(filename = "output/participation_across_trial.png", 
       plot = partiplot,
       dpi = 600,
       units = "cm",
       height = 15,
       width = 20)

ggsave(filename = "output/participation_across_trial.svg", 
       plot = partiplot,
       units = "cm",
       height = 15,
       width = 20)


# ......................................................
# ......................................................
# fit a GLM to see the differences in response rate
p <- participation[participation$group != "Both",]

m <- glm(value_perc ~ group + dc, data = p)

summary(m)

summary(aov(m))

capture.output(summary(m),
               print(cat("\n\n\n")),
               summary(aov(m)),
               file = "output/regression_participation_by_gender.txt")

# .......................................
# .......................................
# Agreement in trait evaluation ####

# now produce the kendall tau tables comparing each trait 
# in each data collection moment against the overall impression
# in the given data collection moment
agreement <- data.frame()

for (i in seq_along(moments)) {
  
  trait_i <- trait_list[grepl(moments[i], names(trait_list))]
  
  # filter the data so it matches the dims in all traits
  keep <- NULL
  
  for(j in seq_along(trait_i)) {
    keep <- cbind(keep, trait_i[[j]]$keep)
  }
  
  keep <- rowSums(keep)
  keep <- keep == length(trait_i)

  # list of rankings
  compare <- list()
  
  for (k in seq_along(groups)) {
    
    cp <- list()
    
    if (k == 1) keep_k <- keep
    if (k != 1) keep_k <- keep & dat$group == groups[k]
    
    for (j in seq_along(trait_i)) {
      cp[[j]] <- rank_tricot(dat[keep_k, ],
                             items = itemnames,
                             input = c(trait_i[[j]]$index_pos, trait_i[[j]]$index_neg))
      
    }
    
    compare[[k]] <- cp
  
  }
  
  
  
  reference <- which(grepl("overallimpression", names(trait_i)))
  
  labs <- unlist(lapply(trait_i, function (x) {x[["trait"]]}))
  
  agree <- lapply(compare, function(x) {
    summarise_agreement(x[[reference]], 
                        x[-reference], 
                        labels = labs[-reference])
  })
  
  agree <- do.call(rbind, agree)
  
  agree$group <- rep(groups, each = length(labs[-reference]))
  
  agree$moment <- trait_i[[1]]$moment
  
  agreement <- rbind(agreement, agree)
  
}

agreement

# remove final evaluation
agreement <- as.data.frame(agreement[agreement$moment != "Final Evaluation", ])

agreement$kendall <- agreement$kendall / 100

# write this in the outputs
write.csv(agreement, "output/agreement_table_by_overall_in_each_data_collection_moment.csv")

agreement$moment <- factor(agreement$moment, levels = unique(agreement$moment))

a <- agreement[agreement$group != "All", ]

x <- as.data.frame(do.call(rbind, strsplit(a$group, " - ")))

names(x) <- c("gender", "zone")

a <- cbind(a, x)

head(a)

a$group <- paste(a$moment, a$gender, a$zone, sep = " - ")

a <- split(a, a$group)

a <- lapply(a, function(x) {
  ind <- which.max(x$kendall)[[1]]
  x[ind, ]
})

a <- do.call(rbind, a)

a$labels

a$labels <- paste(a$labels, do.call(rbind, strsplit(rownames(a), " - "))[,1], sep = " - ")

paste(unique(a$labels), collapse = "', '")

lv <- c('Germination sprouting - First Month', 'Suitability to soil and environment - First Month',
        'Plant growth - Third Month', 'Survival sprouting - Third Month',
  'Survival sprouting and number of plants left - Sixth Month', 'Disease resistance - Sixth Month',
  'Suitability to soil and environment - Ninth Month', 'Weed competitiveness and canopy formation - Ninth Month',
  'Plant growth - Ninth Month',
  'Fresh roots yield - Harvesting Fresh Roots',
  'Swelling of mash - Gari Processing', 'Gari yield - Gari Processing',
  'Taste - Gari Quality', 'Smoothness - Gari Quality',
  'Taste - Eba Quality After Preparation', 'Taste - Eba Quality After Storage', 'Smoothness - Eba Quality After Storage')

a$labels <- factor(a$labels, levels = lv)


top_trait <- ggplot(a, aes(x = kendall, y = labels, color = gender)) + 
  geom_point(stat = "identity") +
  facet_wrap(zone ~ gender,  
             nrow = 2,
             scales = "free") +
  scale_colour_manual(values = c("#8c510a", "#5ab4ac"), name = "", guide = FALSE) +
  theme_bw() +
  scale_x_continuous(limits = c(.5, 1),
                     labels = c(.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.background = element_blank()) +
 labs(x = "Kendall correlation", y = "")


ggsave(filename = "output/top_trait_by_crop_stage.png",
       plot = top_trait,
       dpi = 600,
       units = "cm",
       height = 20,
       width = 30)

ggsave(filename = "output/top_trait_by_crop_stage.svg",
       plot = top_trait,
       dpi = 600,
       units = "cm",
       height = 20,
       width = 30)

kendall_correlation <-
  ggplot(agreement, aes(x = kendall, y = labels, fill = group)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = "white",
           show.legend = FALSE) +
  scale_fill_manual(values = colrs) +
  scale_x_continuous(limits = c(0, 1),
                     labels = c(0, .25, .5, .75, 1)) +
  facet_grid(moment ~ group,
             scales = "free",
             space = "free",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.background = element_blank()) +
 labs(x = "Kendall correlation", y = "")

kendall_correlation
 
ggsave(filename = "output/kendall_cor_by_crop_stage.png",
       plot = kendall_correlation,
       dpi = 600,
       units = "cm",
       height = 25,
       width = 46)

ggsave(filename = "output/kendall_cor_by_crop_stage.svg",
       plot = kendall_correlation,
       dpi = 500,
       units = "cm",
       height = 25,
       width = 46)

# .......................................
# .......................................
# Agreement in trait evaluation Overall impression ####
# now compare all the traits against the final evaluation
# filter the data so it matches the dims in all traits
keep <- NULL

for(j in seq_along(trait_list)) {
  keep <- cbind(keep, trait_list[[j]]$keep)
}

colSums(keep)

keep <- rowSums(keep)
keep <- keep == length(trait_list)

sum(keep)

# list of rankings
compare <- list()

for (k in seq_along(groups)) {
  
  cp <- list()
  
  if (k == 1) keep_k <- keep
  if (k != 1) keep_k <- keep & dat$group == groups[k]
  
  print(sum(keep_k))
  
  for (j in seq_along(trait_list)) {
    cp[[j]] <- rank_tricot(dat[keep_k, ],
                           items = itemnames,
                           input = c(trait_list[[j]]$index_pos, trait_list[[j]]$index_neg))
    
  }
  
  compare[[k]] <- cp
  
}

reference <- which(grepl("final_evaluation_choiceforreplant", names(trait_list)))

#labs <- unlist(lapply(trait_list, function (x) {paste(x[["trait"]], " - " , x[["moment"]]) }))
labs <- unlist(lapply(trait_list, function (x) {x[["trait"]]}))
labs2 <- unlist(lapply(trait_list, function (x) {x[["moment"]]}))

agree <- lapply(compare, function(x) {
  summarise_agreement(x[[reference]], 
                      x[-reference], 
                      labels = labs[-reference])
})

agree <- do.call(rbind, agree)

rownames(agree) <- 1:nrow(agree)

agree

agree$kendall <- agree$kendall / 100

agree$group <- rep(groups, each = length(labs[-reference]))

agree$dc <- rep(labs2[-reference], times = length(groups))

write.csv(agree, "output/agreement_table_by_choice_for_replant.csv")

# sort values from lower to higher by data collection moment
# this is to get the levels and set a factor in the plot 
# get only the kendall for All
a <- agree[agree$group == "All", ]
a <- split(a, a$dc)

levels_traits <- as.vector(unlist(lapply(a, function(x) {
  x <- x[order(x$kendall), ]
  union("Overall impression", x$labels)
})))

rm(a)

a <- agree[agree$labels != "Choice for replant", ]
a <- a[a$labels != "Overall impression", ]

a$dc <- factor(a$dc, levels = unique(a$dc))

a$labels <- factor(a$labels, levels = unique(levels_traits))

a$labels <- paste(a$labels, a$dc,  sep = " - ")

a$labels <- factor(a$labels, levels = unique(a$labels))

a$kendall[a$kendall <= 0] <- 0.01

# get the best three from each group
a <- a[a$group != "All", ]
a <- split(a, a$group)

a <- lapply(a, function(x){
  ind <- which(rank(x$kendall * - 1, ties.method = "first") %in% c(1:5))
  x <- x[ind, ]
  x[rev(order(x$kendall)), ]
})

a <- do.call(rbind, a)

rownames(a) <- 1:nrow(a)

x <- as.data.frame(do.call(rbind, strsplit(a$group, " - ")))
names(x) <- c("gender", "zone")

a <- cbind(a, x)

head(a)

topfive <- 
ggplot(a, aes(x = kendall, y = labels, color = gender)) + 
  geom_point(stat = "identity") +
  facet_wrap(zone ~ gender,  
             nrow = 2,
             scales = "free") +
  scale_colour_manual(values = c("#8c510a", "#5ab4ac"), name = "", guide = FALSE) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 1),
                     labels = c(0, .25, 0.5, 0.75, 1))  +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.background = element_blank()) +
  labs(x = "Kendall correlation", y = "")

ggsave(filename = "output/top_five_traits.png",
       plot = topfive,
       dpi = 600,
       units = "cm",
       height = 20,
       width = 30)

ggsave(filename = "output/top_five_traits.svg",
       plot = topfive,
       dpi = 600,
       units = "cm",
       height = 20,
       width = 30)


kendall_cor_overall <- 
  ggplot(a, aes(x = kendall, y = labels, fill = group)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = "white", 
           show.legend = FALSE) +
  scale_fill_manual(values = colrs) +
  scale_x_continuous(limits = c(0, 1),
                     labels = c(0, .25, .5, .75, 1)) +
  facet_grid(~ group, space = "free",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.background = element_blank()) +
  labs(x = "Kendall correlation", y = "")

kendall_cor_overall

ggsave(filename = "output/kendall_cor_overall.png", 
       plot = kendall_cor_overall,
       dpi = 500,
       units = "cm",
       height = 25,
       width = 46)

ggsave(filename = "output/kendall_cor_overall.svg", 
       plot = kendall_cor_overall,
       dpi = 500,
       units = "cm",
       height = 25,
       width = 46)


# fit a GLM to see the differences in correlation
m <- glm(kendall ~ group + labels, data = a)

summary(m)

capture.output(summary(m), file = "output/regression_kendall_cor_overall_impression_by_group.txt")

# .......................................
# .......................................
# PlackettLuce ####
# now fit a PlackettLuce in all rankings and look for the changes in 
# variety performance over time
mod <- list()

for (j in seq_along(groups)) {
  
  if (j == 1) keep <- trait_list[[i]]$keep
  if (j > 1) keep <- trait_list[[i]]$keep & dat$group == groups[j]
  mod_j <- list()
  
  for (i in seq_along(trait_list)) {
    
    R <- rank_tricot(dat[keep, ],
                     items = itemnames,
                     input = c(trait_list[[i]]$index_pos, trait_list[[i]]$index_neg))
    
    mod_j[[i]] <- PlackettLuce(R)
    
    
  }
  
  mod[[j]] <- mod_j
  
}

overall <- which(grepl("overallimpression", names(trait_list)))

mod_overall <- lapply(mod, function(x){
  x[overall]
})

moments <- as.vector(unlist(lapply(trait_list[overall], function(x) {x$moment})))

moments <- rep(moments, length(groups))

gender <- rep(groups, each = length(unique(moments)))

coeffs <- data.frame()

mod <- NULL

for(i in seq_along(mod_overall)) {
  mod <- c(mod, mod_overall[[i]])
}

# get the win probs as a data.frame
for (i in seq_along(mod)) {
  
  c_i <- coef(mod[[i]], log = FALSE)
  
  c_i <- data.frame(genotype = names(c_i),
                    winprob = c_i, 
                    moment = moments[i],
                    group = gender[i])
  
  coeffs <- rbind(coeffs, c_i)
  
}

coeffs

coeffs <- coeffs[coeffs$genotype!="tie2",]
coeffs <- coeffs[coeffs$genotype!="tie3",]

unique(coeffs$genotype)

coeffs$moment <- factor(coeffs$moment, levels = unique(coeffs$moment))

head(coeffs)

# find the max point in the first data collection moment 
maxfirst <- max(coeffs$winprob[coeffs$moment == "First Month" & coeffs$group == "All"])
maxfinal <- max(coeffs$winprob[coeffs$moment == "Final Evaluation" & coeffs$group == "All"])

winprobs <- 
ggplot(coeffs, aes(x = moment, y = winprob, group = group,
                   color = group, linetype = group)) +
  geom_line() +
  scale_colour_manual(values = colrs, name = "", guide = FALSE) +
  scale_linetype_manual(values = c(1:length(groups)), guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(color = colrs, 
                                                   linetype = c(1:length(groups))))) +
  #geom_point(size = 0.5) +
  #geom_hline(aes(yintercept = maxfirst)) +
  #geom_hline(aes(yintercept = maxfinal), linetype = 3) +
  theme_bw() +
  facet_wrap( ~ genotype) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Crop stage", y = "Probability of winning") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 9, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.background = element_blank())


winprobs

ggsave(filename = "output/win_probs_overall_each_data_collection.png", 
       plot = winprobs,
       dpi = 600,
       units = "cm",
       height = 25,
       width = 25)

ggsave(filename = "output/win_probs_overall_each_data_collection.svg", 
       plot = winprobs,
       dpi = 600,
       units = "cm",
       height = 25,
       width = 25)






