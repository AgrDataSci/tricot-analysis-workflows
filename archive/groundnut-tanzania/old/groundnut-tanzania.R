# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")
library("readxl")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix <- function(L){
  
  n_i <- length(L)
  
  #Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) <- names(L)
  colnames(r) <- names(L)
  
  #Prepare the effective N matrix
  N <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) <- names(L)
  colnames(N) <- names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij <- kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] <- as.numeric(rNij[1])
      N[i,j] <- as.numeric(rNij[2]) 
      
    }
    
  }
  
  #Make matrices symmetric    
  r[upper.tri(r)] <- t(r)[upper.tri(r)]
  N[upper.tri(N)] <- t(N)[upper.tri(N)]
  
  #Wrap result
  results <- list(Kendall_tau_corr=r,Effective_N=N)
  return(results)
  
}

genotype_info = read_xlsx("data/groundnut-genotype-release-information.xlsx")


# # read file with API key, this is a txt file 
# # indicating the key(s) and the server from which the data should 
# # be retrieved. Its a matrix with two coluns, the first being the key and the second 
# # being the server
# key = read.table("token/api-key.txt", sep = ",")
# key = data.frame(key = key[,1], server = key[, 2])
# 
# # get the list of projects from the user indicated in the API key
# projects = data.frame()
# 
# for(i in seq_along(key$key)) {
#   projects = rbind(projects, 
#                    getProjectsCM(key$key[i], server = key$server[i]))
# }
# 
# 
# # select by userowner
# userowner = c('Happy')
# 
# 
# projects = projects[projects$user_owner %in% userowner, ]
# 
# projects = projects[grepl("groundnut", tolower(projects$project_name)), ]
# 
# # make a data.frame with project name, userowner, API key and server
# # but not save it, to keep the API key confidential
# serverdata = projects[,c("project_id", "user_owner", "server")]
# serverdata = merge(serverdata, key, by = "server")
# 
# # fetch the data from climmob
# cmdata = list()
# 
# for(i in seq_along(projects$project_id)) {
#   
#   d_i = getDataCM(serverdata$key[i], 
#                   project = serverdata$project_id[i],
#                   userowner = serverdata$user_owner[i],
#                   server = serverdata$server[i],
#                   as.data.frame = FALSE)
#   
#   cmdata[[i]] = d_i
#   
# }
# 
# 
# save(cmdata, file = "data/groundnut-data-tanzania.rda")

load("data/groundnut-data-tanzania.rda")

# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

keep = unlist(lapply(dat, nrow)) > 0

dat = dat[keep]

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

# for the summaries here, I only use data from package distribution and participant registration
uniquenames = uniquenames[grep("id$|package_|registration_|_longitude|_latitude", uniquenames)]

uniquenames

# fix some codes that diverged over time
dat = lapply(dat, function(x){
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  names(x) = gsub("overallchar2", "overall", names(x))
  x
})

# put the data together 
dat = rowbind(dat)

# fix trial coordinator name
sort(unique(dat$package_coordinator))

dat$package_coordinator[dat$package_coordinator == "HAPPY DAUDI"] = "Happy Daudi"

# countries
unique(dat$package_country)

#write.csv(dat, "data/groundnut-data.csv", row.names = F)

# ..................................
# get summaries ####
traits = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(traits, function(x){
  x$trait_label
}))

traits

traits = gsub("_qst_|_|tricot", " ", traits)
traits = ClimMobTools:::.title_case(traits)
traits = gsub("Podyield", "Pod yield", traits)
traits = gsub("Haulmyield", "Haulm yield", traits)
traits = gsub("yield", "Yield", traits)
traits = gsub("Grainsize", "Grain Size", traits)
traits = gsub("Graincolour", "Grain Color", traits)
traits = gsub("content", " Content", traits)
traits = gsub("resistance", " Resistance", traits)
traits = gsub("tolerance", " Tolerance", traits)
traits = gsub("labour", " Labor", traits)
traits = gsub("Physiologicalmaturity ", "", traits)
traits = gsub("Agronomicperformance ", "", traits)
traits = gsub("Reproductive Diseases Resistance", "Diseases Resistance [Reproductive]", traits)
traits = gsub("Reproductive Vigor", "Vigor [Reproductive]", traits)
traits = gsub(" Overall", "", traits)
traits = gsub("Vegetative ", "", traits)
traits = gsub("Reproductive ", "", traits)
traits

out = !traits %in% c("Vegetative", "Reproductive", "Flowering")

traitlist = getTraitList(dat, c("_pos", "_neg"), trait.labels = traits)

traits = traits[out]

traitlist = traitlist[out]

pack_index = paste0("package_item_", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

dat[pack_index] = lapply(dat[pack_index], function(x){
  x[x=="Mnanje 2009 (check)"] = "Mnanje 09"
  x[x=="Mangaka 2009 (check)"] = "Mangaka 2009"
  x[x=="NALX -22-18"] = "NALX-22-18"
  x
})

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

for (i in seq_along(genotype_info$genotype)) {
  
  dat[pack_index] = lapply(dat[pack_index], function(x){
    x[ x == genotype_info$genotype[i] ] = genotype_info$final_name[i]
    x
  })
  
  
}

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames


# PlackettLuce rankings
traits

reference = which(traits == "Overall")

dat$gender = ifelse(dat$registration_gender == "Prefer Not To Respond", NA, dat$registration_gender)

dat = dat[!is.na(dat$gender), ]

table(dat$gender)


# Coordinates ####
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

#dat = dat[, !keep]

lon = grep("_longitude", names(lonlat))
lon = lonlat[, lon]

lon = as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of
  # getting the coordinates from the trial, not the point of
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] = "registration_pointofdelivery_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

plot(lonlat)

# latitude higher than 23 is wrong
lonlat$longitude[lonlat$latitude > 23] = NA
lonlat$latitude[lonlat$latitude > 23] = NA

# also lonlat with 0, 0
lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA

plot(lonlat)

plot_map(lonlat, c("longitude", "latitude"),
         map_provider = "OpenStreetMap.Mapnik")




# ....................................
### Backward selection to get the significant traits with p < 0.05 #######
# R = lapply(traitlist, function(x){
#   rank_tricot(data = dat,
#               items = pack_index,
#               input = x$string,
#               validate.rankings = TRUE)
# })
# 
# traits
# 
# true = TRUE
# traits_to_keep = traits
# reference = which(grepl("Overall", traits_to_keep))
# reference
# pval = 0.05
# R_to_backward = R
# iteration = 1
# 
# # kendall = lapply(R[-reference], function(x){
# #   kendallTau(x, R[[reference]])
# # })
# # 
# # kendall = do.call("rbind", kendall)
# # 
# # kendall$trait = traits[-reference]
# # 
# # kendall
# 
# while(true) {
#   
#   cat("Iteration num.", iteration, "\n")
#   
#   rN = kendallTauCorMatrix(R_to_backward)
#   
#   r = rN[[1]]
#   
#   N = rN[[2]]
#   
#   # Test with the full matrix, just to see if it works
#   pr = cor2pcor(r)
#   
#   # Simple way to work with these values is to convert to Pearson's r
#   r_pearson = sin(3.141592654 * r * .5)
#   
#   dimnames(r_pearson) = list(traits_to_keep, traits_to_keep)
#   
#   #calculate pearson correlation P-value
#   ttv = list()
#   ttest_pvalue_matrix = matrix(NA, nrow = length(traits_to_keep), ncol = length(traits_to_keep))
#   
#   # Fill the matrix
#   for(i in seq_along(traits_to_keep)){
#     for (j in seq_along(traits_to_keep)){ #Fills lower triangle only, to avoid calculating the same value twice
#       keep_tt = r_pearson[j,i] *sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
#       ttv = keep_tt
#       keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
#       ttest_pvalue_matrix[j,i] = keep_tt_value
#     }
#   }
#   
#   # get pvalues compared to the reference trait
#   pvalues = ttest_pvalue_matrix[, reference]
#   
#   # check if any pvalue is higher than the threshold
#   true = any(pvalues > pval)
#   
#   if (true) {
#     # remove the trait with higher pvalue
#     max_pvalue = which.max(pvalues)
#     
#     traits_to_keep = traits_to_keep[-max_pvalue]
#     
#     R_to_backward = R_to_backward[-max_pvalue]
#     
#     reference = which(grepl("Overall", traits_to_keep))
#     
#     iteration = iteration + 1
#   }
# }
# 
# cor_backward = data.frame(trait = traits_to_keep,
#                           cor = r_pearson[, reference],
#                           pval = ttest_pvalue_matrix[, reference])
# 
# 
# cor_backward = cor_backward[cor_backward$cor > 0, ]
# 
# drop = grepl("Overall", cor_backward$trait)
# 
# cor_backward[drop, "cor"] = NA
# 
# cor_backward = na.omit(cor_backward)
# 
# write.csv(cor_backward,
#           "output/kendall-partial-correlation-backward-selection.csv",
#           row.names = FALSE)

cor_backward = read.csv("output/kendall-partial-correlation-backward-selection.csv")

cor_backward = cor_backward[order(cor_backward$cor), ]

cor_backward$trait = factor(cor_backward$trait, 
                            levels = unique(cor_backward$trait))

head(cor_backward)

# make a bar plot plot
cor_plot =
  ggplot(data = cor_backward,
         aes(y = cor,
             x = trait,
             fill  = trait)) +
  geom_chicklet(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = rep('#006d2c', nrow(cor_backward))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey10"),
        strip.text.y = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 16, color = "grey10"),
        legend.text = element_text(size = 16, color = "grey10"),
        axis.text = element_text(size = 16, color = "grey10"),
        axis.title = element_text(size = 16, color = "grey10"),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Kendal rank correlation with Overall Preference")

cor_plot

ggsave("output/kendall-tau-partial-correlation-all.pdf",
       plot = cor_plot,
       height = 10,
       width = 12)


# ...............................................
# ...............................................
# Kendall partial correlation ####
traits

# check data availability
cbind(traits, unlist(lapply(traitlist, function(x) sum(x$keep))))

gender = unique(dat$gender)

trait_keep = traits %in% union(cor_backward$trait, "Overall")

traitlist = traitlist[trait_keep]

traits = traits[trait_keep]

reference_trait = which(grepl("Overall", traits))

trait_cor = data.frame()

# for (g in seq_along(gender)) {
#   
#   R = lapply(traitlist, function(x){
#     rank_tricot(data = dat[dat$gender == gender[g], ],
#                 items = pack_index,
#                 input = x$string,
#                 validate.rankings = TRUE)
#   })
#   
#   rN = kendallTauCorMatrix(R)
#   
#   r = rN[[1]]
#   
#   N = rN[[2]]
#   
#   # Test with the full matrix, just to see if it works
#   pr = cor2pcor(r)
#   
#   # Simple way to work with these values is to convert to Pearson's r
#   r_pearson = sin(3.141592654 * r * .5)
#   
#   dimnames(r_pearson) = list(traits , traits)
#   
#   r_pearson
#   
#   #calculate pearson correlation P-value 
#   ttv = list()
#   ttest_pvalue_matrix = matrix(NA, 
#                                nrow = length(traits), 
#                                ncol = length(traits))
#   
#   #Fill the matrix
#   for(i in seq_along(traits)){
#     for (j in seq_along(traits)){ #Fills lower triangle only, to avoid calculating the same value twice
#       
#       keep_tt = r_pearson[j,i] * sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
#       ttv = keep_tt
#       keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
#       ttest_pvalue_matrix[j,i] = keep_tt_value
#     }
#   }
#   
#   cor_g = data.frame(trait = traits, 
#                      cor = r_pearson[, reference_trait],
#                      pval = ttest_pvalue_matrix[, reference_trait],
#                      gender = gender[g])
#   
#   cor_g = cor_g[-reference_trait, ]
#   
#   trait_cor = rbind(trait_cor, cor_g)
#   
# }
# 
# write.csv(trait_cor, "output/kendall-cor-from-rankings-gender-split.csv",
#           row.names = FALSE)

trait_cor = read.csv("output/kendall-cor-from-rankings-gender-split.csv")

table(dat$package_project_name, dat$gender)

table(dat$gender)

ord = order(trait_cor[trait_cor$gender == "Man", "cor"])

lev = trait_cor[trait_cor$gender == "Man", "trait"][ord]

trait_cor$trait = factor(trait_cor$trait, levels = lev)

ngender = table(dat$gender)

trait_cor$gender = ifelse(trait_cor$gender == "Man",
                          paste0(trait_cor$gender, " (n = ", ngender[1], ")"),
                          ifelse(trait_cor$gender == "Woman",
                                 paste0(trait_cor$gender, " (n = ", ngender[2], ")"),
                                 trait_cor$gender))

# make a bar plot plot
cor_plot =
  ggplot(data = trait_cor,
         aes(y = cor,
             x = trait,
             fill = trait)) +
  geom_chicklet(show.legend = FALSE) +
  coord_flip() +
  facet_grid(~ gender) +
  scale_fill_manual(values = rep('#006d2c', length(traits))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey10"),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text = element_text(size = 16, color = "grey10"),
        legend.text = element_text(size = 16, color = "grey10"),
        axis.text = element_text(size = 16, color = "grey10"),
        axis.title = element_text(size = 16, color = "grey10"),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Correlation with Overall Preference")

cor_plot

ggsave("output/kendall-tau-groundnut-gender-split.pdf",
       plot = cor_plot,
       height = 8,
       width = 14)



# ..............................
# ..............................
# Worth map  ####

reference = "Naliendele 09"

# get list of released varieties
released = genotype_info$release_year != "Not released"

released = genotype_info[released, ]

released = released[!duplicated(released$final_name), ]

released = released[order(released$release_year), ]

#released = released[released$genotype %in% sel, ] 

released$final_name = factor(released$final_name,
                             levels = union(reference, released$final_name))

R = lapply(traitlist, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

mod = lapply(R, PlackettLuce)

winprobs = gosset:::.combine_coeffs(mod, 
                                    log = TRUE,
                                    vcov = FALSE,
                                    ref = reference)

unique(rownames(winprobs))

# add name of features
names(winprobs) = traits

sel = rownames(winprobs) %in% released$final_name

winprobs = winprobs[sel, ]

winprobs = data.frame(items = rep(dimnames(winprobs)[[1]], 
                                  times = ncol(winprobs)),
                      labels = rep(dimnames(winprobs)[[2]], 
                                   each = nrow(winprobs)),
                      winprob = as.numeric(unlist(winprobs)))

winprobs$labels = factor(winprobs$labels, levels = traits)

winprobs$items = factor(winprobs$items, levels = levels(released$final_name))

lims = max(abs(winprobs$winprob)) * c(-1, 1)

ggplot(winprobs, 
           aes(x = items, 
               y = labels,
               fill = winprob,
               label = as.character(round(winprob, 2)))) +
  geom_tile() + 
  scale_fill_distiller(palette = "BrBG", 
                       limit = lims,
                       direction = 1,
                       na.value = "white",
                       name = "") +
  theme_minimal() +
  theme(axis.text = element_text(color = "grey20"),
        strip.text.x = element_text(color = "grey20"),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 1, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "", 
       y = "",
       fill = "")


ggsave("output/worth-map-groudnut.pdf",
       plot = last_plot(),
       height = 12,
       width = 12,
       units = "cm")

# get winprobs again 
winprobs = gosset:::.combine_coeffs(mod, 
                                    log = TRUE,
                                    vcov = FALSE)

unique(rownames(winprobs))

# add name of features
names(winprobs) = traits

sel = rownames(winprobs) %in% released$final_name

winprobs = winprobs[sel, ]

winprobs = t(winprobs)

pc = princomp(winprobs, cor = FALSE)

pcplot = factoextra::fviz_pca_biplot(pc, 
                                     repel = TRUE,
                                     title = "")

pcplot

ggsave("output/biplot-groundnut-performance-all-traits.pdf",
       plot = pcplot,
       height = 18,
       width = 18,
       units = "cm")
