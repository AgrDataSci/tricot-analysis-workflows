library("tidyverse")
library("ggplot2")
library("ggrepel")
library("janitor")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("readxl")
library("geodata")
library("sf")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")
# Load data
dat = read.csv("data/groundnut-tricot-tanzania.csv")

table(dat$package_project_name)

dat = dat[dat$package_project_name != "gnuttz2025", ]

#dat = dat[dat$package_project_name != "gnuttZ2324", ]

# data with kendall tau backward selection 
trait_cor = read.csv("output/kendall-partial-correlation-backward-selection.csv")

trait_cor[11, ] = c("Overall", 1, 0)

# name of adm zones in tanzania
adm = gadm("TZ", 1, "data")

adm = st_as_sf(adm)

# add agroecological zones
adm$region = ifelse(adm$NAME_1 == "Kagera" | adm$NAME_1 == "Mwanza" | adm$NAME_1 == "Mara" , "Lake",
                    ifelse(adm$NAME_1 == "Kigoma" | adm$NAME_1 ==  "Shinyanga" | adm$NAME_1 == "Tabora" | adm$NAME_1 == "Geita"  | adm$NAME_1 == "Simiyu", "Western",
                           ifelse(adm$NAME_1 == "Arusha" | adm$NAME_1 == "Kilimanjaro" | adm$NAME_1 == "Manyara" | adm$NAME_1 == "Tanga", "Northern",
                                  ifelse(adm$NAME_1 == "Singida" | adm$NAME_1 =="Dodoma", "Central",
                                         ifelse(adm$NAME_1 == "Rukwa" | adm$NAME_1 == "Mbeya" | adm$NAME_1 == "Iringa" | adm$NAME_1 == "Katavi" | adm$NAME_1 == "Songwe" | adm$NAME_1 == "Njombe", "Southern Highlands",
                                                ifelse(adm$NAME_1 == "Lindi" | adm$NAME_1 == "Mtwara" | adm$NAME_1 == "Ruvuma", "Southern",
                                                       "Eastern"))))))

coords = st_as_sf(dat[,c("longitude", "latitude")], coords = c("longitude", "latitude"), crs = 4326)

adm_info = st_join(coords, adm, join = st_intersects)

dat$agzone = adm_info$region

table(dat$agzone)

#............................................
#............................................
# PlackettLuce ####
names(dat)
pack_index = paste0("package_item_", letters[1:3])

table(unlist(dat[pack_index]))

#..........................................................
#..........................................................
# Organize traits ####
# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

index = which(traitlabels %in% trait_cor$trait)

# keep traits selected in the previous script 
# from backward selection
trait_list = getTraitList(dat,
                          pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)

traitlabels[index]

traitlabels = traitlabels[index]

trait_list = trait_list[index]

traits = traits[index]

table(unlist(dat[pack_index]))

itemnames = unique(sort(unlist(dat[pack_index])))

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce with aggregated rankings ####
R = list()

unitrait = unique(trait_cor$trait)

indicesbase = 1:nrow(dat)

for(i in seq_along(unitrait)) {
  
  print(unitrait[i])
  
  index = grep(unitrait[i], traitlabels)
  
  RG = lapply(trait_list[index], function(x){
    rank_tricot(items = pack_index,
                input = x$string,
                data = dat,
                validate.rankings = TRUE)
  })
  
  RG = do.call("rbind", RG)
  
  RG = group(RG, index = rep(1:nrow(dat), length(index)))
  
  R[[i]] = RG
  
}

R

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
traitlabels = unitrait

ov = grep("Overall", traitlabels)

pdf(file = "output/trial-network.pdf",
    width = 9,
    height = 9)
plot(network(R[[ov]]))
dev.off()

anov = lapply(R, function(x){
  a = anova(PlackettLuce(x))
  a[2, ]
})

anov = do.call("rbind", anov)

anov[,1] = traitlabels

anov

write.csv(anov, 
          "output/anova.csv")

# remove traits with no significant ANOVA 
sel = anov[anov$`Pr(>Chisq)` < 0.1, 1]

# also remove "Marketability" since it is an "unbreedible trait"
sel = sel[-grep("Marketability", sel)]

index = which(traitlabels %in% sel)

traitlabels[index]

traitlabels = traitlabels[index]

trait_list = trait_list[index]

traits = traits[index]

R = R[index]

trait_cor = trait_cor[index, ]

# ........................................
# ........................................
# Define a split to the data  ####
split = dat$agzone

group_split = table(split)

trait_available = lapply(trait_list, function(x){
  z = data.frame(N = x$keep, split = split)
  table(z$N, z$split)[2,]
})

trait_available = do.call("rbind", trait_available)

trait_available = cbind(trait_available, N = rowSums(trait_available))

trait_available = as.data.frame(trait_available)

trait_available$trait = traitlabels

trait_available

# ........................................
# ........................................
# Calculate PL model  ####
# define order of traits 
index = order(trait_cor$cor)

mod = lapply(R, PlackettLuce)

worth = 
  worth_map(mod, 
            labels = traitlabels,
            labels.order = rev(traitlabels[index])) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0),
        text = element_text(size = 15)) 


worth

ggsave("output/worth-map-trait-performance.pdf",
       plot = worth,
       height = 15,
       width = 35,
       units = "cm")



# Likelihood ratio will check whether the ranks are significantly distinct 
# from each gender
llr = lapply(R, function(x){
  likelihood_ratio(x, split = split)
})

llr = do.call("rbind", llr)

llr$trait = traitlabels

llr

write.csv(llr, 
          "output/likelihood-ratio-clusters.csv")


index = grep("Overall", traitlabels)

# PCA and worth map
reference_trait = grep("Overall", traitlabels)
traits = traitlabels
pca_plot = list()
top3 = data.frame()
sel_items = c("ICGIL 17105", "ICGIL 17120", "ICGIL 17123", "ICGV-SM 02724", "ICGV-SM 03707",
              "ICGV-SM 05534", "ICGV-SM 10014", "ICGV-SM 16514", "ICGV-SM 16524", "ICGV-SM 16630",
              "ICGV-SM 16645", "ICGV-SM 17533", "Local", "Mangaka 2009", "Mnanje 09")

for (i in seq_along(group_split)) {

  # run the model
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })

  mod_i = lapply(R_subset, PlackettLuce)

  if(i == 1) {
    itemsorder = names(rev(sort(coefficients(mod_i[[reference_trait]], log = FALSE))))
  }

  # selection index
  coeffs = lapply(mod_i, function(x){
    gosset::resample(x, bootstrap = TRUE,
             seed = 1424,
             n1 = 10,
             log = TRUE)
  })

  coeffs = do.call(cbind, coeffs)

  rmv = which(names(coeffs) == "item")[-1]

  coeffs = coeffs[-rmv]

  names(coeffs)[-1] = traitlabels
  
  coeffs = coeffs[coeffs$item %in% sel_items, ]
  
  # PCA
  pc = princomp(coeffs[, -1], cor = TRUE)

  pcplot = plot_pca(pc, scale = 4) +
    labs(title = paste0("(", LETTERS[i], ") ",
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i]))) 
    #theme(text = element_text(size = 10)) 

  pca_plot[[i]] = pcplot

  ggsave(paste0("output/pca-trait-performance-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = pca_plot[[i]],
         height = 20,
         width = 20,
         units = "cm")
  
  coeffs = lapply(mod_i, function(x){
    r = rank(coefficients(x, log = F)[sel_items] * -1)
    r = sort(r)[1:3]
    data.frame(items = names(r), rank = as.vector(r))
  })
  
  coeffs = do.call("rbind", coeffs)
  
  coeffs$trait = rep(traitlabels, each = 3)
  
  coeffs$az = names(group_split)[i]
  
  top3 = rbind(top3, coeffs)

}

p = 
  pca_plot[[1]] + pca_plot[[2]] + pca_plot[[3]] +
  pca_plot[[4]] + pca_plot[[5]] + pca_plot[[6]] +
  plot_layout(ncol = 3)

ggsave("output/pca-trait-performance-clusters.pdf",
       plot = p,
       height = 20,
       width = 30,
       units = "cm")

# # ....................................
# # Selection index using Kendall tau as weights ####
kendall = trait_cor
kendall$cor = as.numeric(kendall$cor)

kendall$weights = kendall$cor / sum(kendall$cor)

# identify the "Overall" trait and apply the penalty
kendall$weights[reference_trait] = log(1 + kendall$weights[reference_trait])

# re-normalize weights to ensure they sum to 1
kendall$weights = kendall$weights / sum(kendall$weights)

kendall

weights = kendall$weights

select = data.frame()

for (i in seq_along(group_split)) {
  
  # run the model
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  # extract the coefficients
  coeffs = lapply(mod_i, function(x) {
    z = coefficients(x, log = FALSE)[sel_items]
    z = z / sum(z)
  })
  
  coeffs = do.call("rbind", coeffs)
  
  # apply the weights within the coefficients
  coeffs = apply(coeffs, 2, function(x) {x * weights})
  
  coeffs = colSums(coeffs)
  
  # put it in a data.frame
  select_i = data.frame(item = names(coeffs),
                        slice = names(group_split)[i],
                        score = as.vector(coeffs))
  
  select = rbind(select, select_i)
  
  
}

sel_index = 
  ggplot(data = select,
       aes(x = score, 
           y = item, 
           fill = "green")) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = "dodge") +
  scale_fill_manual(values = c("#4575b4")) +
  facet_wrap(~ slice) +
  #scale_fill_brewer(palette = "Accent") +
  geom_vline(xintercept = mean(select$score),
             colour = "red",
             linewidth = 0.5) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  labs(x = "Selection score",
       y = "")


sel_index

ggsave("output/selection-index.pdf",
       plot = sel_index,
       width = 15,
       height = 15,
       units = "cm")

kendall$pval = as.numeric(kendall$pval)

write.csv(kendall, "output/kendall-with-weights.csv", row.names = FALSE)




# # ....................................
# # Kendall tau inside the nodes ####
# kendall = data.frame()
# k_plot = list()
# t_labels = unlist(lapply(trait_list, function(x) x$trait_label))
# k = 10
# reference_trait = grep("Overall", t_labels)
# 
# for (i in seq_along(group_split)) {
#   
#   # run kendall correlation among traits for this group
#   R_traits = lapply(trait_list, function(x){
#     rank_tricot(dat[split == names(group_split[i]), ], 
#                 pack_index, 
#                 x$string, 
#                 validate.rankings = TRUE)
#   })
#   
#   
#   n = nrow(R_traits[[ov]])
#   set.seed(1111)
#   folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
#   
#   # run over folds
#   for (j in seq_len(k)) {
#     
#     kt = lapply(R_traits[-ov], function(x){
#       kendallTau(x[folds != j, ],
#                  R_traits[[ov]][folds != j, ])
#     })
#     
#     kt = do.call("rbind", kt)
#     
#     kt$trait = t_labels[-ov]
#     
#     kt$group = names(group_split)[i]
#     
#     kendall = rbind(kendall, kt)
#     
#   }
#   
#   if(i == 1) lvls = unique(kendall$trait[order(kendall$kendallTau)])
#   
#   kendall$trait = factor(kendall$trait, levels = lvls)
#   
#   kendall_plot = 
#     ggplot(kendall[kendall$group == names(group_split[i]), ],
#            aes(y = trait, x = kendallTau, 
#                fill = group, color = group)) +
#     geom_boxplot(alpha = 0.8, show.legend = FALSE) +
#     theme_minimal() +
#     scale_fill_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
#     scale_color_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
#     xlim(0, 0.6) +
#     labs(y = "", 
#          x = "Kendall correlation with Overall Preference",
#          title = paste0("(", LETTERS[i], ") ", 
#                         names(group_split[i]),
#                         ", n = ",
#                         as.integer(group_split[i]))) +
#     theme(legend.title = element_blank(),
#           legend.position = "bottom",
#           text = element_text(colour = "grey10", size = 16),
#           title = element_text(size = 0))
#   
#   # write individual plots
#   ggsave(paste0("output/kendall-tau-", i, "-", tolower(names(group_split)[i]), ".pdf"),
#          plot = kendall_plot,
#          height = 15,
#          width = 15,
#          units = "cm")
#   
#   k_plot[[i]] =
#     kendall_plot + 
#     labs(y = "", 
#          x = "",
#          title = paste0("(", LETTERS[i], ") ", 
#                         names(group_split[i]),
#                         ", n = ",
#                         as.integer(group_split[i])))
#   
#   if(i > 1) {
#     k_plot[[i]] =
#       k_plot[[i]] +
#       theme(axis.text.y = element_blank())
#     
#   }
#   
# }
# 
# kendall_plot = 
#   ggplot(kendall, aes(y = trait, x = kendallTau, 
#                       fill = group, color = group)) +
#   geom_boxplot(alpha = 0.8) +
#   theme_minimal() +
#   scale_fill_manual(values = c("grey20","#225ea8", "#e31a1c")) +
#   scale_color_manual(values = c("grey20","#225ea8", "#e31a1c")) +
#   labs(y = "", x = "Kendall correlation with Choice for Replanting") +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         text = element_text(colour = "grey20", size = 15))
# 
# kendall_plot
# 
# ggsave("output/kendall-correlation.pdf",
#        plot = kendall_plot,
#        height = 30,
#        width = 25,
#        units = "cm")
# 
# 
# p = k_plot[[1]] + k_plot[[2]] + k_plot[[3]]
# 
# p
# 
# ggsave("output/kendall-correlation-panels.pdf",
#        plot = p,
#        height = 15,
#        width = 30,
#        units = "cm")
# 
# 
# 
# coords$cluster = covar$cluster
# 
# 
# plot(coords)
# 
# 
