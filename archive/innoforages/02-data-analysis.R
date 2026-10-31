library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("sf")
library("geodata")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("patchwork")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

load("data/data.rda")

# weights = read.csv("data/trait-weights.csv")
# 
# weights$w = weights$weight / sum(weights$weight)
# 
# unique(dat$project_code)

dat = dat[dat$project_code == "SS25", ]

dat$registration_plantingdate = gsub("2020|2022|2024|2023|2026","2025", 
                                     dat$registration_plantingdate)

dat$registration_submitted_date

sort(dat$registration_plantingdate)

boxplot(as.Date(dat$registration_plantingdate) ~ dat$project_code)

# Trial map ######
# make the trial map
coords = ClimMobTools:::.get_trial_coordinates(dat, return = "coordinates", seed = 12)

# extract country ISO2 code
ctry = dat$project_country[1]

# download GADM map using geodata
adm = geodata::gadm(country = ctry, level = 1, path = "data/")

adm = st_as_sf(adm)

xy = st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)

plot(xy)

trial_map = 
  ggplot() +
  geom_sf(data = adm, fill = "#ffffe5", color = "black", alpha = 1) +
  geom_sf(data = xy, color = "blue", size = 1.5, shape = 21, fill = "red", stroke = 1) +
  theme_minimal() +
  theme(legend.title = element_blank(),
                            legend.position = "bottom") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3)

ggsave(paste0("output/south/trial-map-", tag, ".pdf"),
       plot = trial_map,
       width = 15,
       height = 15,
       units = "cm")


# # Main country (Norway, admin 1)
# no_adm1 =
#   geodata::gadm(country = "NOR", level = 1, path = "data/") |>
#   st_as_sf()
# 
# # Neighbouring countries as simple country polygons (admin 0)
# neighbours_iso3 = c("SWE", "FIN", "DNK")
# 
# neigh_list = lapply(neighbours_iso3, function(cc) {
#   geodata::gadm(country = cc, level = 0, path = "data/") |>
#     st_as_sf()
# })
# 
# neigh = do.call(rbind, neigh_list)
# 
# # Expanded bounding box so neighbours appear but keep focus on Norway
# bb = st_bbox(no_adm1)
# bb_sfc = st_as_sfc(bb)
# 
# trial_map =
#   ggplot() +
#   # Neighbour countries (background)
#   geom_sf(data = neigh,
#           color = "grey70",
#           fill = "grey95",
#           linewidth = 0.2) +
#   # Norway (highlight)
#   geom_sf(data = no_adm1,
#           fill = "#ffffe5",
#           color = "black",
#           linewidth = 0.4) +
#   # Trial points
#   geom_sf(data = xy,
#           color = "blue",
#           size = 1.5,
#           shape = 21,
#           fill = "red",
#           stroke = 1) +
#   coord_sf(xlim = xlim, ylim = ylim) +
#   theme_minimal() +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "bottom"
#   ) +
#   annotation_north_arrow(
#     location = "tl",
#     which_north = "true",
#     style = north_arrow_fancy_orienteering
#   ) +
#   annotation_scale(
#     location = "br",
#     width_hint = 0.3
#   )
# 
# ggsave(paste0("output/south/trial-map-option2", tag, ".pdf"),
#        plot = trial_map,
#        width = 25,
#        height = 15,
#        units = "cm")

#..........................................................
#..........................................................
#..........................................................
# Socioeco ####
dat$registration_gender = as.factor(dat$registration_gender)

ggplot(dat, aes(y = registration_age, 
                x = registration_gender)) +
  geom_boxplot() +
  labs(y = "Alder",
       x = "") +
  theme_minimal()

ggsave("output/south/age-gender.pdf",
       plot = last_plot(),
       width = 12,
       height = 12,
       units = "cm")

#..........................................................
#..........................................................
#..........................................................
# Organize traits ####
# Assess trait rankings 
pack_index = paste0("package_item_", LETTERS[1:3])

table(unlist(dat[pack_index]))

#s = apply(dat[pack_index], 2, function(x) grepl("Blanding 12|Blanding 9", x))

#s = rowSums(s) > 0

#x = dat[s ,c(2, 6:9, grep("_pos|_neg", colnames(dat)))]

# create a representation of the trial network
net = dat[, pack_index]

net$best = "A"
net$worst = "C"

r = rank_tricot(net, pack_index, c("best", "worst"))

pdf(file = paste0("output/south/trial-network-", tag, ".pdf"),
    width = 7,
    height = 7)
plot(network(r))
dev.off()

# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

# check number of entries
keep = unlist(lapply(trait_list, function(x){
  sum(x$keep)
}))

keep = keep > 15

trait_list = trait_list[keep]

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

traits

traitlabels = c("Jorddekke", "Ugras", "Vinteroverlevelse", "Vårvekst")

traitlabels

dat[dat=="Not observed"] = NA

itemnames = unique(sort(unlist(dat[pack_index])))

itemnames = union(union(itemnames[1], itemnames[5:12]), itemnames)

#View(dat[,c(pack_index, trait_list[[3]]$string)])

#..........................................................
#..........................................................
#..........................................................
# handle ties
# PlackettLuce rankings ####
R =  lapply(trait_list, function(x){
  
  d = fill_tie(dat, cols = x$string, seed = 1706)
  
  rank_tricot(items = pack_index,
              input = x$string,
              data = d,
              validate.rankings = TRUE)
})

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
anov = lapply(R, function(x){
  a = anova(PlackettLuce(x))
  a[2, ]
})

anov = do.call("rbind", anov)

anov[,1] = traitlabels

anov

write.csv(anov, 
          paste0("output/south/anova-", tag, ".csv"))

# ........................................
# ........................................
# Calculate PL model  ####
ref = "Blanding 2"

traitorder = rev(traitlabels)

mod = lapply(R, function(x){
  PlackettLuce(x)
})

worth_map(mod, labels = traitlabels, labels.order = traitorder)

worth = 
  worth_map(mod, labels = traitlabels, labels.order = traitorder) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0)) 

ggsave(paste0("output/south/worth-map-trait-performance.pdf"),
       plot = worth,
       height = 8,
       width = 16,
       units = "cm")

# # test the difference in rankings across regions
# split_var = dat$project_code
# splits = unique(split_var)
# 
# table(split_var)
# 
# llr = lapply(R, function(x){
#   likelihood_ratio(x, split = split_var)
# })
# 
# llr = do.call(rbind, llr)
# 
# llr$trait = traitlabels
# 
# write.csv(llr, "output/south/likelihood-ratio.csv", row.names = FALSE)
# 
# # now split
# mod1 = lapply(R, function(x){
#   PlackettLuce(x[split_var == splits[1]])
# })
# 
# mod2 = lapply(R, function(x){
#   PlackettLuce(x[split_var == splits[2]])
# })
# 
# worth1 = 
#   worth_map(mod1, 
#             labels = traitlabels, 
#             labels.order = traitorder) +
#   labs(x = "", y = "", title = paste("Sør",  splits[1])) +
#   theme(axis.text.y = element_text(vjust = 0.5, 
#                                    hjust = 1, 
#                                    angle = 0)) 
# 
# worth2 = 
#   worth_map(mod2, 
#             labels = traitlabels, 
#             labels.order = traitorder)  +
#   labs(x = "", y = "", title = paste("Nord",  splits[2])) +
#   theme(axis.text.y = element_text(vjust = 0.5, 
#                                    hjust = 1, 
#                                    angle = 0)) 
# 
# worth = worth1 / worth2
# 
# ggsave(paste0("output/south/worth-map-trait-performance-region.pdf"),
#        plot = worth,
#        height = 12,
#        width = 16,
#        units = "cm")
# 
# # ........................................
# ........................................
# Apply selection index  ####
npoints = 50
coeffs = lapply(mod, function(x){
  gosset::resample(x, bootstrap = TRUE, seed = 1424, n1 = npoints, log = FALSE) 
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = traitlabels

# for (w in seq_along(weights$trait)) {
#    coeffs[, weights$trait[w]] = coeffs[,weights$trait[w]] * weights$w[w]
# }

coeffs = cbind(index = rowSums(coeffs[2:ncol(coeffs)]),
               coeffs) 

lvls = unique(coeffs$item[order(coeffs$index)])

coeffs$item = factor(coeffs$item, levels = lvls)

selection = 
  ggplot(coeffs, aes(y = item, x = index)) +
  geom_boxplot(alpha = 0.2) +
  theme_minimal() +
  labs(y = "", x = "Koeffisient") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(colour = "grey20"))

selection

ggsave(plot = selection, 
       paste0("output/south/selecion-index.pdf"),
       width = 10,
       height = 10,
       units = "cm")

# Reliability
rel = lapply(mod, function(x){
  reliability(x, ref = ref)
})

for(i in seq_along(traitlabels)){
  rel[[i]]$trait = traitlabels[i]
}

rel = do.call("rbind", rel)

rel$trait = factor(rel$trait, levels = traitlabels)

rel$improvement = round((rel$reliability / 0.5 - 1), 2)

rel = rel[order(rel$improvement), ]

rel$item = factor(rel$item, levels = unique(rel$item))

rel = rel[!rel$item %in% ref, ]

rel$item = factor(rel$item, levels = paste0("Blanding ", 1:12))

reliability_plot =
  ggplot(data = rel,
         aes(x = improvement, 
             y = trait,
             fill = "white")) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = "dodge", 
           show.legend = FALSE) +
  scale_fill_manual(values = "#5aae61") +
  facet_wrap(~ item, strip.position = "bottom", ncol = 3) +
  geom_vline(xintercept = 0,
             colour = "grey40",
             linewidth = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 5)) +
  labs(x = paste("Sannsynlighet for å overprestere", ref),
       y = "")


reliability_plot

ggsave(plot = reliability_plot,
       paste0("output/south/reliability.pdf"),
       width = 15,
       height = 14,
       units = "cm")

for(i in seq_along(traitlabels)) {
  p = plot_logworth(mod[[i]], ref = ref) +
    labs(title = traitlabels[i]) +
    theme(strip.text = element_blank())
  
  ggsave(plot = p,
         paste0("output/south/logworth-", traitlabels[i],".pdf"),
         width = 13,
         height = 10,
         units = "cm")
}


