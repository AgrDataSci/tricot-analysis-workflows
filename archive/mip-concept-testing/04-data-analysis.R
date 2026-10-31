library("tidyverse")
library("patchwork")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("ggalt")

# Load data
dat = read.csv("data/mip-tricot-data-with-clusters.csv")

names(dat)

pack_index = paste0("video", 1:3)

table(unlist(dat[pack_index]))

table(unlist(dat[pack_index]), rep(dat$crop, 3))

keep = apply(dat[pack_index],1, function(x) sum(is.na(x))) < 2

dat = dat[keep, ]

dat[pack_index] = lapply(dat[pack_index], function(x){
  x[x == "Low labor"] = "Low labour"
  x
})

table(dat$gender, dat$cluster)

table(dat$country, dat$cluster)

table(dat$crop, dat$cluster)

table(unlist(dat[pack_index]), rep(dat$cluster, 3))

#............................................
#............................................
# PlackettLuce ####
itemnames = unique(unlist(dat[pack_index]))

R = rank_tricot(items = pack_index, 
                input = c("best", "worst"),
                data = dat,
                validate.rankings = TRUE, 
                group = FALSE)


R = R[, !grepl("Feed and food", colnames(R))]

p =
  plot(PlackettLuce(R), log = FALSE) +
  theme_minimal(base_family = "Helvetica", base_size = 13) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        strip.text.x = ggplot2::element_blank(),
        plot.margin = margin(rep(10, 4))) +
  labs(x = "Probability of outperforming (worth)",
       y = "Breeding concept")

p
  
ggsave("output/pl-coefficients-all-data.pdf",
       plot = p, 
       width = 15,
       height = 10,
       units = "cm")


# ........................................
# ........................................
# Prepare baseline data ####
# visualise using dumbbell plots putting the coefficients from 
# all data as the baseline
coeff_all = qvcalc(itempar(PlackettLuce(R), log = FALSE))$qvframe
coeff_all$concept = rownames(coeff_all)
names(coeff_all)[names(coeff_all) == "estimate"] = "all"
coeff_all = coeff_all[,c("concept", "all")]

# ........................................
# ........................................
# Split the data by gender ####
split = dat$gender

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
llr = likelihood_ratio(R, split = split)

llr

write.csv(llr, 
          "output/likelihood-ratio-gender.csv")

G = list()
for(i in seq_along(group_split)) {
  G[[i]] = R[split == names(group_split)[i], ]
}

mod = lapply(G, PlackettLuce)

coeff_split = lapply(mod, function(x){
  coeff = qvcalc(itempar(x, log = FALSE))$qvframe
  coeff$concept = rownames(coeff)
  coeff = merge(coeff_all,
                coeff[c("estimate", "concept")], 
                by = "concept")
  coeff$concept = factor(coeff$concept, levels = rev(sort(coeff$concept)))
  coeff
})

coeff_split

do.call(cbind, lapply(coeff_split, function(x){
  as.character(x$concept[order(x$estimate * -1)[1:5]])
}))

# one row per concept
p = list()

for(i in seq_along(coeff_split)) {
  p[[i]] = 
    ggplot(coeff_split[[i]]) +
    geom_dumbbell(aes(y = concept,
                      x = all, 
                      xend = estimate),
                  color = "grey", 
                  size = 1.5,
                  colour_x = "#0072B2", 
                  colour_xend = "#D55E00") +
    theme_minimal(base_family = "Helvetica", base_size = 13) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", size = 0.3),
          axis.ticks = element_line(color = "black", size = 0.3),
          plot.title = element_text(hjust = 0.5, size = 13)) +
    scale_x_continuous(expand = c(0,0), 
                       limits = c(0, 0.25), 
                       breaks = seq(0, 0.40, 0.05)) +
    labs(title = names(group_split)[i],
         x = "",
         y = "Breeding concept")
  
  if(i == 2 | i == 4){
    p[[i]] =
      p[[i]] +
      theme(axis.text.y = element_blank()) +
      labs(y = "")
  }
  
}


p = (p[[1]] / p[[2]]) + 
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)),
                  tag_levels = 'a',
                  caption = "Probability of outperforming (worth)") &
  theme(plot.caption = element_text(hjust = 0.6, size = 12))

p

ggsave("output/pl-coefficients-gender.pdf",
       plot = p,
       width = 21,
       height = 10,
       units = "cm")


# ........................................
# ........................................
# Split the data by clusters

# Cluster 1: “Farming for resilience”
# Meaning: Farmers focused on securing stable, long-term returns despite climate or market risks.
# Cluster 2: “Farming for family nutrition”
# Meaning: Farmers primarily focused on feeding their families with diverse, nutritious crops.
# Cluster 3: “Farming for market access”
# Meaning: Farmers farming primarily to sell quickly and respond to market demands.
# Cluster 4: “Farming for family security”
# Meaning: Farmers producing for large families, balancing home consumption and some sales to ensure livelihood stability.

lvls = c("Farming for resilience", "Farming for family nutrition",
         "Farming for market access", "Farming for family security")

dat$cluster_desc = ifelse(dat$cluster == 1, lvls[1],
                          ifelse(dat$cluster == 2, lvls[2],
                                 ifelse(dat$cluster == 3, lvls[3],
                                        ifelse(dat$cluster == 4, lvls[4],
                                               NA))))
dat$cluster_desc = factor(dat$cluster_desc, levels = lvls)

split = dat$cluster_desc

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
llr = likelihood_ratio(R, split = split)

llr

write.csv(llr, 
          "output/likelihood-ratio-clusters.csv")

G = list()
for(i in seq_along(group_split)) {
  G[[i]] = R[split == names(group_split)[i], ]
}

mod = lapply(G, PlackettLuce)

coeff_split = lapply(mod, function(x){
  coeff = qvcalc(itempar(x, log = FALSE))$qvframe
  coeff$concept = rownames(coeff)
  coeff = merge(coeff_all,
                coeff[c("estimate", "concept")], 
                by = "concept")
  coeff$concept = factor(coeff$concept, levels = rev(sort(coeff$concept)))
  coeff
})

do.call(cbind, lapply(coeff_split, function(x){
  paste(as.character(x$concept[order(x$estimate * -1)[1:5]]), collapse = ", ")
}))

# one row per concept
p = list()

for(i in seq_along(coeff_split)) {
  p[[i]] = 
    ggplot(coeff_split[[i]]) +
    geom_dumbbell(aes(y = concept,
                      x = all, 
                      xend = estimate),
                  color = "grey", 
                  size = 1.5,
                  colour_x = "#0072B2", 
                  colour_xend = "#D55E00") +
    theme_minimal(base_family = "Helvetica", base_size = 13) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", size = 0.3),
          axis.ticks = element_line(color = "black", size = 0.3),
          plot.title = element_text(hjust = 0.5, size = 13)) +
    scale_x_continuous(expand = c(0,0), 
                       limits = c(0, 0.30), 
                       breaks = seq(0, 0.30, 0.05)) +
    labs(title = names(group_split)[i],
         x = "",
         y = "Breeding concept")
  
  if(i == 2 | i == 4){
    p[[i]] = 
      p[[i]] +
      theme(axis.text.y = element_blank()) +
      labs(y = "")
  }
  
  if(i == 3){
    p[[i]] = 
      p[[i]] +
      labs(y = "")
  }
  
}


p = p[[1]] + p[[2]] + p[[3]] + p[[4]] +  
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)),
                  caption = "Probability of outperforming (worth)",
                  tag_levels = 'a') &
  theme(plot.caption = element_text(hjust = 0.6, size = 12))

p

ggsave("output/pl-coefficients-clusters.pdf",
       plot = p,
       width = 21,
       height = 20,
       units = "cm")


ranks = do.call(rbind, lapply(coeff_split, function(x){
  data.frame(concept = as.character(x$concept[order(x$estimate * -1)[1:5]]),
             rank = 1:5)
}))

ranks$cluster = factor(rep(lvls, each = 5), levels = lvls)

ranks$concept = factor(ranks$concept, levels = rev(c("Resilient",
                                                 "High-yielding hybrid",
                                                 "Intercropping",
                                                 "Drought escape",
                                                 "Low labour",
                                                 "Home use",
                                                 "High nutritional")))

ranks

ggplot(ranks, mapping = aes(y = concept, x = rank,
                            colour = cluster, group = cluster)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_family = "Helvetica", base_size = 13) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        plot.title = element_text(hjust = 0.5, size = 13),
        legend.title = element_blank(),
        legend.position = c(0.17, 0.17)) +
  labs(title = "",
       x = "Rank (top-5)",
       y = "Breeding concept")


ggsave("output/ranking-shift.pdf",
       plot = last_plot(),
       width = 21,
       height = 10,
       units = "cm")
