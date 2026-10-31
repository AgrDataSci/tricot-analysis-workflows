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

pld = cbind(G = group(R, index = 1:length(R)),
            dat[c("hh_num", "hh_mem_18", "livestock_name",
                  "hh_crop", "age", "farm_prevseason",
                  "consump_hh", "sold_later","sold_direct",
                  "gender", "cluster")],
            intercept = 1)

pld$gender = as.factor(pld$gender)

pld$cluster = as.factor(pld$cluster)


head(pld)

n_k = 10

# model with no covariates
# intercept-only model
cv0 = crossvalidation(G ~ intercept, 
                      data = pld, 
                      alpha = 0.01, 
                      seed = 1159, 
                      k = n_k)

# model with gender 
# Gender-based model
cv1 = crossvalidation(G ~ gender, 
                      data = pld, 
                      alpha = 0.01, 
                      seed = 1159, 
                      k = n_k)

# model randomly picking any of the key covariates
# RPART-model 
cv2 = crossvalidation(G ~ cluster + hh_crop + hh_num + hh_mem_18 + livestock_name + age +
                        farm_prevseason + consump_hh + sold_later + sold_direct, 
                      data = pld, 
                      alpha = 0.01,
                      seed = 1159,
                      k = n_k)

lapply(cv2$raw$models, node_labels)

lapply(cv2$raw$models, top_n)

# model with the market segments
cv3 = crossvalidation(G ~ cluster, 
                      data = pld, 
                      alpha = 0.01,
                      seed = 1159,
                      k = n_k)

cv0
cv1
cv2
cv3

# make dataframe for the boxplot
models = c("Intercept-only", "Gender-based", "RPART-based", "Cluster-based")
models = rep(models, each = n_k)

models = data.frame(model = factor(models, levels = unique(models)),
                    logLik = c(cv0$raw$estimators$logLik,
                               cv1$raw$estimators$logLik,
                               cv2$raw$estimators$logLik,
                               cv3$raw$estimators$logLik))


ggplot(models, aes(x = logLik,
                   y = model)) +
  geom_boxplot() +
  theme_minimal(base_family = "Helvetica", base_size = 13) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        plot.title = element_text(hjust = 0.5, size = 13))

