library("cluster")
library("factoextra")
library("FactoMineR")
library("caret")
library("tidyverse")

# Load data
dat = read.csv("data/mip-tricot-data.csv")

names(dat)

pack_index = paste0("video", 1:3)

table(unlist(dat[pack_index]))

table(unlist(dat[pack_index]), rep(dat$crop, 3))

keep = apply(dat[pack_index],1, function(x) sum(is.na(x))) < 2

dat = dat[keep, ]

dat$gender = ifelse(dat$gender == "Male", "Men", "Women")

#............................................
#............................................
# Select relevant socio-economic variables
names(dat)

str(dat)

sel = c("hh_num", "hh_mem_18", "livestock_name",
        "hh_crop", "gender", "crop", "age", "farm_prevseason",
        "consump_hh", "sold_later","sold_direct", "sec_most_incom")

covar = dat[sel]

# impute missing values
str(covar)

covar[1:ncol(covar)] = lapply(covar[1:ncol(covar)], function(x){
  
  if (class(x) != "character") {
    x[is.na(x)] = median(x, na.rm = TRUE)
  }
  
  if (class(x) == "numeric") {
    x = round(x, 2)
  }
  
  if (class(x) == "character") {
    x = ifelse(is.na(x), "Not specified", x)
    x = as.factor(x)
  }
  
  x
  
})

str(covar)

# Perform FAMD (numeric + factors)
famd_result = FAMD(covar, graph = FALSE)

# plot variables
fviz_famd_var(famd_result)

sel = c("hh_num", "livestock_name",
        "hh_crop", "sold_later",
        "age", "sold_direct")

covar = covar[sel]

# one-hot encoding for categorical variables
encoded = dummyVars("~.", data = covar, fullRank = TRUE)
transformed = predict(encoded, covar)
scaled = scale(transformed) # Scale numerical values

# Determine optimal number of clusters using the Elbow method
wss = sapply(2:10, function(k) {
  kmeans(scaled, centers = k, nstart = 10)$tot.withinss
})

# plot elbow curve
plot(2:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "N clusters (k)", 
     ylab = "Total within sum of squares")

# apply K-Means clustering with optimal k (e.g., 4)
k_optimal = 4
set.seed(1551)
kmeans_result = kmeans(scaled, centers = k_optimal, nstart = 10)
covar$cluster = as.factor(kmeans_result$cluster)

table(covar$cluster)

pca = prcomp(scaled)
pca_data = as.data.frame(pca$x[, 1:2])
pca_data$cluster = covar$cluster


ggplot(pca_data, aes(x = PC1, y = PC2, color = covar$cluster)) +
  geom_point(size = 2) +
  labs(title = "",
       x = "PC1",
       y = "PC2") +
  theme_minimal(base_family = "Helvetica") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.margin = margin(rep(10, 4)))


ggsave("output/cluster-pca.pdf",
       width = 13,
       height = 13,
       units = "cm")

dat$cluster = covar$cluster

table(dat$gender, covar$cluster)

table(dat$country, covar$cluster)

table(dat$crop, covar$cluster)

table(unlist(dat[pack_index]), rep(covar$cluster, 3))

write.csv(dat, "data/mip-tricot-data-with-clusters.csv", row.names = FALSE)


sel = c("hh_num", "hh_mem_18", "livestock_name",
        "hh_crop", "gender", "age", "farm_prevseason",
        "consump_hh", "sold_later","sold_direct")


summdat = 
  dat %>% 
  group_by(cluster) %>% 
  summarise(men = (sum(gender == "Men") / length(gender)) * 100,
            farmer_age = mean(age),
            #farmer_age_sd = sd(age),
            hh_number = mean(hh_num),
            #hh_number_sd = sd(hh_num),
            hh_men_18 = mean(hh_mem_18),
            #hh_men_18_sd = sd(hh_mem_18),
            n_livestock = mean(livestock_name),
            #n_livestock_sd = sd(livestock_name),
            n_crop = mean(hh_crop),
            #n_crop_sd = sd(hh_crop),
            farm_area = mean(farm_prevseason),
            #farm_area_sd = sd(farm_prevseason),
            hh_consumpion = mean(consump_hh),
            hh_sold_direct = mean(sold_direct),
            hh_sold_later = mean(sold_later)) 


summdat
summdat[2:ncol(summdat)] = lapply(summdat[2:ncol(summdat)], function(x) round(x, 2))

write.csv(t(summdat), "output/cluster-summaries.csv", row.names = TRUE)




