library("cluster")
library("factoextra")
library("FactoMineR")
library("caret")
library("tidyverse")

# Load data
dat1 = read.csv("data/rain-covariates.csv")
dat2 = read.csv("data/temp-covariates.csv")

dat = merge(dat1, dat2, by = "block_id", all.x = TRUE)

dat = na.omit(dat)

coords = read.csv("data/planting-dates-longlat.csv")

coords$week = format(as.Date(coords$plantingdate), "%U")

coords$xy = coords$longitude + coords$latitude

plot(coords$xy)

dat = merge(dat, coords[, c("block_id", "week")], by = "block_id", all.x = TRUE)


#............................................
#............................................
# Select relevant socio-economic variables
names(dat)

str(dat)

covar = dat[2:ncol(dat)]

# impute missing values
str(covar)

summary(covar)

covar = covar[-grep("CFD", names(covar))]

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
k_optimal = 5
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

dat = merge(dat, coords[,c("block_id", "longitude", "latitude", "week")], by = "block_id", all.x = TRUE)

write.csv(dat, "data/clim-covariates.csv", row.names = FALSE)

dat
