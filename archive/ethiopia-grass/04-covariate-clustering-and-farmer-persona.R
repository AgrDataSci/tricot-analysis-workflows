# .......................................
# .......................................
library("cluster")
library("PlackettLuce")
library("tidyverse")
library("factoextra")
library("readxl")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
# list.files("data")
# 
# dat = read_excel("data/socioeconomic-data-ethiopiagrass.xlsx")
# 
# # Inspect data structure
# str(dat)
# 
# # Step 1: Data Cleaning and Preparation
# # Remove irrelevant columns (update based on dataset specifics)
# irrelevant_columns = c("start", "end", "Date of interview", "Write the enumerator name",
#                         "_id", "_uuid", "_submission_time", "_validation_status", 
#                         "_notes", "_status", "_submitted_by", "__version__", "_tags", "_index")
# cleaned_data = dat %>%
#   select(-one_of(irrelevant_columns)) %>%
#   mutate(across(everything(), as.numeric, .names = "auto")) # Convert factors to numeric
# 
# # Handle missing values (e.g., replace with median or mean)
# cleaned_data = cleaned_data %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
# 
# # Step 2: Clustering
# # Scale the numeric data
# scaled_data = scale(cleaned_data)
# 
# # Apply K-Means clustering (3 clusters, can adjust based on data)
# set.seed(42)
# kmeans_result = kmeans(scaled_data, centers = 3)
# 
# # Add cluster labels to the dataset
# cleaned_data$Cluster = as.factor(kmeans_result$cluster)
# 
# # Step 3: Identifying Key Variables
# # Perform ANOVA for each variable to determine its importance
# anova_results = lapply(names(cleaned_data)[-ncol(cleaned_data)], function(var) {
#   formula = as.formula(paste(var, "~ Cluster"))
#   aov(formula, data = cleaned_data)
# })
# 
# # Extract variables with significant p-values (p < 0.05)
# significant_variables = names(cleaned_data)[-ncol(cleaned_data)][sapply(anova_results, function(aov_model) {
#   summary(aov_model)[[1]]$`Pr(>F)`[1] < 0.05
# })]
# 
# # Print significant variables
# print(significant_variables)
# 
# # Step 4: Visualization
# # Visualize clusters using principal components
# library(ggplot2)
# pca = prcomp(scaled_data)
# pca_data = as.data.frame(pca$x[, 1:2])
# pca_data$Cluster = cleaned_data$Cluster
# 
# ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
#   geom_point(size = 3) +
#   labs(title = "Clusters Visualization with PCA", x = "Principal Component 1", y = "Principal Component 2") +
#   theme_minimal()


# Select significant columns (update the list as needed)
selected_columns = c(
  "_GPS coordinates_latitude",
  "_GPS coordinates_longitude",
  "_GPS coordinates_altitude",
  "Which labor are available to engage on forage production/Household head",
  "Which labor are available to engage on forage production/Spouse",
  "Which labor are available to engage on forage production/Children",
  "Which labor are available to engage on forage production/Hired",
  "What is the farm size of the household (in ha)?",
  "How many cattles does the household have?",
  "What is your main source of livestock feed and forage/Grazing",
  "Which other forages have you heard of and interested to grow, but do not currently have/Oat-Vetch",
  "What challenges have you faced in cutivating or scaling improved forages under Ethiopia Grass project?/Agroecological mismatch"
)

# Filter and clean data
filtered_data = dat %>% 
  select(all_of(selected_columns)) %>%
  na.omit()

# Scale the data for clustering
scaled_data = scale(filtered_data)

# Apply K-means clustering
set.seed(123)
kmeans_result = kmeans(scaled_data, centers = 3) # Adjust clusters as needed

# Add cluster labels to the data
filtered_data$Cluster = as.factor(kmeans_result$cluster)

# Visualize clustering results
fviz_cluster(kmeans_result, data = scaled_data) +
  theme_bw() +
  labs(title = "")

ggsave("output/farmers-clusters.pdf",
       width = 20,
       height = 20,
       units = "cm")

dat$cluster = kmeans_result$cluster

dat$cluster_desc = dat$cluster

table(dat$cluster)

# ..............................
# ..............................
# labels to clusters from chatGPT
clust_labs = c("Traditional",
               "Innovative high-altitude",
               "Semi-commercial")

dat$cluster_desc[dat$cluster_desc == 1] = clust_labs[1]
dat$cluster_desc[dat$cluster_desc == 2] = clust_labs[2]
dat$cluster_desc[dat$cluster_desc == 3] = clust_labs[3]

table(dat$cluster_desc)

x = dat[,c("block_id", "cluster", "cluster_desc")]

write.csv(x, "data/farmers-clusters.csv", row.names = FALSE)

