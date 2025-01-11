library(tidyverse)

Train <- read_delim("/Users/vignesh/RStudio/IDA_Homework/IDA_HW_8/wine+quality/winequality-red.csv", 
                    delim = ";", show_col_types = FALSE)
Train <- as_tibble(Train)
glimpse(Train)

table(quality)
nlevels(quality$quality)
summary(quality)

sum(duplicated(Train)) 
Train <- Train[!duplicated(Train), ]

quality <- Train %>% 
  select(quality) %>% 
  mutate(quality = as_factor(quality))

Train <- Train %>% select(-quality)
glimpse(Train)




# Function for Calculating Q1 and Q3 Quartiles
Q1 <- function(x, na.rm = TRUE) quantile(x, na.rm = na.rm)[2]
Q3 <- function(x, na.rm = TRUE) quantile(x, na.rm = na.rm)[4]

# Function for Summary Statistics
myNumericSummary <- function(x) {
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm = TRUE),
    min(x, na.rm = TRUE), Q1(x, na.rm = TRUE), median(x, na.rm = TRUE),
    Q3(x, na.rm = TRUE), max(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}


# Step 2: Compute Summary of Numeric Data
numericSummary <- Train %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min","Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("fixed acidity":"alcohol", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n), unique = as.numeric(unique), missing = as.numeric(missing),
         missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, 
                unique, unique_pct, everything())
numericSummary



plot_histogram <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data), aes(x = value)) +
    geom_histogram(aes(fill = after_stat(count)), color = "black", bins = 30) +
    labs(title = paste("Histogram of", column_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal() + 
    theme(legend.position = "none") 
}

map2(Train, names(Train), plot_histogram)

library(e1071)

skewValues_tibble <- Train %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness") %>%
  arrange(Skewness)
skewValues_tibble

library(caret)   

BoxCoxTrans(Train$chlorides)
BoxCoxTrans(Train$`residual sugar` + 1)
BoxCoxTrans(Train$sulphates+1)
BoxCoxTrans(Train$`total sulfur dioxide`+1)
BoxCoxTrans(Train$`free sulfur dioxide`+1)

boxcox_obj1 <- BoxCoxTrans(Train$chlorides)
boxcox_obj2 <- BoxCoxTrans(Train$`residual sugar` + 1)
boxcox_obj3 <- BoxCoxTrans(Train$sulphates+1)
boxcox_obj4 <- BoxCoxTrans(Train$`total sulfur dioxide`+1)
boxcox_obj5 <- BoxCoxTrans(Train$`free sulfur dioxide`+1)

Train$chlorides <- predict(boxcox_obj1, Train$chlorides)
Train$`residual sugar` <- predict(boxcox_obj2, Train$`residual sugar`+1)
Train$sulphates <- predict(boxcox_obj3, Train$sulphates+1)
Train$`total sulfur dioxide` <- predict(boxcox_obj4, Train$`total sulfur dioxide`+1)
Train$`free sulfur dioxide` <- predict(boxcox_obj5, Train$`free sulfur dioxide`+1)

plot_histogram <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data), aes(x = value)) +
    geom_histogram(aes(fill = after_stat(count)), color = "black", bins = 30) +
    scale_fill_gradient(low = "cadetblue2", high = "coral2") +  # Gradient color
    labs(title = paste("Histogram of", column_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal() + 
    theme(legend.position = "none")  # Display legend for gradient
}



# Example usage:
map2(Train, names(Train), plot_histogram)

#######################################
# Scaling and centering
#######################################

preProcess_obj <- preProcess(Train, 
                             method = c("center", "scale"))

# Transform the data using the preprocessing object
Train <- predict(preProcess_obj, Train)


#######################################
# Principal Component Analysis
#######################################
pca <- prcomp(Train, scale = TRUE)
summary(pca)

pcaData <- tibble(PC1 = pca$x[, 1], PC2 = pca$x[, 2])

pcaData <- tibble(PC1 = pca$x[, 1], 
                      PC2 = pca$x[, 2], 
                      Quality = factor(quality$quality))

# Plot the PCA results with quality as the color fill
ggplot(pcaData, aes(x = PC1, y = PC2, color = Quality)) +
  geom_point(size = 2) +
  labs(title = "PCA of Dataset with Quality Labels",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Wine Quality") +
  theme_minimal()

# library(factoextra)
# 
# # Scree plot
# fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) +
#   labs(title = "Scree Plot: Variance Explained by Components")

variance_data <- tibble(
  Principal_Component = 1:length(pca$sdev),
  Cumulative_Variance = cumsum((pca$sdev^2) / sum(pca$sdev^2)) * 100
)

# Plot cumulative variance explained with labels
ggplot(variance_data, aes(x = Principal_Component, y = Cumulative_Variance)) +
  geom_line(color = "cadetblue3", size = 1) +
  geom_point(color = "red3", size = 2) +
  geom_text(aes(label = paste0(round(Cumulative_Variance), "%")), 
            vjust = -0.5, 
            color = "black", 
            size = 3.5) +  
  scale_x_continuous(breaks = 1:nrow(variance_data)) +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Cumulative Variance (%)"
  ) +
  theme_minimal()

pca_data <- as_tibble(pca$x[, 1:8])

#######################################
# tsne
#######################################

library(Rtsne)


tsne_result <- Rtsne(Train, dims = 2, perplexity = 10, theta = 0.5, verbose = TRUE)

# Extract t-SNE components
tsne_data <- tibble(
  TSNE1 = tsne_result$Y[, 1],
  TSNE2 = tsne_result$Y[, 2],
  Quality = factor(quality$quality) # Use the quality factor for coloring
)

# ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = Quality)) +
#   geom_point(size = 2) +
#   labs(
#     title = "t-SNE Visualization",
#     x = "t-SNE Dimension 1",
#     y = "t-SNE Dimension 2",
#     color = "Wine Quality"
#   ) +
#   theme_minimal()

kmeans_tsne <- kmeans(tsne_data[, c("TSNE1", "TSNE2")], centers = 6, nstart = 25)

# Add cluster labels to the t-SNE data
tsne_data <- tsne_data %>%
  mutate(ClusterKMeans = factor(kmeans_tsne$cluster))

# Plot t-SNE with K-means clustering
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = ClusterKMeans)) +
  geom_point(size = 2) +
  labs(
    title = "t-SNE with K-means Clustering",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

#######################################
# KMeans
#######################################

kmeans_result <- kmeans(Train, centers = 6, nstart = 25)

# Use the first two principal components for plotting
cluster_data <- tibble(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  ClusterKMeans = factor(kmeans_result$cluster),  # K-means cluster assignments
  Quality = factor(quality$quality)        # Wine quality
)

# Plot the K-means clustering result
ggplot(cluster_data, aes(x = PC1, y = PC2, color = ClusterKMeans)) +
  geom_point(size = 2) +
  labs(
    title = "K-means Clustering with PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Plot the K-means clustering result
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = cluster_data$ClusterKMeans)) +
  geom_point(size = 2) +
  labs(
    title = "K-means Clustering with tsne",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

library(useful)
# Evaluate K-means for multiple cluster options using FitKMeans
fit_kmeans_result <- FitKMeans(Train, max.clusters = 10, nstart = 40, iter.max = 100)

# Plot the evaluation to visualize the optimal number of clusters
PlotHartigan(fit_kmeans_result) +
  labs(
    title = "Hartigan's Rule for Optimal Clusters",
    x = "Number of Clusters",
    y = "Hartigan's Index"
  ) +
  theme_minimal()

# Compute SSE for different numbers of clusters
sse_kmeans <- sapply(1:10, function(k) {
  kmeans(Train, centers = k, nstart = 25)$tot.withinss
})

# Plot SSE vs. Number of Clusters
ggplot(data = tibble(Clusters = 1:10, SSE = sse_kmeans), aes(x = Clusters, y = SSE)) +
  geom_line(color = "blue3", size = 1) +
  geom_point(color = "red3", size = 3) +
  labs(
    title = "Elbow Plot for K-means Clustering",
    x = "Number of Clusters",
    y = "Sum of Squared Errors (SSE)"
  ) +
  theme_minimal()

#############
# PAM
############

library(cluster)

# Apply k-medoids clustering
kmedoids_result <- pam(Train, k = 6)  # k = 6 clusters

# Add k-medoids cluster labels to the dataset
cluster_data$Cluster_kmedoids <- factor(kmedoids_result$clustering)

# Plot PAM clustering results with medoids
ggplot(cluster_data, aes(x = PC1, y = PC2, color = Cluster_kmedoids)) +
  geom_point(size = 2) +
  labs(
    title = "PAM Clustering with PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Compute average silhouette width for different numbers of clusters
silhouette_pam <- sapply(2:10, function(k) {
  pam_res <- pam(Train, k = k)
  mean(silhouette(pam_res$clustering, dist(Train))[, 3])  # Average silhouette width
})

# Plot Average Silhouette Width vs. Number of Clusters
ggplot(data = tibble(Clusters = 2:10, Silhouette = silhouette_pam), aes(x = Clusters, y = Silhouette)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Elbow Plot for PAM Clustering",
    x = "Number of Clusters",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()



#############
# Hierarchial Clustering
############

# Compute distance matrix
dist_matrix <- dist(Train)

# Perform hierarchical clustering using Ward's method
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the tree into 6 clusters
hierarchical_clusters <- cutree(hclust_result, k = 6)

# Add hierarchical cluster labels to the dataset
cluster_data$Cluster_hierarchical <- factor(hierarchical_clusters)

# Plot dendrogram
plot(hclust_result, labels = FALSE, main = "Dendrogram of Hierarchical Clustering")
rect.hclust(hclust_result, k = 6, border = "coral")

#############
# DBSACN
############

library(dbscan)

# Apply DBSCAN
dbscan_result <- dbscan(Train, eps = 0.77, minPts = 5)

# Add DBSCAN cluster labels to the dataset
cluster_data$Cluster_dbscan <- factor(dbscan_result$cluster)

# Plot DBSCAN clustering result
ggplot(cluster_data, aes(x = PC1, y = PC2, color = Cluster_dbscan)) +
  geom_point(size = 2) +
  labs(
    title = "DBSCAN Clustering of Dataset",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

#############
# OPTICS
############

library(dbscan)

# Apply OPTICS
optics_result <- optics(Train, eps = 5, minPts = 5)

# Extract cluster labels using a reachability plot
optics_clusters <- extractDBSCAN(optics_result, eps_cl = 0.5)

# Add OPTICS cluster labels to the dataset
cluster_data$Cluster_optics <- factor(optics_clusters$cluster)

# Plot OPTICS clustering result
ggplot(cluster_data, aes(x = PC1, y = PC2, color = Cluster_optics)) +
  geom_point(size = 2) +
  labs(
    title = "OPTICS Clustering of Dataset",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()



