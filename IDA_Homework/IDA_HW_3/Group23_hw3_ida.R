# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(mlbench)    # For loading datasets
data("Glass")       # Load the Glass dataset

# Check for duplicates in the dataset
duplicates <- duplicated(Glass)   # Identify duplicate rows
Glass[duplicates, ]               # Display duplicate rows
sum(duplicated(Glass))            # Count the number of duplicates

# Remove duplicates from the dataset
Glass <- Glass[!duplicates, ]     # Keep only unique rows
sum(duplicated(Glass))            # Verify there are no duplicates left

# Compute the correlation matrix for the first 9 columns
CorMat <- cor(Glass[, 1:9])

# Perform eigen decomposition on the correlation matrix
eigen_result <- eigen(CorMat)     
eigen_values <- eigen_result$values    # Extract eigenvalues
eigen_vector <- eigen_result$vectors   # Extract eigenvectors

# Perform Principal Component Analysis (PCA) on the first 9 columns
pca <- prcomp(Glass[,1:9], scale. = TRUE)  # Standardize before PCA
summary(pca)    # Summary of the PCA results

# Compare eigenvalues to the variances explained by PCA components
print(pca$sdev^2)  # Squared standard deviations to get variances from PCA
print(eigen_values)  # Print eigenvalues from eigen decomposition

# Extract the first two principal components
pc1 <- pca$x[, 1]   # First principal component
pc2 <- pca$x[, 2]   # Second principal component

# Compute the inner product of the first two principal components
inner_product <- sum(pc1 * pc2)  # Should be close to 0 if orthogonal
print(inner_product)

# Visualize the correlation matrix using different methods
library(corrplot)
corrplot(CorMat, method = "color")                # Color method
corrplot(CorMat, method = "number", order = "AOE") # Numbers with 'AOE' order
corrplot(CorMat, method = "circle", order = "AOE") # Circle with 'AOE' order

# Plot the first two principal components using ggplot2
ggplot(mapping = aes(x = pc1, y = pc2, color = Glass$Type)) +
  geom_point()  # Scatter plot of the first two principal components

# Visualize PCA using biplot and ggbiplot
library(ggbiplot)
biplot(pca)                     # Basic biplot of PCA
ggbiplot(pca, groups = Glass$Type)  # Biplot colored by Glass type

# Load libraries for Linear Discriminant Analysis (LDA)
library(MASS)
library(caret)

# Preprocess the data: center and scale
preproc.param <- Glass %>% preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
transformed <- preproc.param %>% predict(Glass)

# Fit an LDA model to the transformed data
lda.model <- lda(Type ~., data = transformed)
lda.model

# Predict using the LDA model
predictions <- lda.model %>% predict(transformed)

# Plot the LDA model results
plot(lda.model)

# Plot LDA results with ggplot2
ggplot(predictions$x, aes(x = LD1, y = LD2, color = Glass$Type)) + 
  geom_point() +
  labs(color = 'Type')

par(mar=c(1,1,2,2))

# Histogram of the first LDA component
ldahist(predictions$x[,1], g = Glass$Type, xlab = "LD1")

# Histogram of the second LDA component
ldahist(predictions$x[,2], g = Glass$Type, col = 3)

# Display the scaling of the LDA model
lda.model$scaling


#---------------
# Analysis of Heptathlon dataset
#---------------

# Load additional libraries and the heptathlon dataset
library(HSAUR2)  # For the heptathlon dataset
library(outliers)  # For Grubb’s test for outliers

heptathlon <- heptathlon  # Load the heptathlon dataset

# Plot the heptathlon data
plot(heptathlon)

# Detect outliers in each column using Grubb's test
outliers <- lapply(heptathlon[,1:7], function (x) {
  test <- grubbs.test(x)  # Grubb's test for outliers
})
outliers

# Identify specific outliers in the 'hurdles' event
heptathlon[heptathlon$hurdles == outlier(heptathlon$hurdles),]  
heptathlon <- heptathlon[heptathlon$hurdles != outlier(heptathlon$hurdles),]  # Remove outlier

# Plot the cleaned heptathlon data
plot(heptathlon)

# Transform running event times to scores
heptathlon_transformed <- heptathlon %>%
  mutate(
    hurdles = max(hurdles) - hurdles,
    run200m = max(run200m) - run200m,
    run800m = max(run800m) - run800m
  )

# Perform PCA on the transformed heptathlon data
Hpca <- prcomp(heptathlon_transformed[, 1:7], scale. = TRUE)

# Visualize PCA results with ggbiplot
ggbiplot(Hpca)

# Extract the first two principal components
Hpc1 <- Hpca$x[,1]
Hpc2 <- Hpca$x[,2]

# Plot first principal component against the heptathlon score
ggplot(mapping = aes(Hpc1, heptathlon$score)) +
  geom_point() +
  labs(y = "score")

#---------------
# Analysis of Housing Data
#---------------

# Load housing data
housingData <- read_csv("housingData.csv")

# Convert housing data to tibble for easier manipulation
housingData <- as_tibble(housingData)

# Select and transform numerical columns, calculate additional features
hd <- housingData %>%
  select_if(is.numeric) %>%
  dplyr::mutate(
    age = YrSold - YearBuilt,
    ageSinceRemodel = YrSold - YearRemodAdd,
    ageofGarage = ifelse(is.na(GarageYrBlt), age, YrSold - GarageYrBlt)
  ) %>%
  dplyr::select(!c(Id, MSSubClass, LotFrontage, GarageYrBlt, MiscVal, YrSold, MoSold, YearBuilt, YearRemodAdd, MasVnrArea))

# View structure of the processed data
glimpse(hd)

# Compute the correlation matrix of the processed data
cor_hd <- cor(hd)

# Visualize the correlation matrix using corrplot
corrplot(cor_hd, method = "circle", order = "alphabet")

# Filter the correlation matrix for significant correlations
significant_cor_hd <- cor_hd  # Copy the original correlation matrix
significant_cor_hd[abs(significant_cor_hd) < 0.5] <- 0  # Set non-significant correlations to 0

# Plot the filtered correlation matrix
corrplot(significant_cor_hd, method = "circle", order = "alphabet")

# Perform PCA on the processed housing data
pca_hd <- prcomp(hd, scale. = TRUE)

# Display summary of PCA results
summary(pca_hd)

# Visualize PCA results using ggbiplot
ggbiplot(pca_hd)

# Plot a scree plot to show the variances explained by the principal components
screeplot(pca_hd, npcs = min(20, length(pca_hd$sdev)), 
          type = "lines", main = "Scree Plot of Pca_hd")
