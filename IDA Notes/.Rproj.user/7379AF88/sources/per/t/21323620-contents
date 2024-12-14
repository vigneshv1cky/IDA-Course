library(tidyverse)
library(gridExtra)
library(VIM)
library(e1071)
library(caret)
library(car)
library(MASS)
library(pls)
library(glmnet)

# 1 Data Quality Report

housingData <- read_csv("housingData.csv")
is_tibble(housingData)

# Creating New Variables called age, ageSinceRemodel, ageofGarage
housingData <- housingData %>%
  mutate(age = YrSold - YearBuilt,
         ageSinceRemodel = YrSold - YearRemodAdd,
         ageofGarage = YrSold - GarageYrBlt,
         logSalePrice = log(SalePrice))

glimpse(housingData)

# Creating New Tibbles for Numeric and Factor Data
housingNumeric <- housingData %>% select_if(is.numeric)
housingFactor <- housingData %>% transmute_if(is.character, as.factor)

# Funtion for Calculating Q1 Quartile.
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

# Funtion for Calculating Q3 Quartile.
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# Funtion to set Summary Functions for Numeric Data
myNumericSummary <- function(x) {
  c(length(x), 
    n_distinct(x), 
    sum(is.na(x)), 
    mean(x, na.rm = TRUE),
    min(x, na.rm = TRUE), 
    Q1(x, na.rm = TRUE), 
    median(x, na.rm = TRUE), 
    Q3(x, na.rm = TRUE),
    max(x, na.rm = TRUE), 
    sd(x, na.rm = TRUE))
}

# Compute Summary of Numeric Data
numericSummary <- housingNumeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min", "Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("Id":"logSalePrice", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    n = as.numeric(n),
    unique = as.numeric(unique),
    missing = as.numeric(missing),
    missing_pct = 100 * missing / n,
    unique_pct = 100 * unique / n
  ) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

glimpse(numericSummary)

# Variables with Missing data Percentage :
# LotFrontage - 20.7% 
# MasVnrArea - 0.4%
# GarageYrBlt - 5.3%
# ageofGarage - 5.3%

# ID Variable is removed because it not a valid variable for analysis
housingNumeric_NoID <- housingNumeric %>%  dplyr::select(-Id)
 
# # BELOW CODE WAS USED FOR MICE IMPUTATION WITH PMM.
# # Load mice Package
# library(mice)
# 
# # Run mice imputation
# imputed_data <- mice(housingNumeric_NoID, m = 5, method = 'pmm', maxit = 50, seed = 123)
# 
# # Step 4: Review the imputed data and get the completed dataset
# summary(imputed_data)
# 
# completed_data <-  complete(imputed_data, action=3)
# completed_data

# Missing variables is Imputed Using KNN from VIM
housingNumeric_imputed <- kNN(housingNumeric_NoID, imp_var=FALSE)

p1 <- ggplot(housingNumeric_NoID, aes(x = LotFrontage)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  ggtitle("Distribution of LotFrontage ") +
  xlab("LotFrontage") +
  ylab("Frequency")

p3 <- ggplot(housingNumeric_imputed, aes(x = LotFrontage)) + 
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  ggtitle("LotFrontage after Imputation") +
  xlab("LotFrontage") + 
  ylab("Frequency") 

# Calculate the maximum y-axis limit based on bin counts directly from ggplot
max_y <- max(layer_data(p1)$count, layer_data(p3)$count)

# Set the y-axis limit to the same range for both plots
p1 <- p1 + ylim(0, max_y)
p3 <- p3 + ylim(0, max_y)

# Arrange the plots side by side
grid.arrange(p1, p3, ncol = 2)

# Computing the skewness of variables and Sorting them
skewValues_tibble <- housingNumeric_imputed %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness") %>% 
  arrange(Skewness)

print(skewValues_tibble, n = Inf)

Trans <- preProcess(housingNumeric_imputed, method = "BoxCox")
Trans

# Apply the Box-Cox transformation to the Imputed Data
housingNumeric_transformed <- predict(Trans, housingNumeric_imputed)

skewValues_tibble_Trans <- housingNumeric_transformed %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness") %>% 
  arrange(Skewness)

print(skewValues_tibble, n = Inf)
# There are skewed Variables even after boxcox Transformation , Dont know if we should apply more transformation or remove those varaibles.
# I believe in removing them as Linear regression requires normality
# I didnt remove them


# ******************************************************************************
# ******************************************************************************
# ******************************************************************************


# Function to compute mode for Factor data
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

# funtion to compute the count of mode for Factor data
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

# Funtion to set Summary Functions for Factor Data
myFactorSummary <- function(x) {
  c(length(x), 
    n_distinct(x),
    sum(is.na(x)),
    getmodes(x, type = 1),
    getmodesCnt(x, type = 1),
    getmodes(x, type = 2),
    getmodesCnt(x, type = 2),
    getmodes(x, type = -1),
    getmodesCnt(x, type = -1))
}

# Compute Summary of Factor Data
FactorSummary <- housingFactor %>%
  reframe(across(everything(), myFactorSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "most_common", "most_common_count", 
                 "2nd_most_common", "2nd_most_common_count", "least_common", 
                 "least_common_count")) %>%
  pivot_longer("MSZoning":"SaleType", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    n = as.numeric(n),
    unique = as.numeric(unique),
    missing = as.numeric(missing),
    missing_pct = 100 * missing / n,
    unique_pct = 100 * unique / n
  ) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

glimpse(FactorSummary)

# Missing value Percentage Of Varibales with Missing Values
# Alley - 93.8%
# MasVnrType - 0.4%
# BsmtQual - 3.1%
# BsmtCond - 3.1%
# BsmtExposure - 3.2%
# BsmtFinType1 - 3.1%
# BsmtFinType2 - 3.2%
# FireplaceQu - 46.6%
# GarageType - 5.3%
# GarageFinish - 5.3%
# GarageQual - 5.3%
# GarageCond - 5.3%
# PoolQC - 99.8%
# Fence - 80.5%
# MiscFeature - 96.6%
# Electrical - 0.1%

# Dropping variables which have missing values grater than 60%
housingFactor <- housingFactor %>% 
  dplyr::select(-c(Alley, PoolQC, MiscFeature, Fence))

# Impute Missing Values in housingFactor
housingFactor_imputed <- kNN(housingFactor, imp_var=FALSE)

# # Calculate frequencies directly from the data
# count_before <- table(housingFactor$FireplaceQu)
# count_after <- table(housingFactor_imputed$FireplaceQu)
# 
# max_y <- max(c(count_before, count_after))

# Bar plot before imputation
p1 <- ggplot(housingFactor, aes(x = FireplaceQu)) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  ggtitle("Distribution of FireplaceQu (Before Imputation)") +
  xlab("FireplaceQu") +
  ylab("Frequency")

# Bar plot after imputation
p3 <- ggplot(housingFactor_imputed, aes(x = FireplaceQu)) + 
  geom_bar(fill = "salmon", color = "black") +
  ggtitle("Distribution of FireplaceQu (After Imputation)") +
  xlab("FireplaceQu") + 
  ylab("Frequency")

# Determine the maximum y-axis value from both plots
max_y <- max(layer_data(p1)$count, layer_data(p3)$count)

# Set y-axis limits for both plots
p1 <- p1 + ylim(0, max_y)
p3 <- p3 + ylim(0, max_y)

# Arrange plots side by side
grid.arrange(p1, p3, ncol = 2)

glimpse(housingFactor_imputed)

factor_levels <- map(housingFactor_imputed, ~fct_count(.x, sort = TRUE))
factor_levels
# Factors to Collapse :
# MSZoning - collapse to 3 groups
# LotShape - collapse to 3 groups
# HouseStyle - collapse to 4 groups

# Collapse MSZoning to 3 groups
housingFactor_imputed$MSZoning <- fct_lump_n(housingFactor_imputed$MSZoning, n=2) 
fct_count(housingFactor_imputed$MSZoning)
# Collapse LotShape tp 3 groups
housingFactor_imputed$LotShape <- fct_lump_n(housingFactor_imputed$LotShape, n=2) 
fct_count(housingFactor_imputed$LotShape)
# Collapse HouseStyle to 4 groups
housingFactor_imputed$HouseStyle <- fct_lump_n(housingFactor_imputed$HouseStyle, n=3) 
fct_count(housingFactor_imputed$HouseStyle)

summary(housingFactor_imputed)

# Combining Numeric and Factor variables
housingDataProcessed <- bind_cols(housingNumeric_transformed, housingFactor_imputed)

# Drop SalePrice as its high Correlated with LogSalePrice
housingDataProcessed <- housingDataProcessed %>% 
  dplyr::select(-SalePrice)

summary(housingDataProcessed)

# ******************************************************************************
# ******************************************************************************
# ******************************************************************************

# Outlier Detection and Removal
housingDataProcessed <-  housingDataProcessed[-c(402, 124), ]

ols_fit <- lm(logSalePrice ~ ., data = housingDataProcessed)
summary(ols_fit)

par(mfrow=c(2,2))
plot(ols_fit)
par(mfrow=c(1,1))

qqPlot(ols_fit)
# There is Non Constant error Variance
ncvTest(ols_fit)
spreadLevelPlot(ols_fit)
outlierTest(ols_fit)
# Many Outliers are present but they don't have leverage on model.

# ******************************************************************************
# ****************************Modelling*****************************************
# ******************************************************************************

# 1a.

# OLS with lm
ols_fit <- lm(logSalePrice ~ ., data = housingDataProcessed)
summary(ols_fit)
AIC(ols_fit)
BIC(ols_fit)


# Stepwise selection based on AIC
stepwise_model <- stepAIC(ols_fit, direction = "both", trace = FALSE)
summary(stepwise_model)
AIC(stepwise_model)
BIC(stepwise_model)
vif(stepwise_model)

# Linear regression after variable selection with stepAIC
ols2 = lm(formula = logSalePrice ~ LotArea + 
     OverallCond + MasVnrArea + BsmtFinSF2 + BsmtFullBath + 
     KitchenAbvGr + Fireplaces + GarageArea + 
     WoodDeckSF + OpenPorchSF + EncPorchSF + PoolArea + 
     ageSinceRemodel + MSZoning + LotConfig + Neighborhood + 
     Condition1 + RoofStyle + Exterior1st + ExterQual + BsmtExposure + 
     BsmtFinType1 + Heating + HeatingQC + CentralAir + Functional + 
     GarageQual + PavedDrive, 
     data = housingDataProcessed)
summary(ols2)
vif(ols2)
par(mfrow=c(2,2))
plot(ols2)
par(mfrow=c(1,1))
# Non Constant Error variance is Present
ncvTest(ols2)


# Build the linear model with interaction terms included
ols_interaction <- lm(formula = logSalePrice ~ LotArea + 
                        OverallCond * ExterQual + MasVnrArea + BsmtFinSF2 + BsmtFullBath + 
                        KitchenAbvGr + Fireplaces + GarageArea * GarageQual + 
                        WoodDeckSF + OpenPorchSF + EncPorchSF + PoolArea + 
                        ageSinceRemodel + MSZoning + LotConfig + Neighborhood * GrLivArea + 
                        Condition1 + RoofStyle + Exterior1st + BsmtExposure + 
                        BsmtFinType1 + Heating + HeatingQC + CentralAir + Functional + 
                        PavedDrive, data = housingDataProcessed)
summary(ols_interaction)
vif(ols_interaction, type = "predictor")

# Perform 5-fold cross-validation using caret
cv_control <- trainControl(method = "cv", number = 10)
ols_cv_interaction_model <- train(logSalePrice ~ LotArea + 
                    OverallCond * ExterQual + MasVnrArea + BsmtFinSF2 + BsmtFullBath + 
                    KitchenAbvGr + Fireplaces + GarageArea * GarageQual + 
                    WoodDeckSF + OpenPorchSF + EncPorchSF + PoolArea + 
                    ageSinceRemodel + MSZoning + LotConfig + Neighborhood * GrLivArea + 
                    Condition1 + RoofStyle + Exterior1st + BsmtExposure + 
                    BsmtFinType1 + Heating + HeatingQC + CentralAir + Functional + PavedDrive,
                    data = housingDataProcessed, 
                    method = "lm", 
                    trControl = cv_control)
# Display cross-validation results
ols_cv_interaction_model
summary(ols_cv_interaction_model)
ols_cv_interaction_rmse <- ols_cv_interaction_model$results$RMSE
ols_cv_interaction_r2 <- ols_cv_interaction_model$results$Rsquared

cv_results <- ols_cv_interaction_model$resample

# Plot RMSE for each fold
ggplot(cv_results, aes(x = Resample, y = RMSE)) +
  geom_point(color = "blue", size = 3) +
  geom_line(group = 1, color = "blue") +
  labs(title = "Cross-Validation RMSE for OLS Model", x = "Fold", y = "RMSE") +
  theme_minimal()

# ******************************************************************************

# 1b.

# PLS
cv_control <- trainControl(method = "cv", number = 5) 

tune_grid <- expand.grid(ncomp = seq(25,35, length = 50))

# Train PLS model using caret
pls_model <- train(logSalePrice ~ .,
                   data = housingDataProcessed, 
                   method = "pls",
                   trControl = cv_control,
                   tuneGrid = tune_grid
                   )
pls_model
summary(pls_model)

plot(pls_model)

pls_rmse <- pls_model$results$RMSE
pls_r2 <- pls_model$results$Rsquared

# ******************************************************************************

# LASSO

cv_control <- trainControl(method = "cv", number = 5)  

# Define the grid of hyperparameters (lambda values) to test
tune_grid <- expand.grid(alpha = 1,  # LASSO regression is when alpha = 1
                         lambda = seq(0.0001, 0.0004, length = 100))  # Range of lambda values

# Train LASSO model using caret and glmnet
lasso_model <- train(
  logSalePrice ~ .,
  data = housingDataProcessed,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = tune_grid
)

lasso_model

summary(lasso_model)

plot(lasso_model)

# Visualize coefficient shrinkage as lambda increases
plot(lasso_model$finalModel, label = TRUE)

coef(lasso_model$finalModel, s = 0.0001878788)

best_lambda <- lasso_model$bestTune$lambda
coef(lasso_model$finalModel, s = best_lambda)

lasso_rmse <- lasso_model$results$RMSE
lasso_r2 <- lasso_model$results$Rsquared

# ******************************************************************************

# Ridge

cv_control <- trainControl(method = "cv", number = 5)  

# Define the grid of hyperparameters (lambda values) to test
tune_grid <- expand.grid(alpha = 0,  # Ridge regression is when alpha = 0
                         lambda = seq(0.0023,0.0024, length = 500))  # Range of lambda values

# Train Ridge model using caret and glmnet
Ridge_model <- train(
  logSalePrice ~ .,
  data = housingDataProcessed,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = tune_grid
)

Ridge_model

summary(Ridge_model)

plot(Ridge_model)

# Visualize coefficient shrinkage as lambda increases
plot(Ridge_model$finalModel, label = TRUE)

best_lambda <- Ridge_model$bestTune$lambda
coef(Ridge_model$finalModel, s = best_lambda)

Ridge_rmse <- Ridge_model$results$RMSE
Ridge_r2 <- Ridge_model$results$Rsquared

# ******************************************************************************

# ElasticNet

cv_control <- trainControl(method = "cv", number = 5)  

# Define the grid of hyperparameters (lambda values) to test
tune_grid <- expand.grid(alpha = seq(0,1, length = 100),
                         lambda = seq(0.0023,0.0024, length = 500))  # Range of lambda values

# Train LASSO model using caret and glmnet
elasticNet_model <- train(
  logSalePrice ~ .,
  data = housingDataProcessed,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = tune_grid
)

elasticNet_model

summary(elasticNet_model)

plot(elasticNet_model)

# Visualize coefficient shrinkage as lambda increases
plot(elasticNet_model$finalModel, label = TRUE)

best_alpha <- elasticNet_model$bestTune$alpha
best_lambda <- elasticNet_model$bestTune$lambda

coef(elasticNet_model$finalModel, s = best_lambda)

elasticnet_rmse <- elasticNet_model$results$RMSE
elasticnet_r2 <- elasticNet_model$results$Rsquared

# ******************************************************************************

# PCR
cv_control <- trainControl(method = "cv", number = 5) 

tune_grid <- expand.grid(ncomp = seq(25,35, length = 100))

# Train PCR model using caret
pcr_model <- train(logSalePrice ~ .,
                   data = housingDataProcessed, 
                   method = "pcr",
                   trControl = cv_control,
                   tuneGrid = tune_grid
)
pcr_model
summary(pls_model)

plot(pls_model)

pcr_rmse <- pcr_model$results$RMSE
pcr_r2 <- pcr_model$results$Rsquared

# ******************************************************************************

model_performance <- data.frame(
  Model = c("OLS with Interactions", "PLS", "LASSO", "Ridge", "ElasticNet", "PCR"),
  Hyperparameter = c("None", "ncomp", "alpha = 1, lambda", "alpha = 0, lambda", "alpha and lambda", "ncomp"),
  CV_RMSE = c(min(ols_cv_interaction_rmse), min(pls_rmse), min(lasso_rmse), min(Ridge_rmse), min(elasticnet_rmse), min(pcr_rmse)),
  CV_R2 = c(max(ols_cv_interaction_r2), max(pls_r2), max(lasso_r2), max(Ridge_r2), max(elasticnet_r2), max(pcr_r2))
)

# Display the summary table
model_performance





