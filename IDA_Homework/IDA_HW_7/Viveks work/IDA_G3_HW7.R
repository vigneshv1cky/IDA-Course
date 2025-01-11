libraries <- c("ggplot2","tidyverse","gridExtra","VIM","e1071","caret", "glmnet", "earth", "rpart", "randomForest", "xgboost", "e1071", "nnet", "gbm")
lapply(libraries, require, character.only = TRUE)

train_df <- read_csv("/Users/vignesh/RStudio/IDA_Homework/IDA Assignemnt 6/2024-ise-dsa-5103-ida-hw-6/Train.csv/Train.csv")
train_df <- as_tibble(train_df)
View(train_df)
#num_train_df <- train_df %>% select_if(is.numeric)
#cat_train_df <- train_df %>% select_if(is.character)
#View(cat_train_df)
#View(num_train_df)
sum(is.na(train_df))
colSums(is.na(train_df))
train_df <- train_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor), ~ ifelse(is.na(.), as.factor(names(sort(table(.), decreasing = TRUE))[1]), .)))
ggplot(train_df, aes(x = readmitted)) + 
  geom_bar() + 
  labs(title = "Distribution of Readmitted Patients")
ggplot(train_df, aes(x = age, fill = readmitted)) + 
  geom_density(alpha = 0.6) + 
  labs(title = "Age vs. Readmission", x = "Age", y = "Density")
ggplot(train_df, aes(x = time_in_hospital, fill = readmitted)) + 
  geom_density(alpha = 0.6) + 
  labs(title = "Time in Hospital vs. Readmission", x = "Time in Hospital", y = "Density")
ggplot(train_df, aes(x = num_lab_procedures, fill = readmitted)) + 
  geom_density(alpha = 0.6) + 
  labs(title = "Number of Lab Procedures vs. Readmission", x = "Number of Lab Procedures", y = "Density")

train_df <- train_df[!duplicated(train_df), ]
cat("Number of rows after removing duplicates:", nrow(train_df), "\n")

aggr(train_df, numbers = TRUE, sortVars = TRUE, cex.axis = 0.7, gap = 3, main = "Missing values")
missing_percent <- sapply(train_df, function(x) mean(is.na(x))) * 100
print(missing_percent)
train_df <- train_df[, missing_percent <= 30]
View(train_df)

#near_zero_var <- nearZeroVar(train_df)
#train_df <- train_df[, -near_zero_var]
#cat("Number of columns after removing low-variance features:", ncol(train_df), "\n")

categorical_vars <- sapply(train_df, is.factor)
numeric_vars <- sapply(train_df, is.numeric)

for (col in names(train_df)[categorical_vars]) {
  if (nlevels(train_df[[col]]) > 50) {
    cat("Column", col, "has", nlevels(train_df[[col]]), "levels. Consider grouping or removing it.\n")
  }
}

#cor_matrix <- cor(train_df[, numeric_vars], use = "complete.obs")
#high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)
#train_df <- train_df[, -high_corr]
#cat("Number of columns after removing highly correlated features:", ncol(train_df), "\n")

#train_df <- train_df[, !names(train_df) %in% c("patientId", "other_irrelevant_column")]
#cat("Number of columns after domain-specific cleaning:", ncol(train_df), "\n")

str(train_df)
summary(train_df)

for (col in names(train_df)) {
  if (is.factor(train_df[[col]]) || is.character(train_df[[col]])) {
    train_df[[col]] <- as.numeric(as.factor(train_df[[col]]))
  }
}

missing_percent <- sapply(train_df, function(x) mean(is.na(x))) * 100
print(missing_percent)

mode_impute <- function(x) {
  mode <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode
  return(x)
}
train_df$race <- mode_impute(train_df$race)
train_df$gender <- mode_impute(train_df$gender)
train_df$age <- mode_impute(train_df$age)

train_df$diagnosis[is.na(train_df$diagnosis)] <- mean(train_df$diagnosis, na.rm = TRUE)


colSums(is.na(train_df))

train_df$readmitted <- factor(train_df$readmitted, levels = c(0, 1), labels = c("No", "Yes"))
levels(train_df$readmitted)

train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
logreg_model <- train(readmitted ~ ., 
                      data = train_df, 
                      method = "glm", 
                      family = "binomial", 
                      trControl = train_control)
lasso_grid <- expand.grid(alpha = 1, 
                          lambda = seq(0.01, 1, by = 0.1))
lasso_model <- train(readmitted ~ ., 
                     data = train_df, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = lasso_grid)
mars_grid <- expand.grid(degree = c(1, 2, 3), 
                         nprune = seq(2, 12, by = 2))
mars_model <- train(readmitted ~ ., 
                    data = train_df, 
                    method = "earth", 
                    trControl = train_control, 
                    tuneGrid = mars_grid)
knn_grid <- expand.grid(k = seq(3, 10, by = 2))
knn_model <- train(readmitted ~ ., 
                   data = train_df, 
                   method = "knn", 
                   trControl = train_control, 
                   tuneGrid = knn_grid)
tree_grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))
tree_model <- train(
  readmitted ~ ., 
  data = train_df, 
  method = "rpart", 
  trControl = train_control, 
  tuneGrid = tree_grid,
  metric = "ROC"  # Specify ROC as the metric
)
rf_grid <- expand.grid(mtry = c(2, 4, 6, 8))
rf_model <- train(readmitted ~ ., 
                  data = train_df, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneGrid = rf_grid)
gbm_grid <- expand.grid(n.trees = seq(50, 200, by = 50), 
                        interaction.depth = c(1, 3, 5), 
                        shrinkage = c(0.01, 0.1), 
                        n.minobsinnode = c(10, 20))
gbm_model <- train(readmitted ~ ., 
                   data = train_df, 
                   method = "gbm", 
                   trControl = train_control, 
                   tuneGrid = gbm_grid, 
                   verbose = FALSE)
log_loss_results <- list()

log_loss <- function(y_true, y_pred) {
  epsilon <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - epsilon), epsilon)
  -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

# Convert `readmitted` to numeric for log loss calculation
y_true <- ifelse(train_df$readmitted == "Yes", 1, 0)

logreg_pred_1 <- predict(logreg_model, train_df, type = "prob")[, "Yes"]
log_loss_results$logreg <- log_loss(y_true, logreg_pred_1)

lasso_pred_1 <- predict(lasso_model, train_df, type = "prob")[, "Yes"]
log_loss_results$lasso <- log_loss(y_true, lasso_pred_1)

mars_pred_1 <- predict(mars_model, train_df, type = "prob")[, "Yes"]
log_loss_results$mars <- log_loss(y_true, mars_pred_1)

knn_pred_1 <- predict(knn_model, train_df, type = "prob")[, "Yes"]
log_loss_results$knn <- log_loss(y_true, knn_pred_1)

#tree_pred <- predict(tree_model, train_df, type = "prob")[, "1"]
#log_loss_results$tree <- log_loss(train_df$readmitted_numeric, tree_pred)

rf_pred_1 <- predict(rf_model, train_df, type = "prob")[, "Yes"]
log_loss_results$rf <- log_loss(y_true, rf_pred_1)

gbm_pred_1 <- predict(gbm_model, train_df, type = "prob")[, "Yes"]
log_loss_results$gbm <- log_loss(y_true, gbm_pred_1)

print(log_loss_results)

best_classifier <- names(which.min(unlist(log_loss_results)))
cat("The best classifier based on log loss is:", best_classifier, 
    "with a log loss of", round(log_loss_results[[best_classifier]], 4))

models <- list(logreg = logreg_model, 
               lasso = lasso_model, 
               mars = mars_model, 
               knn = knn_model, 
               rf = rf_model, 
               gbm = gbm_model)

model_results <- resamples(models)
summary(model_results)

# Initialize an empty data frame for storing model summaries
model_summary <- data.frame(
  Model = character(),
  Method = character(),
  Package = character(),
  Hyperparameter = character(),
  Selection = character(),
  Accuracy = numeric(),
  Kappa = numeric(),
  stringsAsFactors = FALSE
)

# Logistic Regression (logreg)
logreg_acc <- max(logreg_model$results$Accuracy)
logreg_kappa <- max(logreg_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "logreg",
  Method = "glm",
  Package = "stats",
  Hyperparameter = NA,
  Selection = NA,
  Accuracy = round(logreg_acc, 3),
  Kappa = round(logreg_kappa, 3)
))

# Lasso (Penalized Logistic Regression)
lasso_best_param <- lasso_model$bestTune$lambda
lasso_acc <- max(lasso_model$results$Accuracy)
lasso_kappa <- max(lasso_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "lasso (logreg)",
  Method = "glmnet",
  Package = "glmnet",
  Hyperparameter = "lambda",
  Selection = round(lasso_best_param, 2),
  Accuracy = round(lasso_acc, 3),
  Kappa = round(lasso_kappa, 3)
))

# MARS
mars_best_param <- mars_model$bestTune$degree
mars_acc <- max(mars_model$results$Accuracy)
mars_kappa <- max(mars_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "MARS",
  Method = "earth",
  Package = "earth",
  Hyperparameter = "degree",
  Selection = mars_best_param,
  Accuracy = round(mars_acc, 3),
  Kappa = round(mars_kappa, 3)
))

# k-Nearest Neighbors (kNN)
knn_best_param <- knn_model$bestTune$k
knn_acc <- max(knn_model$results$Accuracy)
knn_kappa <- max(knn_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "kNN",
  Method = "knn",
  Package = "class",
  Hyperparameter = "k",
  Selection = knn_best_param,
  Accuracy = round(knn_acc, 3),
  Kappa = round(knn_kappa, 3)
))

# Random Forest
rf_best_param <- rf_model$bestTune$mtry
rf_acc <- max(rf_model$results$Accuracy)
rf_kappa <- max(rf_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "random forest",
  Method = "rf",
  Package = "randomForest",
  Hyperparameter = "mtry",
  Selection = rf_best_param,
  Accuracy = round(rf_acc, 3),
  Kappa = round(rf_kappa, 3)
))

gbm_best_params <- gbm_model$bestTune
gbm_acc <- max(gbm_model$results$Accuracy)
gbm_kappa <- max(gbm_model$results$Kappa)
model_summary <- rbind(model_summary, data.frame(
  Model = "gradient boosting",
  Method = "gbm",
  Package = "gbm",
  Hyperparameter = "n.trees, interaction.depth, shrinkage, n.minobsinnode",
  Selection = paste("n.trees:", gbm_best_params$n.trees,
                    "interaction.depth:", gbm_best_params$interaction.depth,
                    "shrinkage:", gbm_best_params$shrinkage,
                    "n.minobsinnode:", gbm_best_params$n.minobsinnode),
  Accuracy = round(gbm_acc, 3),
  Kappa = round(gbm_kappa, 3)
))

# View the final summary table
print(model_summary)

test_df <- read_csv("C:/Users/saivi/OneDrive/Documents/DSA 5103 INTELLIGENCE SYSTES AND DATA ANALYTICS ASSIGNMENT/2024-dsa-ise-ida-classification-hw-7/hm7-Test-2024.csv")
test_df <- as_tibble(test_df)
View(test_df)
#num_train_df <- train_df %>% select_if(is.numeric)
#cat_train_df <- train_df %>% select_if(is.character)
#View(cat_train_df)
#View(num_train_df)
sum(is.na(test_df))
colSums(is.na(test_df))
test_df <- test_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor), ~ ifelse(is.na(.), as.factor(names(sort(table(.), decreasing = TRUE))[1]), .)))

test_df <- test_df[!duplicated(test_df), ]
cat("Number of rows after removing duplicates:", nrow(test_df), "\n")

aggr(test_df, numbers = TRUE, sortVars = TRUE, cex.axis = 0.7, gap = 3, main = "Missing values")
missing_percent <- sapply(test_df, function(x) mean(is.na(x))) * 100
print(missing_percent)
test_df <- test_df[, missing_percent <= 30]
View(test_df)

#near_zero_var <- nearZeroVar(train_df)
#train_df <- train_df[, -near_zero_var]
#cat("Number of columns after removing low-variance features:", ncol(train_df), "\n")

categorical_vars <- sapply(test_df, is.factor)
numeric_vars <- sapply(test_df, is.numeric)

for (col in names(test_df)[categorical_vars]) {
  if (nlevels(test_df[[col]]) > 50) {
    cat("Column", col, "has", nlevels(test_df[[col]]), "levels. Consider grouping or removing it.\n")
  }
}

#cor_matrix <- cor(train_df[, numeric_vars], use = "complete.obs")
#high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)
#train_df <- train_df[, -high_corr]
#cat("Number of columns after removing highly correlated features:", ncol(train_df), "\n")

#train_df <- train_df[, !names(train_df) %in% c("patientId", "other_irrelevant_column")]
#cat("Number of columns after domain-specific cleaning:", ncol(train_df), "\n")

str(test_df)
summary(test_df)

for (col in names(test_df)) {
  if (is.factor(test_df[[col]]) || is.character(test_df[[col]])) {
    test_df[[col]] <- as.numeric(as.factor(test_df[[col]]))
  }
}

missing_percent <- sapply(test_df, function(x) mean(is.na(x))) * 100
print(missing_percent)

mode_impute <- function(x) {
  mode <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode
  return(x)
}

test_df$race <- mode_impute(test_df$race)
test_df$gender <- mode_impute(test_df$gender)
test_df$age <- mode_impute(test_df$age)
test_df$max_glu_serum <- mode_impute(test_df$max_glu_serum)
test_df$A1Cresult <- mode_impute(test_df$A1Cresult)
test_df$metformin <- mode_impute(test_df$metformin)
test_df$repaglinide <- mode_impute(test_df$repaglinide)
test_df$nateglinide <- mode_impute(test_df$nateglinide)
test_df$chlorpropamide <- mode_impute(test_df$chlorpropamide)
test_df$glimepiride <- mode_impute(test_df$glimepiride)
test_df$acetohexamide <- mode_impute(test_df$acetohexamide)
test_df$glipizide <- mode_impute(test_df$glipizide)
test_df$glyburide <- mode_impute(test_df$glyburide)
test_df$tolbutamide <- mode_impute(test_df$tolbutamide)
test_df$pioglitazone <- mode_impute(test_df$pioglitazone)
test_df$rosiglitazone <- mode_impute(test_df$rosiglitazone)
test_df$acarbose <- mode_impute(test_df$acarbose)
test_df$miglitol <- mode_impute(test_df$miglitol)
test_df$troglitazone <- mode_impute(test_df$troglitazone)
test_df$tolazamide <- mode_impute(test_df$tolazamide)
test_df$examide <- mode_impute(test_df$examide)
test_df$citoglipton <- mode_impute(test_df$citoglipton)
test_df$insulin <- mode_impute(test_df$insulin)
test_df$`glyburide-metformin`<- mode_impute(test_df$`glyburide-metformin`)
test_df$`glipizide-metformin` <- mode_impute(test_df$`glipizide-metformin`)
test_df$`glimepiride-pioglitazone` <- mode_impute(test_df$`glimepiride-pioglitazone`)
test_df$`metformin-rosiglitazone` <- mode_impute(test_df$`metformin-rosiglitazone`)
test_df$`metformin-pioglitazone` <- mode_impute(test_df$`metformin-pioglitazone`)
test_df$diabetesMed <- mode_impute(test_df$diabetesMed)
test_df$diagnosis[is.na(test_df$diagnosis)] <- mean(test_df$diagnosis, na.rm = TRUE)

colSums(is.na(test_df))

log_loss_results_test <- list()

logreg_pred <- predict(logreg_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$logreg <- log_loss(y_true, logreg_pred)

lasso_pred <- predict(lasso_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$lasso <- log_loss(y_true, lasso_pred)

mars_pred <- predict(mars_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$mars <- log_loss(y_true, mars_pred)

knn_pred <- predict(knn_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$knn <- log_loss(y_true, knn_pred)

#tree_pred <- predict(tree_model, train_df, type = "prob")[, "1"]
#log_loss_results$tree <- log_loss(train_df$readmitted_numeric, tree_pred)

rf_pred <- predict(rf_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$rf <- log_loss(y_true, rf_pred)

gbm_pred <- predict(gbm_model, test_df, type = "prob")[, "Yes"]
log_loss_results_test$gbm <- log_loss(y_true, gbm_pred)

print(log_loss_results_test)

library(pROC)

# Get predictions on holdout sets (assuming we used cross-validation in caret)
rf_preds <- rf_model$pred
# Filter the predictions for the best tuning parameter
best_rf_preds <- rf_preds[rf_preds$mtry == rf_model$bestTune$mtry, ]
sum(is.na(best_rf_preds$obs))  # Should be 0
sum(is.na(best_rf_preds$Yes))  # Should be 0

best_rf_preds$obs <- factor(best_rf_preds$obs, levels = c("No", "Yes"))
best_rf_preds$Yes <- as.numeric(best_rf_preds$Yes)
# Calculate ROC and AUC
roc_obj <- roc(best_rf_preds$obs, best_rf_preds$Yes)  # "Yes" is the probability for the positive class
auc_value <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, main = "ROC Curve for Random Forest Model", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)



test_pred <- predict(rf_model, test_df, type = "prob")[, "Yes"]

test_df <- test_df %>% mutate(test_pred)
test_df$patientID <- as.integer(test_df$patientID)


submission <- test_df %>% select(patientID,test_pred) %>% rename(predReadmit=test_pred)

write.csv(submission, "C:/Users/saivi/OneDrive/Documents/DSA 5103 INTELLIGENCE SYSTES AND DATA ANALYTICS ASSIGNMENT/predictions.csv", row.names = FALSE)
