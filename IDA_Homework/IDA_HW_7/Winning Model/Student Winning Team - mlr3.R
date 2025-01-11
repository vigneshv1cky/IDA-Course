library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(data.table)
library(Metrics)
library(dplyr)
library(missRanger)
library(mice)
library(Boruta)
library(ggplot2)
library(mlr3measures)
library(mlr3extralearners)
library(catboost)
library(dbarts)
#you will need these
#library(devtools) 
#install_github("mlr-org/mlr3extralearners")
#install_github('catboost/catboost', subdir = 'catboost/R-package')




#avialable models
mlr_learners$keys()
lrns()
#if you want to learn about a specific model change this
??mlr_learners_classif.ranger


data_original <- read.csv("hm7-train-2024.csv")
test_original <- read.csv("hm7-test-2024.csv")

# Convert to factors
data_prepared <- data_original %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(~ is.integer(.) || is.character(.)), as.factor))

test_prepared <- test_original %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(~ is.integer(.) || is.character(.)), as.factor))

#remove useless features with only 1 level
data_prepared <- data_prepared %>% dplyr::select(-examide, -citoglipton, -glimepiride.pioglitazone)
test_prepared <- test_prepared %>% dplyr::select(-examide, -citoglipton, -glimepiride.pioglitazone)

# Imputation with missRanger. If you use missForest it will not work, it is limited on dimensions/levels 
selected_data <- missRanger(data_prepared, maxiter = 5, num.trees = 200, pmm.k = 5)
#missRanger info
??missRanger

# Imputation with MICE, this is the second imputation to impute because it works on categorical variables
mice_methods <- make.method(selected_data)
numeric_cols <- names(selected_data)[sapply(selected_data, is.numeric)]
categorical_cols <- names(selected_data)[sapply(selected_data, is.factor)]
mice_methods[numeric_cols] <- "norm.predict"
mice_methods[categorical_cols] <- "logreg"
selected_data <- complete(mice(selected_data, m = 15, method = mice_methods))


#boruta information
??boruta
# Sequential Feature Selection, this will automatically delete features that have no influence on readmitted 
# it does this by comparing a feature with random fake numbers compared to the target variable to see if there is a difference 
boruta_result <- Boruta(readmitted ~ ., data = selected_data, doTrace = 3,pvalue = 0.25, maxRuns = 70)
selected_vars <- getSelectedAttributes(boruta_result, withTentative = FALSE)

#these variables is for the test file for later, used to delete columns without using boruta again
selected_data <- selected_data[, c("readmitted", selected_vars)]
columns_to_keep <- setdiff(names(selected_data), "readmitted")

#plot of levels of importance and print which catagories did/didn't meet requirements
plot(boruta_result)
print(boruta_result$finalDecision)


# training and testing
train_task <- TaskClassif$new(id = "readmission", backend = selected_data, target = "readmitted")
test_task <- TaskClassif$new(id = "readmission_test", backend = selected_data, target = "readmitted")
train_task$col_roles$stratum <- "readmitted"

# one-hot encoding
onehot = po("encode", method = "one-hot")

# encode the train and test tasks
train_task_encoded <- onehot$train(list(train_task))[[1]]
test_task_encoded <- onehot$predict(list(test_task))[[1]]

#in case needed to pca anything
pca <- po("pca", rank. = 400) 



# you need to do this to run larger sized models with big datasets
options(future.globals.maxSize = 1024 * 1024 * 1024)  # Set to 1 GB instead of default 500 MB

#e-net example of hyper tuning
learner_glmnet <- lrn("classif.glmnet", predict_type = "prob")

#parameter grids for manual tuning best alpha was previously found
alpha_value <- c(0.2)          
lambda_value <- seq(0.009, 0.011, by = 0.0002) 

# Store the best parameters
best_alpha <- NULL
best_lambda <- NULL
lowest_log_loss <- Inf

# Manual tuning loop
for (alpha in alpha_value) {
  for (lambda in lambda_value) {
    
    learner_glmnet$param_set$values <- list(alpha = alpha, lambda = lambda)
    
    #Set 5 fold cv
    resampling <- rsmp("cv", folds = 5)
    rr <- resample(train_task_encoded, learner_glmnet, resampling)
    
    # Calculate the average log loss for the folds
    avg_log_loss <- rr$aggregate(msr("classif.logloss"))
    
    # Keeps only the best
    if (avg_log_loss < lowest_log_loss) {
      lowest_log_loss <- avg_log_loss
      best_alpha <- alpha
      best_lambda <- lambda
    }
    
    cat("Alpha:", alpha, "Lambda:", lambda, "Log Loss:", avg_log_loss, "\n")
  }
}

# Best parameters and log loss
cat("Best Alpha:", best_alpha, "\n")
cat("Best Lambda:", best_lambda, "\n")
cat("Lowest Log Loss:", lowest_log_loss, "\n")




#these were manually hypertuned
# GBM using a tree
learner_gb <- lrn("classif.xgboost", predict_type = "prob",booster = "gbtree")
#(Another version of a tree)
learner_bart <- lrn("classif.bart", predict_type = "prob", 
                    ntree = 150, k = 1, power= 2.5, base = .99)

# Ranger(Upgraded Random Forest)
learner_rf <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)
learner_rf1 <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)
learner_rf2 <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)
learner_rf3 <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)
learner_rf4 <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)
learner_rf5 <- lrn("classif.ranger", predict_type = "prob", num.trees = 100, max.depth = 20)

# Elastic net
learner_glmnet1<- lrn("classif.glmnet", predict_type = "prob", alpha = .2, lambda = .11)
learner_glmnet2<- lrn("classif.glmnet", predict_type = "prob", alpha = .2, lambda = .09)
learner_glmnet3<- lrn("classif.glmnet", predict_type = "prob", alpha = .2, lambda = .0115)
learner_glmnet4<- lrn("classif.glmnet", predict_type = "prob", alpha = .2, lambda = .013)

# GBM using dart  (better results if you use multiple but it will take 30min per model)
learner_xgb <- lrn("classif.xgboost", predict_type = "prob", 
                   booster = "dart", nrounds = 150, max_depth = 8, eta = 0.2)
#lightGBM
learner_lgb <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.16, num_leaves = 20, max_depth = 7, bagging_fraction = 0.6)
learner_lgb1 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.1, num_leaves = 31, max_depth = 10, bagging_fraction = 0.6)
learner_lgb2 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.1, num_leaves = 31, max_depth = 15, bagging_fraction = 0.6)
learner_lgb3 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.1, num_leaves = 31, max_depth = 15, bagging_fraction = 0.8)
learner_lgb4 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.05, num_leaves = 50, max_depth = 10, bagging_fraction = 0.6)
learner_lgb5 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.05, num_leaves = 50, max_depth = 15, bagging_fraction = 0.6)
learner_lgb6 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.05, num_leaves = 31, max_depth = 15, bagging_fraction = 0.8)
learner_lgb7 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.05, num_leaves = 31, max_depth = 15, bagging_fraction = 0.6)
learner_lgb8 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.1, num_leaves = 15, max_depth = 15, bagging_fraction = 0.6)
learner_lgb9 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.05, num_leaves = 50, max_depth = 15, bagging_fraction = 0.8)
learner_lgb10 <- lrn("classif.lightgbm", predict_type = "prob", learning_rate = 0.2, num_leaves = 50, max_depth = 10, bagging_fraction = 0.8)

#catboost(another tree)
learner_cat <- lrn("classif.catboost", predict_type = "prob")


# Meta-Learner for Stacking(The learner that averages out all the stacked learners) 
meta_learner <- lrn("classif.glmnet", predict_type = "prob", alpha = .2, lambda = .0096)



# Create Stacking Pipeline with Tuned Meta-Learner
stacking_pipeline <- gunion(list(
  po("learner_cv", learner_bart, predict_type = "prob", id = "bart"),
  po("learner_cv", learner_lgb, predict_type = "prob", id = "lgb"),
  po("learner_cv", learner_lgb1, predict_type = "prob", id = "lgb1"),
  po("learner_cv", learner_lgb2, predict_type = "prob", id = "lgb2"),
  po("learner_cv", learner_lgb3, predict_type = "prob", id = "lgb3"),
  po("learner_cv", learner_lgb4, predict_type = "prob", id = "lgb4"),
  po("learner_cv", learner_lgb5, predict_type = "prob", id = "lgb5"),
  po("learner_cv", learner_lgb6, predict_type = "prob", id = "lgb6"),
  po("learner_cv", learner_lgb7, predict_type = "prob", id = "lgb7"),
  po("learner_cv", learner_lgb8, predict_type = "prob", id = "lgb8"),
  po("learner_cv", learner_lgb9, predict_type = "prob", id = "lgb9"),
  po("learner_cv", learner_lgb10, predict_type = "prob", id = "lgb10"),
  po("learner_cv", learner_lgb11, predict_type = "prob", id = "lgb11"),
  po("learner_cv", learner_lgb12, predict_type = "prob", id = "lgb12"),
  po("learner_cv", learner_lgb13, predict_type = "prob", id = "lgb13"),
  po("learner_cv", learner_cat, predict_type = "prob", id = "cat"),
  po("learner_cv", learner_xgb, predict_type = "prob", id = "xgb"),    
  po("learner_cv", learner_glmnet, predict_type = "prob", id = "enet"),
  po("learner_cv", learner_glmnet1, predict_type = "prob", id = "enet1"),
  po("learner_cv", learner_glmnet2, predict_type = "prob", id = "enet2"),
  po("learner_cv", learner_rf, predict_type = "prob", id = "rf"),
  po("learner_cv", learner_rf1, predict_type = "prob", id = "rf1"),
  po("learner_cv", learner_rf2, predict_type = "prob", id = "rf2"),
  po("learner_cv", learner_rf3, predict_type = "prob", id = "rf3"),
  po("learner_cv", learner_rf4, predict_type = "prob", id = "rf4"),
  po("learner_cv", learner_rf5, predict_type = "prob", id = "rf5")
  
  
)) %>>%
  po("featureunion") %>>%       # Combines all the learners
  po("learner", meta_learner, id = "learner")     

# Wrap the pipeline in GraphLearner
graph_learner <- as_learner(stacking_pipeline)
graph_learner$train(train_task_encoded)

# Predict and calculate log loss on the test task
predictions <- graph_learner$predict(test_task_encoded)
combined_features <- as.data.table(predictions)

# Extract probabilities for positive class (even though they are already binary mlr3 makes you convert them)
prob_matrix <- as.matrix(combined_features[, c("prob.0", "prob.1")])
colnames(prob_matrix) <- c("0", "1")
true_labels <- factor(combined_features[["truth"]], levels = c("0", "1"))

# Calculate log loss
log_loss <- mlr3measures::logloss(prob = prob_matrix, truth = true_labels)
print(log_loss)

# Preprocess the test data as before
test_prepared <- missRanger(test_prepared, maxiter = 5, num.trees = 200, pmm.k = 5)
mice_methods <- make.method(test_prepared)
numeric_cols <- names(test_prepared)[sapply(test_prepared, is.numeric)]
categorical_cols <- names(test_prepared)[sapply(test_prepared, is.factor)]
mice_methods[numeric_cols] <- "norm.predict"
mice_methods[categorical_cols] <- "logreg"
test_prepared <- complete(mice(test_prepared, m = 15, method = mice_methods))

# Ensure only selected columns are kept from Boruta without rerunning it
test_prepared <- test_prepared[, columns_to_keep, drop = FALSE]

# One-hot encoding for test data
test_task <- Task$new(id = "readmission_test", backend = test_prepared, task_type = "unsupervised")
test_task_encoded <- onehot$predict(list(test_task))[[1]]

# Align the test data with train data columns (ignore the error in truth, if it's not there it doesn't work for some reason)
test_data_dt <- as.data.table(test_task_encoded$data())
missing_cols <- setdiff(names(train_task_encoded$data()), names(test_data_dt))
for (col in missing_cols) {
  test_data_dt[[col]] <- 0
}
test_data_dt[is.na(test_data_dt)] <- 0
test_data_dt[, "readmitted" := NULL]
test_data_dt[, "truth" := NULL]

# Align data 
test_task_aligned <- Task$new(id = "readmission_test_aligned", backend = test_data_dt, task_type = "unsupervised")

# Predict on test data
predictions <- graph_learner$predict_newdata(test_task_aligned$data())
combined_features_final <- as.data.table(predictions)

# PatientID and probabilities
patientID <- test_original$patientID
predReadmit <- combined_features_final[["prob.1"]]

# Create dataframe and export the CSV
results <- data.frame(patientID = patientID, predReadmit = predReadmit)
write.csv(results, "predictions", row.names = FALSE)

# Confirm  of CSV creation
print("CSV file created successfully.")