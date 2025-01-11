library(tidyverse)
library(gridExtra)
library(VIM)
library(e1071)
library(caret)
library(car)
library(MASS)
library(pls)
library(glmnet)
library(mice)

#################################################
# Loading and Preparing the Train Dataset
#################################################

# Step 1: Load Data and Prepare Numeric Data
Train <- read_csv("/Users/vignesh/RStudio/IDA_Homework/IDA_HW_7/2024-dsa-ise-ida-classification-hw-7/hm7-Train-2024.csv", show_col_types = FALSE)
Train <- as_tibble(Train)
glimpse(Train)

colSums(is.na(Train))

duplicates <- Train[duplicated(Train), ]
nrow(duplicates)  # Number of duplicate rows

#################################################
# Converting Selected Variables to Character Type
#################################################

Train <-  Train %>% 
  mutate(across(c(admission_type, discharge_disposition, admission_source, readmitted, patientID), as.character))
glimpse(Train)

#################################################
# Selecting Numeric Data, Computing Summary Statistics, and Imputing Missing Values
#################################################

# Numeric Data Selection
Train_Numeric <- Train %>% select_if(is.numeric)
glimpse(Train_Numeric)

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
numericSummary <- Train_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min","Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("time_in_hospital":"number_diagnoses", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n), unique = as.numeric(unique), missing = as.numeric(missing),
         missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, 
                unique, unique_pct, everything())
numericSummary

#################################################
# Imputing Missing Values using MICE Imputation
#################################################

imputed_data <- mice(Train_Numeric, m = 5, method = 'pmm', seed = 123)
Train_Numeric <- complete(imputed_data)

numericSummary <- Train_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min","Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("time_in_hospital":"number_diagnoses", names_to = "variable", values_to = "value") %>%
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

map2(Train_Numeric, names(Train_Numeric), plot_histogram)

plot_list0 <- map2(Train_Numeric, names(Train_Numeric), plot_histogram)

#################################################
# Handling Skewness of Numeric variables using BoxCoxTrans from CARET Package
#################################################

# Computing the skewness of variables and Sorting them
skewValues_tibble <- Train_Numeric %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness") %>%
  arrange(Skewness)
skewValues_tibble

BoxCoxTrans(Train_Numeric$number_emergency+1)
BoxCoxTrans(Train_Numeric$number_outpatient+1)
BoxCoxTrans(Train_Numeric$number_inpatient+1)
BoxCoxTrans(Train_Numeric$num_procedures+1)
BoxCoxTrans(Train_Numeric$num_medications+1)

hist((Train_Numeric$number_emergency+1)**(-2))
hist((Train_Numeric$number_emergency+1)**(-2))
hist((Train_Numeric$number_inpatient+1)**(-2))
hist((Train_Numeric$num_procedures+1)**(-0.6))
hist((Train_Numeric$num_medications+1)**(0.3))

Train_Numeric$number_emergency <- (Train_Numeric$number_emergency+1)**(-2)
Train_Numeric$number_outpatient <- (Train_Numeric$number_emergency+1)**(-2)
Train_Numeric$number_inpatient <- (Train_Numeric$number_inpatient+1)**(-2)
Train_Numeric$num_procedures <- (Train_Numeric$num_procedures+1)**(-0.6)
Train_Numeric$num_medications <- (Train_Numeric$num_medications+1)**(0.3)

#################################################
# Numeric variable Centering and Scaling using Caret Package
#################################################

preProcess_obj <- preProcess(Train_Numeric, 
                             method = c("center", "scale"))

# Transform the data using the preprocessing object
Train_Numeric <- predict(preProcess_obj, Train_Numeric)
head(Train_Numeric)

#################################################
# FACTORS
#################################################

# Function to compute mode for Factor data
getmodes <- function(v, type = 1) {
  tbl <- table(v)
  m1 <- which.max(tbl)
  if (type == 1) {
    return(names(m1))  # 1st mode
  } else if (type == 2) {
    if (length(tbl) < 2) return(NA)  # No 2nd mode if only one unique value
    return(names(which.max(tbl[-m1])))  # 2nd mode
  } else if (type == -1) {
    return(names(which.min(tbl)))  # least common mode
  } else {
    stop("Invalid type selected")
  }
}

# Function to compute the count of mode for Factor data
getmodesCnt <- function(v, type = 1) {
  tbl <- table(v)
  m1 <- which.max(tbl)
  if (type == 1) {
    return(max(tbl))  # 1st mode frequency
  } else if (type == 2) {
    if (length(tbl) < 2) return(NA)  # No 2nd mode if only one unique value
    return(max(tbl[-m1]))  # 2nd mode frequency
  } else if (type == -1) {
    return(min(tbl))  # least common frequency
  } else {
    stop("Invalid type selected")
  }
}

# Function to set Summary Functions for Factor Data
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

#################################################
# Selecting Factor Data, Computing Summary Statistics
#################################################

# Step 1: Extract Factor Variables
Train_Factor <- Train %>% 
  select_if(is.character) %>% 
  transmute_if(is.character, as.factor)  # Convert character to factor

names(Train_Factor)
glimpse(Train_Factor)

# Compute Summary of Factor Data
FactorSummary <- Train_Factor %>%
  reframe(across(everything(), myFactorSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "most_common", "most_common_count", 
                 "2nd_most_common", "2nd_most_common_count", "least_common", 
                 "least_common_count")) %>%
  pivot_longer("patientID":"readmitted", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    n = as.numeric(n),
    unique = as.numeric(unique),
    missing = as.numeric(missing),
    missing_pct = 100 * missing / n,
    unique_pct = 100 * unique / n
  ) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())
FactorSummary

#################################################
# Plotting Factor Variables with Barplots with NA
#################################################

plot_barplot1 <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data), aes(x = value, fill = value)) +
    geom_bar() +
    labs(title = paste("Barplot of", column_name), x = column_name, y = "Frequency")+
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend if not needed
}

map2(Train_Factor %>% dplyr::select(-patientID), names(Train_Factor %>% dplyr::select(-patientID)), plot_barplot1)

#################################################
# Plotting Factor Variables by `readmitted` Variable with NA
#################################################

plot_barplot2 <- function(column_data, readmitted_data, column_name) {
  ggplot(
    data = tibble(value = column_data, readmitted = readmitted_data), 
    aes(x = value, fill = readmitted)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Barplot of", column_name, "by Readmitted"), x = column_name, y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")
}

map2(
  Train_Factor %>% dplyr::select(-c(readmitted, patientID)),          # Select all columns except 'readmitted'
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))),    # Get names of selected columns
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)       # Apply the custom plotting function
)


plot_list1 <- map2(
  Train_Factor %>% dplyr::select(-c(patientID, readmitted)), 
  names(Train_Factor %>% dplyr::select(-c(patientID, readmitted))), 
  plot_barplot1
)

plot_list2 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted, patientID)), 
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))), 
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)
)

# Arrange each pair of plots side by side using grid.arrange
combined_plots <- map2(plot_list1, plot_list2, ~ grid.arrange(.x, .y, ncol = 2))
combined_plots

#################################################
# Imputing Missing Factor Data Using Hotdeck Method from VIM Package
#################################################

Train_Factor <- hotdeck(Train_Factor,imp_var=FALSE)

# Compute Summary of Factor Data
FactorSummary <- Train_Factor %>%
  reframe(across(everything(), myFactorSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "most_common", "most_common_count", 
                 "2nd_most_common", "2nd_most_common_count", "least_common", 
                 "least_common_count")) %>%
  pivot_longer("race":"readmitted", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    n = as.numeric(n),
    unique = as.numeric(unique),
    missing = as.numeric(missing),
    missing_pct = 100 * missing / n,
    unique_pct = 100 * unique / n
  ) %>%
  dplyr::select(variable, most_common, "2nd_most_common",least_common, 
                most_common_count, "2nd_most_common_count", least_common_count,everything())

#################################################
# Combining and Displaying Plots Side by Side
#################################################

# Plotting again after hotdeck imputing
plot_list1 <- map2(
  Train_Factor %>% dplyr::select(-c(patientID, readmitted)), 
  names(Train_Factor %>% dplyr::select(-c(patientID, readmitted))), 
  plot_barplot1
)

plot_list2 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted, patientID)), 
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))), 
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)
)

# Arrange each pair of plots side by side using grid.arrange
combined_plots <- map2(plot_list1, plot_list2, ~ grid.arrange(.x, .y, ncol = 2))
combined_plots

#################################################
# Plotting Factor Variables by `readmitted` Proportions
#################################################

# Function to plot the relationship between each factor variable and `readmitted` as proportions
plot_readmitted_relationship_proportion <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data, readmitted = Train_Factor$readmitted),
         aes(x = value, fill = readmitted)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste("Proportion of Readmitted by", column_name),
         x = column_name, y = "Proportion") +
    theme_minimal()
}

# Plot for each factor column against `readmitted`, excluding `readmitted` itself
map2(
  Train_Factor %>% dplyr::select(-c(readmitted,patientID)),          # Select all columns except 'readmitted'
  names(Train_Factor %>% dplyr::select(-c(readmitted,patientID))),    # Get names of selected columns
  ~ plot_readmitted_relationship_proportion(.x, .y)      # Apply the custom plotting function
)

#################################################
# Combining and Displaying Plots Side by Side
#################################################

# Plotting again after hotdeck imputing
plot_list1 <- map2(
  Train_Factor %>% dplyr::select(-c(patientID, readmitted)),
  names(Train_Factor %>% dplyr::select(-c(patientID, readmitted))),
  plot_barplot1
)

plot_list2 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted, patientID)),
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))),
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)
)

plot_list3 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted,patientID)),          # Select all columns except 'readmitted'
  names(Train_Factor %>% dplyr::select(-c(readmitted,patientID))),    # Get names of selected columns
  ~ plot_readmitted_relationship_proportion(.x, .y)      # Apply the custom plotting function
)

combined_plots <- pmap(list(plot_list1, plot_list2, plot_list3),
                       ~ grid.arrange(..1, ..2, ..3, ncol = 3))
combined_plots


lapply(Train_Factor, table)

glimpse(Train_Factor)

#################################################
# Feature Selection of Factor Variables
#################################################

# Factor feature selection
Train_Factor <- Train_Factor %>%
  dplyr::select(c(patientID,readmitted,medical_specialty,diagnosis))

glimpse(Train_Factor)
lapply(Train_Factor, table)

#################################################
# Combining and Displaying Plots Side by Side
#################################################

# Plotting again after feature selection
plot_list1 <- map2(
  Train_Factor %>% dplyr::select(-c(patientID, readmitted)),
  names(Train_Factor %>% dplyr::select(-c(patientID, readmitted))),
  plot_barplot1
)

plot_list2 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted, patientID)),
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))),
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)
)

plot_list3 <- map2(
  Train_Factor %>% dplyr::select(-c(readmitted,patientID)),          # Select all columns except 'readmitted'
  names(Train_Factor %>% dplyr::select(-c(readmitted,patientID))),    # Get names of selected columns
  ~ plot_readmitted_relationship_proportion(.x, .y)      # Apply the custom plotting function
)

combined_plots <- pmap(list(plot_list1, plot_list2, plot_list3),
                       ~ grid.arrange(..1, ..2, ..3, ncol = 3))


# Print Factor Levels of each factor variable
factor_levels <- map(Train_Factor, ~fct_count(.x, sort = TRUE))
# Print each element in the list with a limit on levels displayed
map(factor_levels, ~print(.x, n = 50))

#################################################
# Factor Collapsing
#################################################

glimpse(Train_Factor)
lapply(Train_Factor, table)

Train_Factor_Collapsed <- Train_Factor
Train_Factor_Collapsed$medical_specialty <- fct_lump_n(Train_Factor$medical_specialty, n= 19)
Train_Factor_Collapsed$diagnosis <- fct_lump_n(Train_Factor$diagnosis, n= 19)

glimpse(Train_Factor_Collapsed)
lapply(Train_Factor_Collapsed, table)

#################################################
# Combining and Displaying Plots Side by Side
#################################################

# Plotting again after feature selection
plot_list1 <- map2(
  Train_Factor_Collapsed %>% dplyr::select(-c(patientID, readmitted)),
  names(Train_Factor %>% dplyr::select(-c(patientID, readmitted))),
  plot_barplot1
)

plot_list2 <- map2(
  Train_Factor_Collapsed %>% dplyr::select(-c(readmitted, patientID)),
  names(Train_Factor %>% dplyr::select(-c(readmitted, patientID))),
  ~ plot_barplot2(.x, Train_Factor$readmitted, .y)
)

plot_list3 <- map2(
  Train_Factor_Collapsed %>% dplyr::select(-c(readmitted,patientID)),          # Select all columns except 'readmitted'
  names(Train_Factor %>% dplyr::select(-c(readmitted,patientID))),    # Get names of selected columns
  ~ plot_readmitted_relationship_proportion(.x, .y)      # Apply the custom plotting function
)

combined_plots <- pmap(list(plot_list1, plot_list2, plot_list3),
                       ~ grid.arrange(..1, ..2, ..3, ncol = 3))


#################################################
# Plotting the Mean of Numeric Variables by `readmitted`
#################################################

# Function to plot the average of each numeric variable by `readmitted` using bar plots
plot_mean_relationship_to_readmitted <- function(column_data, column_name) {
  # Calculate the mean for each level of `readmitted`
  data <- tibble(value = column_data, readmitted = Train_Factor$readmitted) %>%
    group_by(readmitted) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))
  
  # Create bar plot for the mean
  ggplot(data, aes(x = readmitted, y = mean_value, fill = readmitted)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Average", column_name, "by Readmitted"), 
         x = "Readmitted", y = paste("Average", column_name)) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend if not needed
}

map2(
  Train_Numeric,                       # Select all numeric columns
  names(Train_Numeric),                 # Get names of numeric columns
  ~ plot_mean_relationship_to_readmitted(.x, .y)  # Apply the custom plotting function
)

#################################################
# Plotting the Distribution of Numeric Variables by `readmitted` Using Boxplots
#################################################

plot_boxplot_relationship_to_readmitted <- function(column_data, column_name) {
  # Create boxplot for each numeric variable by `readmitted`
  ggplot(data = tibble(value = column_data, readmitted = Train_Factor$readmitted), 
         aes(x = readmitted, y = value, fill = readmitted)) +
    geom_boxplot() +  # Create boxplot
    labs(title = paste("Distribution of", column_name, "by Readmitted"), 
         x = "Readmitted", 
         y = column_name) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend if not needed
}

map2(
  Train_Numeric,                       # Select all numeric columns
  names(Train_Numeric),                 # Get names of numeric columns
  ~ plot_boxplot_relationship_to_readmitted(.x, .y)  # Apply the custom plotting function
)

#################################################
# Plotting the Scatter plot of Numeric Variables by `readmitted`
#################################################

plot_scatter <- function(column_data, column_name) {
  data <- tibble(value = column_data, readmitted = Train_Factor$readmitted)
  
  ggplot(data, aes(x = readmitted, y = value, color = readmitted)) +
    geom_jitter(alpha = 0.5, width = 0.1) +
    labs(title = paste("Scatter Plot of", column_name, "by Readmitted"),
         x = "Readmitted", y = column_name) +
    theme_minimal()
}

map2(
  Train_Numeric,                       
  names(Train_Numeric),                 
  ~ plot_scatter(.x, .y)  
)


# Plot the average for each numeric column against `readmitted`
plot_list4 <- map2(
  Train_Numeric,                       # Select all numeric columns
  names(Train_Numeric),                 # Get names of numeric columns
  ~ plot_scatter(.x, .y)  # Apply the custom plotting function
)

#################################################
# Plotting Histograms of Numeric Variables by `readmitted`
#################################################

plot_histogram_by_factor <- function(column_data, factor_data, column_name, factor_name) {
  ggplot(data = tibble(value = column_data, factor = factor_data), aes(x = value, fill = factor)) +
    geom_histogram(position = "dodge", color = "black", bins = 30, width = 0.8) +  # Adjust width for spacing
    scale_fill_manual(values = c("0" = "lightcoral", "1" = "cadetblue2"), 
                      name = "Readmitted") +
    labs(title = paste("Histogram of", column_name, "by", factor_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "right")  # Optional: Adjust legend position if necessary
}

plot_list5 <- map(names(Train_Numeric), 
                 ~ plot_histogram_by_factor(Train_Numeric[[.]], Train_Factor$readmitted, . , "readmitted"))
plot_list5

#################################################
# Combining and Displaying Plots Side by Side
#################################################

combined_plots <- pmap(list(plot_list0, plot_list5, plot_list4),
                       ~ grid.arrange(..1, ..2, ..3, ncol = 3))
combined_plots

#################################################
# Feature Selection: Dropping Specific Variables from Train_Numeric
#################################################

names(Train_Numeric)

Train_Numeric <- Train_Numeric %>%
  dplyr::select(-c(indicator_level,indicator_2_level))
names(Train_Numeric)

#################################################
# Merging Data and Dropping patientID Column
#################################################

Final_Data <- bind_cols(Train_Numeric, Train_Factor_Collapsed)  # Join numeric and factor data
Final_Data <- Final_Data %>% 
  dplyr::select(-patientID)

names(Final_Data)

colSums(is.na(Final_Data))
glimpse(Final_Data)
summary(Final_Data)

##################
# Modelling
#################

#################################################
# Convert Levels for `readmitted`
#################################################

# Convert `readmitted` levels to "No" and "Yes" if they are currently "0" and "1"
Final_Data$readmitted <- factor(Final_Data$readmitted, 
                                levels = c("0", "1"),  # Original levels
                                labels = c("No", "Yes"))  # New levels

glimpse(Final_Data)
levels(Final_Data$readmitted)
table(Final_Data$readmitted)
colSums(is.na(Final_Data))
summary(Final_Data)
cor_matrix <- cor(Final_Data[ , sapply(Final_Data, is.numeric)])
corrplot::corrplot(cor_matrix,method = "color")


#################################################
# Balancing the target variable
#################################################
head(Final_Data)

# Separate the predictors and the target variable
x <- Final_Data %>% dplyr::select(-readmitted)
y <- Final_Data$readmitted

# Perform upsampling
Final_Data <- downSample(x, y)
head(Final_Data)

# Check the distribution of the new balanced dataset
table(Final_Data$Class)

Final_Data <- Final_Data %>% rename(readmitted = Class)
head(Final_Data)

#################################################
# Balancing the target variable - Tried using SMOTE , BUt failed
#################################################

# library(smotefamily)
# 
# Final_Data$readmitted <- ifelse(Final_Data$readmitted == "Yes", 1, 0)
# Final_Data$readmitted <- as.numeric(Final_Data$readmitted)
# Final_Data$diagnosis <- as.numeric(as.factor(Final_Data$diagnosis))
# Final_Data$medical_specialty <- as.numeric(as.factor(Final_Data$medical_specialty))
# head(Final_Data)
# table(Final_Data$readmitted)
# glimpse(Final_Data)
# 
# 
# smote_train <- SMOTE(X = Final_Data, target = Final_Data$readmitted)
# head(smote_train$syn_data)

#################################################
# Train Control Setup
#################################################

# Train Controls
train_control <- trainControl(method = "cv", number = 10, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary, 
                              allowParallel = TRUE)


# Logistic Regression Model
#################################################

# Logistic
logreg_model <- train(readmitted ~ ., 
                      data = Final_Data, 
                      method = "glm", 
                      family = "binomial", 
                      trControl = train_control,
                      metric = "ROC")
logreg_model
varImp(logreg_model)

# Lasso Regression Model (Initial and Tuned)
#################################################

# Lasso
lasso_model <- train(readmitted ~ ., 
                     data = Final_Data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneLength = 20,
                     metric = "ROC")
lasso_model
plot(lasso_model)

lasso_grid <- expand.grid(alpha = 1, 
                          lambda = seq(0.0001, 0.01, by = 0.0001))
lasso_model <- train(readmitted ~ ., 
                     data = Final_Data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = lasso_grid,
                     metric = "ROC")
lasso_model
plot(lasso_model)
varImp(lasso_model)

# Multivariate Adaptive Regression Splines (MARS)
#################################################

# MARS
mars_model <- train(readmitted ~ ., 
                     data = Final_Data, 
                     method = "earth", 
                     trControl = train_control, 
                     tuneLength = 20,
                     metric = "ROC")
mars_model


mars_grid <- expand.grid(degree = c(1, 2, 3), 
                         nprune = seq(10, 20, by = 1))
mars_model <- train(readmitted ~ ., 
                    data = Final_Data, 
                    method = "earth", 
                    trControl = train_control, 
                    tuneGrid = mars_grid,
                    metric = "ROC")
mars_model
plot(mars_model)
varImp(mars_model)

#################################################
# Parallel Processing Setup - Used when required
#################################################

library(doParallel)

# Detect number of cores and set up cluster
num_cores <- detectCores() - 2  # Leave one core free
cl <- makeCluster(num_cores)
cl
registerDoParallel(cl)

# Stop the cluster after training
stopCluster(cl)
registerDoSEQ()

# Random Forest Model (Initial and Tuned)
#################################################

rf_model <- train(readmitted ~ ., 
                  data = Final_Data, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneLength = 20,
                  metric = "ROC",
                  verbose = TRUE)

rf_grid <- expand.grid(mtry = 4)
rf_model <- train(readmitted ~ ., 
                  data = Final_Data, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneGrid = rf_grid,
                  metric = "ROC",
                  verbose = TRUE)
rf_model
varImp(rf_model)

# XGBoost
####################################

xgb_model <- train(readmitted ~ ., 
                   data = Final_Data, 
                   method = "xgbTree", 
                   trControl = train_control,
                   metric = "ROC",
                   tuneGrid = expand.grid(nrounds = 150, max_depth = 6, 
                                          eta = 0.3, gamma = 0, 
                                          colsample_bytree = 0.6, 
                                          min_child_weight = 1, 
                                          subsample = 1))

# xgb_model <- train(readmitted ~ ., 
#                    data = Final_Data, 
#                    method = "xgbTree", 
#                    trControl = train_control,
#                    metric = "ROC",
#                    tuneLength = 10)

xgb_model
varImp(xgb_model)

# Neural network
####################################
library(nnet)

train_control <- trainControl(method = "cv", number = 10, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary, 
                              allowParallel = TRUE)

# Neural Network Model
nn_model <- train(readmitted ~ ., 
                  data = Final_Data, 
                  method = "nnet", 
                  trControl = train_control,
                  metric = "ROC",
                  tuneGrid = expand.grid(size = c(6,7,8), decay = c(0.8)))
?nnet
nn_model

# Model Performance Comparison
#################################################

performence_comp <- resamples(list(MARS = mars_model,
               RandForest = rf_model,
               Lasso = lasso_model,
               Logistic = logreg_model))
summary(performence_comp)

#################################################
# Log Loss Calculation 
#################################################

log_loss_results <- list()

log_loss <- function(y_true, y_pred) {
  epsilon <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - epsilon), epsilon)
  -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

head(Final_Data)

# Convert `readmitted` to numeric for log loss calculation
y_true <- ifelse(xgb_model$trainingData$.outcome == "Yes", 1, 0)

rf_pred <- predict(xgb_model, Final_Data, type = "prob")[, "Yes"]
log_loss_results$rf <- log_loss(y_true, rf_pred)
print(log_loss_results)

############
# Testing
############

Test <- read_csv("/Users/vignesh/RStudio/IDA_Homework/IDA_HW_7/2024-dsa-ise-ida-classification-hw-7/hm7-Test-2024.csv", show_col_types = FALSE)
Test <- as_tibble(Test)
glimpse(Test)

sum(is.na(Test$patientID))
Test <- Test[!is.na(Test$patientID), ]

Test_duplicates <- Test[duplicated(Test$patientID), ]
nrow(Test_duplicates)

Test <- Test[!duplicated(Test$patientID), ]

names(Test)

Test <- Test %>%
  mutate(across(c(admission_type, discharge_disposition, admission_source, patientID), as.character))


Test_Numeric <- Test %>% select_if(is.numeric)
names(Test_Numeric)

# Step 2: Compute Summary of Numeric Data
numericSummary <- Test_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min","Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("time_in_hospital":"number_diagnoses", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n), unique = as.numeric(unique), missing = as.numeric(missing),
         missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, 
                unique, unique_pct, everything())

imputed_data <- mice(Test_Numeric, m = 5, method = 'pmm', seed = 123)
Test_Numeric <- complete(imputed_data)
names(Test_Numeric)

Test_Numeric$number_emergency <- (Test_Numeric$number_emergency+1)**(-2)
Test_Numeric$number_outpatient <- (Test_Numeric$number_emergency+1)**(-2)
Test_Numeric$number_inpatient <- (Test_Numeric$number_inpatient+1)**(-2)
Test_Numeric$num_procedures <- (Test_Numeric$num_procedures+1)**(-0.6)
Test_Numeric$num_medications <- (Test_Numeric$num_medications+1)**(0.3)

#################################################
# Test Numeric variable Centering and Scaling
#################################################

preProcess_obj <- preProcess(Test_Numeric, 
                             method = c("center", "scale"))

# Transform the data using the preprocessing object
Test_Numeric <- predict(preProcess_obj, Test_Numeric)
head(Test_Numeric)

#################################################
# Test Numeric variable Feature Selection
#################################################

Test_Numeric <- Test_Numeric %>%
  dplyr::select(-c(indicator_level,indicator_2_level))

####################################################################

# Step 1: Extract Factor Variables
Test_Factor <- Test %>% 
  select_if(is.character) %>% 
  transmute_if(is.character, as.factor)

names(Test_Factor)
glimpse(Test_Factor)

# Compute Summary of Factor Data
FactorSummary <- Test_Factor %>%
  reframe(across(everything(), myFactorSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "most_common", "most_common_count", 
                 "2nd_most_common", "2nd_most_common_count", "least_common", 
                 "least_common_count")) %>%
  pivot_longer("patientID":"diabetesMed", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    n = as.numeric(n),
    unique = as.numeric(unique),
    missing = as.numeric(missing),
    missing_pct = 100 * missing / n,
    unique_pct = 100 * unique / n
  ) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

Test_Factor <- hotdeck(Test_Factor, imp_var = FALSE)

Test_Factor <- Test_Factor %>%
  dplyr::select(c(patientID, medical_specialty,diagnosis))
head(Test_Factor)

Test_Factor_patientID <- Test_Factor %>%
  dplyr::select(c(patientID))
head(Test_Factor_patientID)

#################################################
# Train_Factor_Collapsed <- Train_Factor
# # print(fct_count(Train_Factor_Collapsed$diagnosis, sort = TRUE), n = Inf)
# 
# Train_Factor_Collapsed$medical_specialty <- fct_lump_n(Train_Factor$medical_specialty, n=69)
# Train_Factor_Collapsed$diagnosis <- fct_lump_n(Train_Factor$diagnosis, n= 667)
#################################################

Test_Factor_Collapsed <- Test_Factor
head(Test_Factor_Collapsed)
# print(fct_count(Test_Factor_Collapsed$diagnosis, sort = TRUE), n = Inf)

Test_Factor_Collapsed$medical_specialty <- fct_lump_n(Test_Factor_Collapsed$medical_specialty, n= 19)
Test_Factor_Collapsed$diagnosis <- fct_lump_n(Test_Factor_Collapsed$diagnosis, n= 19)

lapply(Train_Factor_Collapsed, levels)
lapply(Test_Factor_Collapsed, levels)

Test_Final_Data <- bind_cols(Test_Numeric, Test_Factor_Collapsed) 
head(Test_Final_Data)

Test_Final_Data_patient_ID <- Test_Final_Data %>% 
  dplyr::select(patientID)
head(Test_Final_Data_patient_ID)

Test_Final_Data <- Test_Final_Data %>% 
  dplyr::select(-patientID)
head(Test_Final_Data)

##################
# Modelling
#################

test_pred <- predict(xgb_model, Test_Final_Data, type = "prob")[, "Yes"]
test_pred

test_df <- tibble(predReadmit = test_pred)
test_df <- test_df %>% 
  mutate(patientID = Test_Final_Data_patient_ID$patientID) %>% 
  dplyr::select(patientID,predReadmit)
head(test_df)


# Optionally, save the tibble to a CSV file
write_csv(test_df, "XGBoost_predictions_Nov_11_v2.csv")

