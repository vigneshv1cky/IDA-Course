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

# Step 1: Load Data and Prepare Numeric Data
Train <- read_csv("2024-ise-dsa-5103-ida-hw-6/Train.csv/Train.csv", show_col_types = FALSE)
Train <- as_tibble(Train)

# Numeric Data Selection
Train_Numeric <- Train %>% select_if(is.numeric)

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
  pivot_longer("sessionId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n), unique = as.numeric(unique), missing = as.numeric(missing),
    missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, 
                unique, unique_pct, everything())

# Step 3: Remove Unwanted Columns
Train_Numeric <- Train_Numeric %>% 
  dplyr::select(-c(adwordsClickInfo.page, bounces, newVisits, sessionId))

# Compute Summary of Numeric Data
numericSummary <- Train_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min", "Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("custId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n),unique = as.numeric(unique),missing = as.numeric(missing),
    missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, 
                unique_pct, everything())

# Step 4: Impute Missing Values
Train_Numeric <- kNN(Train_Numeric, variable = c("pageviews"), k = 5, imp_var = FALSE)

# Recompute Summary of Numeric Data
numericSummary <- Train_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min", "Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("custId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n),unique = as.numeric(unique),missing = as.numeric(missing),
         missing_pct = 100 * missing / n, unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, 
                unique_pct, everything())

# Step 5: Plot Histograms
plot_histogram <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data), aes(x = value)) +
    geom_histogram() +
    labs(title = paste("Histogram of", column_name), x = column_name,
         y = "Frequency")
}

map2(Train_Numeric, names(Train_Numeric), plot_histogram)

# # Step 6: Scatter Plots
# ggplot(data = Train_Numeric, aes(x = visitStartTime, y = revenue)) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Visit Start Time", 
#        x = "Visit Start Time", y = "Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = pageviews, y = revenue)) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Pageviews", 
#        x = "Pageviews", y = "Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = timeSinceLastVisit, y = revenue)) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Time Since Last Visit", 
#        x = "Time Since Last Visit", y = "Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = visitNumber, y = revenue)) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Visit Number", 
#        x = "Visit Number", y = "Revenue")
# 
# ggplot(data = Train_Numeric, aes(x = visitStartTime, y = log(revenue+1))) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Visit Start Time", 
#        x = "Visit Start Time", y = "log Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = pageviews, y = log(revenue+1))) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Pageviews", 
#        x = "Pageviews", y = "log Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = timeSinceLastVisit, y = log(revenue+1))) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Time Since Last Visit", 
#        x = "Time Since Last Visit", y = "log Revenue")
# 
# ggplot(data = Train_Numeric[-39511,], aes(x = visitNumber, y = log(revenue+1))) +
#   geom_point(alpha = 0.5) +
#   labs(title = "Scatter Plot of Revenue vs Visit Number", 
#        x = "Visit Number", y = "log Revenue")
# 
# ggplot(data =Train_Numeric , aes(x = log(revenue+1))) +
#   geom_histogram()


#################################################
# FACTORS
#################################################

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

# Function to set Summary Functions for Factor Data
myFactorSummary <- function(x) {
  c(length(x), n_distinct(x), sum(is.na(x)), getmodes(x, type = 1),
    getmodesCnt(x, type = 1), getmodes(x, type = 2), getmodesCnt(x, type = 2),
    getmodes(x, type = -1), getmodesCnt(x, type = -1))
}

# Step 1: Extract Factor Variables
Train_Factor <- Train %>% 
  select_if(is.character) %>% 
  transmute_if(is.character, as.factor)  # Convert character to factor

names(Train_Factor)

# Step 2: Summarize Missing Data
Factor_missing_summary <- Train_Factor %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

# Step 3: Remove Unwanted Columns
Train_Factor <- Train_Factor %>% 
  dplyr::select(-c(adContent, adwordsClickInfo.slot, 
                   adwordsClickInfo.adNetworkType, adwordsClickInfo.adNetworkType,
                   adwordsClickInfo.gclId, keyword, campaign, metro, 
                   referralPath, city, region, networkDomain, topLevelDomain))

# Step 4: Plot Barplots
plot_barplot <- function(column_data, column_name) {
  ggplot(data = tibble(value = column_data), aes(x = value)) +
    geom_bar() +
    labs(title = paste("Barplot of", column_name), x = column_name,
         y = "Frequency")
}

# Plot barplots for each factor variable
map2(Train_Factor, names(Train_Factor), plot_barplot)

# Function to create bar plots for average revenue per level of factor variables
plot_avg_revenue <- function(column_data, column_name) {
  Train_Factor_Revenue %>%
    group_by(!!sym(column_name)) %>%
    summarise(avg_revenue = mean(revenue, na.rm = TRUE)) %>%
    ggplot(aes(x = .data[[column_name]], y = avg_revenue)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
    labs(title = paste("Average Revenue by", column_name), x = column_name, y = "Average Revenue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}

# Plot each factor variable against average revenue
map(names(factor_columns), ~ plot_avg_revenue(factor_columns[[.x]], .x))

# Print Factor Levels of each factor variable
factor_levels <- map(Train_Factor, ~fct_count(.x, sort = TRUE))
factor_levels

# Collapse factor levels
Train_Factor$channelGrouping <- fct_lump_n(Train_Factor$channelGrouping , n = 4)
fct_count(Train_Factor$channelGrouping)
Train_Factor$browser <- fct_lump_n(Train_Factor$browser , n = 2)
fct_count(Train_Factor$browser)
Train_Factor$operatingSystem <- fct_lump_n(Train_Factor$operatingSystem , n = 6)
fct_count(Train_Factor$operatingSystem)
Train_Factor$source <- fct_lump_n(Train_Factor$source , n = 4)
fct_count(Train_Factor$source)
Train_Factor$medium <- fct_lump_n(Train_Factor$medium , n = 3)
fct_count(Train_Factor$medium)
Train_Factor$country <- fct_lump_n(Train_Factor$country , n = 1)
fct_count(Train_Factor$country)
Train_Factor$subContinent <- fct_lump_n(Train_Factor$subContinent , n = 1)
fct_count(Train_Factor$subContinent)
Train_Factor$continent <- fct_lump_n(Train_Factor$continent , n = 3)
fct_count(Train_Factor$continent)

# Drop 'subContinent' as it will not be needed
Train_Factor <- Train_Factor %>% 
  dplyr::select(-subContinent)

# Step 6: Impute Missing Values
Train_Factor_imputed <- hotdeck(Train_Factor,imp_var=FALSE)

# Step 7: Visualize Barplots After Imputation
map2(Train_Factor_imputed, names(Train_Factor_imputed), plot_barplot)

# Comparison Barplot Before and After Imputation
p1 <- ggplot(data = Train_Factor_imputed, aes(x = medium)) +
  geom_bar() +
  ylim(c(0, 35000)) +
  labs(title = "Barplot after imputation")
p2 <- ggplot(data = Train_Factor, aes(x = medium)) +
  geom_bar() +
  ylim(c(0, 35000)) +
  labs(title = "Barplot before imputation")
grid.arrange(p1,p2, ncol=2)

# Step 8: Summary of Missing Data After Imputation
Factor_imputed_missing_summary <- Train_Factor_imputed %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))


#################################################
# Final
#################################################

Final_Data <- Train %>%
  dplyr::select(date) %>%                       # Select 'date' column
  bind_cols(Train_Numeric, Train_Factor_imputed)  # Join numeric and factor data

names(Final_Data)

colSums(is.na(Final_Data))

Final_Data <- Final_Data %>%
  mutate(
    day_of_week = wday(date, label = TRUE),  # Extract day of the week
    cust_lag_visit = lag(visitNumber, order_by = visitStartTime),  # Previous visit number
    consecutive_visits = ifelse(visitNumber == cust_lag_visit + 1, 1, 0)  # Check consecutive
  )

names(Final_Data)

# Group by custId and create new features
customer_features <- Final_Data %>%
  group_by(custId) %>%
  summarise(
    log_total_revenue = log1p(sum(revenue)),  # log(1 + total_revenue)
    total_visits = n(),
    avg_pageviews_per_visit = mean(pageviews),
    days_between_first_last_visit = as.numeric(max(date) - min(date)),
    different_day_visits = n_distinct(date),  # Number of unique visit days
    total_sessions = max(visitNumber),  # Total sessions per customer
    avg_session_num = mean(visitNumber),
    avg_time_since_last = mean(timeSinceLastVisit),
    max_time_since_last = max(timeSinceLastVisit),
    mobileUsage = mean(isMobile),
    directVisits = mean(isTrueDirect),
    pageviews = sum(pageviews),
    mostCommonBrowser = names(sort(table(browser), decreasing = TRUE)[1]),
    mostCommonOS = names(sort(table(operatingSystem), decreasing = TRUE)[1]),
    mostCommonDevice = names(sort(table(deviceCategory), decreasing = TRUE)[1]),
    primaryChannel = names(sort(table(channelGrouping), decreasing = TRUE)[1]),
    mostFrequentContinent = names(sort(table(continent), decreasing = TRUE)[1]),
    mostFrequentCountry = names(sort(table(country), decreasing = TRUE)[1]),
    primarySource = names(sort(table(source), decreasing = TRUE)[1]),
    primaryMedium = names(sort(table(medium), decreasing = TRUE)[1]),
    mostCommonDay = names(sort(table(day_of_week), decreasing = TRUE)[1])  # Most frequent day of visit
  )

names(customer_features)

#################################################
# Analysis of Customer Features
#################################################

# Select only numeric columns from customer_features dataset
customer_features_Numeric <- customer_features %>% select_if(is.numeric)

# Remove specific columns from the numeric dataset
customer_features_Numeric <- customer_features_Numeric %>%
  dplyr::select(-c(log_total_revenue, custId))

# Display the names of the remaining numeric columns
names(customer_features_Numeric)

# Compute Summary of Numeric Data
customer_features_numericSummary <- customer_features_Numeric %>%
  reframe(across(everything(), myNumericSummary)) %>%
  cbind(stat = c("n", "unique", "missing", "mean", "min", "Q1", "median", "Q3", "max", "sd")) %>%
  pivot_longer("total_visits":"total_visits", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(n = as.numeric(n), unique = as.numeric(unique),
         missing = as.numeric(missing), missing_pct = 100 * missing / n,
         unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, 
                unique, unique_pct, everything())

# Computing the skewness of variables and Sorting them
skewValues_tibble <- customer_features_Numeric %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness") %>%
  arrange(Skewness)

# Print skewness values for all variables
print(skewValues_tibble, n = Inf)

# Compute Box-Cox transformation to various numeric features to stabilize variance
BoxCoxTrans(customer_features_Numeric$mobileUsage+1)
BoxCoxTrans(customer_features_Numeric$directVisits+1)
BoxCoxTrans(customer_features_Numeric$avg_pageviews_per_visit)
BoxCoxTrans(customer_features_Numeric$days_between_first_last_visit+1)
BoxCoxTrans(customer_features_Numeric$max_time_since_last+1)
BoxCoxTrans(customer_features_Numeric$avg_time_since_last+1)
BoxCoxTrans(customer_features_Numeric$pageviews)
BoxCoxTrans(customer_features_Numeric$different_day_visits)
BoxCoxTrans(customer_features_Numeric$total_sessions)
BoxCoxTrans(customer_features_Numeric$avg_session_num)
BoxCoxTrans(customer_features_Numeric$total_visits)

# Plot histograms of the original numeric features
hist(customer_features_Numeric$mobileUsage)
hist(customer_features_Numeric$directVisits)
hist(customer_features_Numeric$avg_pageviews_per_visit)
hist(customer_features_Numeric$days_between_first_last_visit)
hist(customer_features_Numeric$max_time_since_last)
hist(customer_features_Numeric$avg_time_since_last)
hist(customer_features_Numeric$pageviews)
hist(customer_features_Numeric$different_day_visits)
hist(customer_features_Numeric$total_sessions)
hist(customer_features_Numeric$avg_session_num)
hist(customer_features_Numeric$total_visits)

# Plot histograms after applying Box-Cox transformations to assess distribution changes
hist((customer_features_Numeric$mobileUsage+1)**(-2))
hist((customer_features_Numeric$directVisits+1)**(-2))
hist((customer_features_Numeric$avg_pageviews_per_visit)**(-0.6))
hist((customer_features_Numeric$days_between_first_last_visit+1)**(-2))
hist((customer_features_Numeric$max_time_since_last+1)**(-0.4))
hist((customer_features_Numeric$avg_pageviews_per_visit)**(-0.5))
hist((customer_features_Numeric$pageviews)**(-0.5))
hist((customer_features_Numeric$different_day_visits)**(-2))
hist((customer_features_Numeric$total_sessions)**(-2))
hist((customer_features_Numeric$avg_session_num)**(-2))
hist((customer_features_Numeric$total_visits)**(-2))

# Apply Box-Cox transformation to various numeric features to stabilize variance
customer_features_Numeric$mobileUsage <- (customer_features_Numeric$mobileUsage+1)**(-2)
customer_features_Numeric$directVisits <- (customer_features_Numeric$directVisits+1)**(-2)
customer_features_Numeric$avg_pageviews_per_visit <- (customer_features_Numeric$avg_pageviews_per_visit)**(-0.6)
customer_features_Numeric$days_between_first_last_visit <- (customer_features_Numeric$days_between_first_last_visit+1)**(-2)
customer_features_Numeric$max_time_since_last <- (customer_features_Numeric$max_time_since_last+1)**(-0.4)
customer_features_Numeric$avg_pageviews_per_visit <- (customer_features_Numeric$avg_pageviews_per_visit)**(-0.5)
customer_features_Numeric$pageviews <- (customer_features_Numeric$pageviews)**(-0.5)
customer_features_Numeric$different_day_visits <- (customer_features_Numeric$different_day_visits)**(-2)
customer_features_Numeric$total_sessions <- (customer_features_Numeric$total_sessions)**(-2)
customer_features_Numeric$avg_session_num <- (customer_features_Numeric$avg_session_num)**(-2)
customer_features_Numeric$total_visits <- (customer_features_Numeric$total_visits)**(-2)

# Preprocess the numeric data: center and scale
preProcess_obj <- preProcess(customer_features_Numeric, 
                             method = c("center", "scale"))

# Transform the data using the preprocessing object
customer_features_Numeric <- predict(preProcess_obj, customer_features_Numeric)

# # Add the original columns custId and log_total_revenue back to the transformed data
# customer_features_Numeric <- customer_features_Numeric %>%
#   mutate(custId = customer_features$custId,
#          log_total_revenue = customer_features$log_total_revenue)

# Add log_total_revenue back to the transformed data and Remove directVisits 
customer_features_Numeric <- customer_features_Numeric %>%
  mutate(log_total_revenue = customer_features$log_total_revenue) %>% 
  dplyr::select(-directVisits)
  

# Transform character columns into factors
customer_features_Factor <- customer_features %>% 
  transmute_if(is.character, as.factor)

# Combine numeric and factor features into a final dataset
Final_customer_features <- customer_features_Numeric %>% 
  bind_cols(customer_features_Factor)

# Display the names of the final features
names(Final_customer_features)

# Function to create scatter plots
plot_scatter <- function(x, y, xlab, ylab, title) {
  plot(x, y, 
       main = title, 
       xlab = xlab, 
       ylab = ylab)
}

# Create scatter plots to visualize relationships between log_total_revenue and other features
plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$total_visits,
             "log_total_revenue", "total_visits", "log_total_revenue vs total_visits")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$avg_pageviews_per_visit,
             "log_total_revenue", "avg_pageviews_per_visit", "log_total_revenue vs avg_pageviews_per_visit")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$days_between_first_last_visit,
             "log_total_revenue", "days_between_first_last_visit", "log_total_revenue vs days_between_first_last_visit")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$different_day_visits,
             "log_total_revenue", "different_day_visits", "log_total_revenue vs different_day_visits")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$total_sessions,
             "log_total_revenue", "total_sessions", "log_total_revenue vs total_sessions")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$avg_session_num,
             "log_total_revenue", "avg_session_num", "log_total_revenue vs avg_session_num")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$avg_time_since_last,
             "log_total_revenue", "avg_time_since_last", "log_total_revenue vs avg_time_since_last")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$max_time_since_last,
             "log_total_revenue", "max_time_since_last", "log_total_revenue vs max_time_since_last")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$mobileUsage,
             "log_total_revenue", "mobileUsage", "log_total_revenue vs mobileUsage")
# 
# plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$directVisits,
#              "log_total_revenue", "directVisits", "log_total_revenue vs directVisits")

plot_scatter(Final_customer_features$log_total_revenue, Final_customer_features$pageviews,
             "log_total_revenue", "pageviews", "log_total_revenue vs pageviews")


colSums(is.na(Final_customer_features))


#################################################
# End Of Analysis of Customer Features
#################################################

#################################################
# Handling multicollinearity
#################################################

ols_fit_lm <- lm(log_total_revenue ~ .,
                 data = Final_customer_features)

par(mfrow=c(2,2))
plot(ols_fit_lm)
par(mfrow=c(1,1))
outlierTest(ols_fit_lm)
car::vif(ols_fit_lm)>10

# Remove high-VIF variables (starting with mobileUsage, total_sessions, and avg_session_num)
Final_customer_features_reduced <- Final_customer_features %>%
  dplyr::select(-c(mobileUsage, total_sessions, days_between_first_last_visit,
                   total_visits))

# Compute the correlation matrix of numerical variables
cor_matrix <- cor(Final_customer_features[, sapply(Final_customer_features, is.numeric)])

# Print the correlation matrix
heatmap(cor_matrix)

corrplot::corrplot(cor_matrix,method = "color")

# Remove high-VIF variables (starting with mobileUsage, total_sessions, and avg_session_num)
Final_customer_features <- Final_customer_features %>%
  dplyr::select(-c(mobileUsage, total_sessions, days_between_first_last_visit,
                   total_visits))

#################################################
# Lets Build Models
#################################################

# Perform 10-fold cross-validation using caret
cv_control <- trainControl(method = "cv", number = 10)

# OLS
ols_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "lm", 
                   trControl = cv_control)
ols_fit
min(ols_fit$results$RMSE)
summary(ols_fit)

#################################################
# Lasso
lasso_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "lasso", 
                   trControl = cv_control,
                   tuneLength = 20)
lasso_fit
plot(lasso_fit)

# Lasso
lasso_grid <- expand.grid(fraction = seq(0.85,1,length=100))
lasso_fit <- train(log_total_revenue ~ .,
                 data = Final_customer_features, 
                 method = "lasso", 
                 trControl = cv_control,
                 tuneGrid = lasso_grid)
lasso_fit
plot(lasso_fit)
min(lasso_fit$results$RMSE)

#################################################
# PLS
pls_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "pls", 
                   trControl = cv_control,
                 tuneLength = 20)
pls_fit
plot(pls_fit)

pls_grid <- expand.grid(ncomp = seq(25,35,length=100))
pls_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "pls", 
                   trControl = cv_control,
                   tuneGrid = pls_grid)
pls_fit
plot(pls_fit)
min(pls_fit$results$RMSE)

#################################################
# ElasticNet
glmnet_fit <- train(log_total_revenue ~ .,
                 data = Final_customer_features, 
                 method = "glmnet", 
                 trControl = cv_control,
                 tuneLength = 5)
glmnet_fit
plot(glmnet_fit)

# ElasticNet
glmnet_grid <- expand.grid( alpha = seq(0.8, 1, length = 10), 
  lambda = seq(0, 0.1, length = 10) 
)

glmnet_fit <- train(log_total_revenue ~ .,
                 data = Final_customer_features, 
                 method = "glmnet", 
                 trControl = cv_control,
                 tuneGrid = glmnet_grid)
glmnet_fit
plot(glmnet_fit)
min(glmnet_fit$results$RMSE)

#################################################
# PCR
PCR_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "pcr", 
                   trControl = cv_control,
                   tuneLength = 20)
PCR_fit
plot(PCR_fit)

# PCR
PCR_grid <- expand.grid(ncomp = seq(20,35,length=100))
PCR_fit <- train(log_total_revenue ~ .,
                   data = Final_customer_features, 
                   method = "pcr", 
                   trControl = cv_control,
                   tuneGrid = PCR_grid)
PCR_fit
plot(PCR_fit)
min(PCR_fit$results$RMSE)

#################################################

# MARS
MARS_fit <- train(log_total_revenue ~ ., 
                  data = Final_customer_features, 
                  method = "earth", 
                  trControl = cv_control,
                  tuneLength = 20)
print(MARS_fit)
plot(MARS_fit)

# MARS
MARS_grid <- expand.grid(degree = 1,  # Degree of interaction
                         nprune = seq(8, 20, by = 5))  # Number of terms

MARS_fit <- train(log_total_revenue ~ ., 
                  data = Final_customer_features, 
                  method = "earth", 
                  trControl = cv_control, 
                  tuneGrid = MARS_grid)
print(MARS_fit)
plot(MARS_fit)
min(MARS_fit$results$RMSE)

#################################################

# Extracting RMSE for each model
ols_cv_rmse <- min(ols_fit$results$RMSE)
pls_cv_rmse <- min(pls_fit$results$RMSE)
lasso_cv_rmse <- min(lasso_fit$results$RMSE)
glmnet_cv_rmse <- min(glmnet_fit$results$RMSE)
PCR_cv_rmse <- min(PCR_fit$results$RMSE)
MARS_cv_rmse <- min(MARS_fit$results$RMSE)

# Summary Data Frame
model_performance <- data.frame(
  Model = c("OLS", "PLS", "LASSO", "glmnet", "PCR","MARS"),
  Hyperparameter = c("None", "ncomp", "alpha=1, lambda", "alpha,Lambda", "ncomp", "degree = 1 , nprune"),
  CV_RMSE = c(ols_cv_rmse, pls_cv_rmse, lasso_cv_rmse, glmnet_cv_rmse, PCR_cv_rmse, MARS_cv_rmse)
)

# Display the sorted data frame
print(model_performance)

############
# Testing
############

Test <- read_csv("~/RStudio/IDA_Homework/IDA Assignemnt 6/2024-ise-dsa-5103-ida-hw-6/Test.csv/Test.csv", show_col_types = FALSE)
Test <- as_tibble(Test)

# Select numeric and factor columns as done for Train
Test_Numeric <- Test %>% select_if(is.numeric)
Test_Factor <- Test %>% select_if(is.character) %>% 
  transmute_if(is.character, as.factor)

# Apply same column selection as Train
Test_Numeric <- Test_Numeric %>%
  dplyr::select(-c(adwordsClickInfo.page, bounces, newVisits, sessionId))

colSums(is.na(Test_Numeric))

Test_Numeric <- kNN(Test_Numeric, k = 5, imp_var = FALSE)

Test_Factor <- Test_Factor %>% 
  dplyr::select(-c(adContent, adwordsClickInfo.slot, 
                   adwordsClickInfo.adNetworkType, adwordsClickInfo.adNetworkType,
                   adwordsClickInfo.gclId, keyword, campaign, metro, 
                   referralPath, city, region, networkDomain, topLevelDomain))

Test_Factor$channelGrouping <- fct_lump_n(Test_Factor$channelGrouping , n = 4)
Test_Factor$browser <- fct_lump_n(Test_Factor$browser , n = 2)
Test_Factor$operatingSystem <- fct_lump_n(Test_Factor$operatingSystem , n = 6)
Test_Factor$source <- fct_lump_n(Test_Factor$source , n = 4)
Test_Factor$medium <- fct_lump_n(Test_Factor$medium , n = 3)
Test_Factor$country <- fct_lump_n(Test_Factor$country , n = 1)
Test_Factor$subContinent <- fct_lump_n(Test_Factor$subContinent , n = 1)
Test_Factor$continent <- fct_lump_n(Test_Factor$continent , n = 3)

factor_levels <- map(Test_Factor, ~fct_count(.x, sort = TRUE))
factor_levels

Test_Factor <- hotdeck(Test_Factor,imp_var=FALSE)

Test_Final <- Test %>%
  dplyr::select(date) %>%
  bind_cols(Test_Numeric, Test_Factor) %>%
  dplyr::select(-subContinent)
  
Test_Final <- Test_Final %>%
  mutate(
    day_of_week = wday(date, label = TRUE),  # Extract day of the week
    cust_lag_visit = lag(visitNumber, order_by = visitStartTime),  # Previous visit number
    consecutive_visits = ifelse(visitNumber == cust_lag_visit + 1, 1, 0)  # Check consecutive
  )

# You may need to group by custId and create aggregated features
Test_customer_features <- Test_Final %>%
  group_by(custId) %>%
  summarise(
    total_visits = n(),
    avg_pageviews_per_visit = mean(pageviews),
    days_between_first_last_visit = as.numeric(max(date) - min(date)),
    different_day_visits = n_distinct(date),  # Number of unique visit days
    total_sessions = max(visitNumber),  # Total sessions per customer
    avg_session_num = mean(visitNumber),
    avg_time_since_last = mean(timeSinceLastVisit),
    max_time_since_last = max(timeSinceLastVisit),
    mobileUsage = mean(isMobile),
    directVisits = mean(isTrueDirect),
    pageviews = sum(pageviews),
    mostCommonBrowser = names(sort(table(browser), decreasing = TRUE)[1]),
    mostCommonOS = names(sort(table(operatingSystem), decreasing = TRUE)[1]),
    mostCommonDevice = names(sort(table(deviceCategory), decreasing = TRUE)[1]),
    primaryChannel = names(sort(table(channelGrouping), decreasing = TRUE)[1]),
    mostFrequentContinent = names(sort(table(continent), decreasing = TRUE)[1]),
    mostFrequentCountry = names(sort(table(country), decreasing = TRUE)[1]),
    primarySource = names(sort(table(source), decreasing = TRUE)[1]),
    primaryMedium = names(sort(table(medium), decreasing = TRUE)[1]),
    mostCommonDay = names(sort(table(day_of_week), decreasing = TRUE)[1])  # Most frequent day of visit
  )

# Apply transformations to the aggregated features (based on training)
Test_customer_features <- Test_customer_features %>%
  mutate(
    # Example transformations
    mobileUsage = (mobileUsage + 1)^(-2),  
    directVisits = (directVisits + 1)^(-2),  
    avg_pageviews_per_visit = (avg_pageviews_per_visit)^(-0.6),  
    days_between_first_last_visit = (days_between_first_last_visit + 1)^(-2),  
    max_time_since_last = (max_time_since_last + 1)^(-0.4),  
    avg_pageviews_per_visit = (avg_pageviews_per_visit)^(-0.5),
    pageviews = (pageviews)^(-0.5),
    different_day_visits = (different_day_visits)^(-2),
    total_sessions = (total_sessions)^(-2),
    avg_session_num = (avg_session_num)^(-2),
    total_visits = (total_visits)^(-0.2)
  )

Test_customer_features_custID <- Test_customer_features %>% 
  dplyr::select(custId)

Test_customer_features <- Test_customer_features %>% 
  dplyr::select(-custId)

Test_customer_features_Numeric <- Test_customer_features %>% 
  select_if(is.numeric)

Test_customer_features_Numeric <- predict(preProcess_obj, Test_customer_features_Numeric)

Test_customer_features_Numeric <- Test_customer_features_Numeric %>% 
  dplyr::select(-directVisits)

# Transform character columns into factors
Test_customer_features_Factor <- Test_customer_features %>% 
  transmute_if(is.character, as.factor)

# Combine numeric and factor features into a final dataset
Test_Final_customer_features <- Test_customer_features_Numeric %>% 
  bind_cols(Test_customer_features_Factor)

# Remove high-VIF variables
Test_Final_customer_features <- Test_Final_customer_features %>%
  dplyr::select(-c(mobileUsage, total_sessions, days_between_first_last_visit,
                   total_visits))

lasso_predictions <- predict(lasso_fit, newdata = Test_Final_customer_features)


MARS_predictions <- predict(MARS_fit, newdata = Test_Final_customer_features)

predictions_tibble <- predictions_tibble %>%
  mutate(predRevenue = as.vector(predRevenue))
glimpse(predictions_tibble)

# Optionally, save the tibble to a CSV file
write_csv(predictions_tibble, "MARS_predictions_Oct_24_3.csv")


############

