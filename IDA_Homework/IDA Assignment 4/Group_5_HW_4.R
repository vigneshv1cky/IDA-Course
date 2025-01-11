library(tidyverse)

# 1 Data Quality Report

housingData <- read_csv("housingData.csv")
is_tibble(housingData)

# (a) After loading the housing data into a data frame (or tibble) named housingData, run the code
# listed below to create three new variables.
# housingData <- housingData %>%
#   dplyr::mutate(age = YrSold - YearBuilt,
#                 ageSinceRemodel = YrSold - YearRemodAdd,
#                 ageofGarage = YrSold - GarageYrBlt))
housingData <- housingData %>%
  mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)


# (b) (2 points) Use the dplyr package to create a tibble named housingNumeric which contains all of
# the numeric variables from the original data. Please use the dplyr::select command along with
# the is.numeric function to complete this task.
housingNumeric <- housingData %>% select_if(is.numeric)
is_tibble(housingNumeric)

# (c) (2 points) Use the dplyr package to create a tibble named housingFactor which contains all of
# the numeric variables from the original data. You can use dplyr::select command here or, if
# you like, consider the transmute command to simultaneously keep only the character variables and
# change all character variables to factors.
housingFactor <- housingData %>% transmute_if(is.character, as.factor)

# (d) Try the glimpse command to take a look at your new tibbles.
glimpse(housingNumeric)
glimpse(housingFactor)

# (e) (4 points) Unfortunately, R does not have a method for extracting only Q1 or Q3. So, we will
# create our own user-defined functions to do this for us. Use the following code to create two new
# functions, Q1 and Q3, respectively.
# Q1<-function(x,na.rm=TRUE) {
#   quantile(x,na.rm=na.rm)[2]
# }
# Q3<-function(x,na.rm=TRUE) {
#   quantile(x,na.rm=na.rm)[4]
# }
# Briefly explain what these two new functions are doing.

Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# In the quantile function in R, when you compute quartiles of a data vector
# without specifying any specific probabilities,
# it returns a named vector of percentiles that typically include the following ->
# Index [1] - Minimum: It retrieves the minimum value of the data set.
# Index [2] - First Quartile: It retrieves the first quartile, which is what your Q1 function extracts.
# Index [3] - Median: It retrieves the median of the data set.
# Index [4] - Third Quartile: It retrieves the third quartile.
# Index [5] - Maximum: It retrieves the maximum value of the data set.
# Q1 access the index 2, the first quartile and Q3 access the index 4, the third quartile.


# (f) Next, we are going to create a new function that will apply several summary statistics to our data
# all at once. Create the new function myNumericSummary with the following code.
# myNumericSummary <- function(x){
#   c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
#     min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
#     max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
# }
# This code accepts a numerical vector x as an input parameter and then returns a vector where the
# first element is the length of the input vector (i.e., the number of observations), the second element
# is the number of unique values, the third is the number of missing values, the forth is the mean
# value of non-missing numerics, etc. Notice the use of our new functions Q1 and Q3.

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

# (g) (8 points) Utitlize the dplyr::summarize command together with the new myNumericSummary
# function to apply the new function to every variable in the housingNumeric data set. You may
# need to look up some examples of how to use summmarize and the across() syntax from dplyr to
# do this eﬃciently. Save the results of this operation in a new tibble named numericSummary.

numericSummary <- housingNumeric %>%  
  summarize(across(everything(), myNumericSummary))
glimpse(numericSummary)

# (h) Next, column bind some labels to our summary statistics with the following code.
# numericSummary <-cbind(
#   stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
#   numericSummary)
# If you glimpse the results, it should look something like Figure 3.

numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
glimpse(numericSummary)

# (i) While this is good data here, you need to perform a little trick on it so we can use the kable
# function and produce the table we want, i.e., need to “pivot” the data a couple of times. You also
# need to add a couple more computed values: percent missing and percent unique fields. Use the
# following code to accomplish this.
# numericSummaryFinal <- numericSummary %>%
#   pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>%
#   pivot_wider(names_from = stat, values_from = value) %>%
#   mutate(missing_pct = 100*missing/n,
#          unique_pct = 100*unique/n) %>%
#   select(variable, n, missing, missing_pct, unique, unique_pct, everything())
# and finally, produce the first part of the Data Quality report,
# library(knitr)
# options(digits=3)
# options(scipen=99)
# numericSummaryFinal %>% kable()

numericSummaryFinal <- numericSummary %>%
  pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
library(knitr)
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

# (j) (30 points) Create the second part of the Data Quality report associated with the non-numeric
# data. See Figure 2 for a report excerpt.
# Note: R does not have functions for identifying the first, second, or least commmon modes. Use
# the code below to accomplish this.

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

FactorSummary <- housingFactor %>%  
  summarize(across(everything(), myFactorSummary))
glimpse(FactorSummary)
FactorSummary <-cbind(
  stat=c("n","unique","missing","most_common","most_common_count","2nd_most_common",
         "2nd_most_common_count","least_common","least_common_count"),
  FactorSummary)
glimpse(FactorSummary)
FactorSummaryFinal <- FactorSummary %>%
  pivot_longer("MSZoning":"SaleType", names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value)
glimpse(FactorSummaryFinal)
FactorSummaryFinal$n <- as.numeric(FactorSummaryFinal$n)
FactorSummaryFinal$unique <- as.numeric(FactorSummaryFinal$unique)
FactorSummaryFinal$missing <- as.numeric(FactorSummaryFinal$missing)
FactorSummaryFinal <- FactorSummaryFinal %>% 
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
library(knitr)
options(digits=3)
options(scipen=99)
FactorSummaryFinal %>% kable()

# ******************************************************************************
# ******************************************************************************
# ******************************************************************************
  
# (a) (8 points) Via visual inspection, identify two numeric variables that are highly skewed
# (e.g., not symmetric and far from normally distributed).Use a transformation method
# (e.g., ladder of powers or boxcox transformation) to transform these variables to be more
# normally distributed. Show visual depictions of distributions before/after transformations
  

glimpse(housingNumeric)
typeof(housingNumeric)
library(e1071)
library(caret)
skewValues <- apply(housingNumeric, 2, skewness)
skewValues
# Calculate skewness using mutate and across
skewValues_tibble <- housingNumeric %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Skewness")
# Print the tibble
skewValues_tibble
# Lot Area Tranformation
BoxCoxTrans(housingNumeric$LotArea)
housingNumeric$LotAreaLog <- log(housingNumeric$LotArea)
BoxCoxTrans(housingNumeric$LotAreaLog)
# Plotting before transformation
ggplot(housingNumeric, aes(x = LotArea)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  ggtitle("Original Distribution of Lot Area") +
  xlab("Lot Area (sq feet)") +
  ylab("Frequency")
# Plotting after transformation
ggplot(housingNumeric, aes(x = LotAreaLog)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  ggtitle("Original Distribution of Lot Area Log") +
  xlab("Lot Area Log (sq feet)") +
  ylab("Frequency")
# OpenPorchSF Transformation
BoxCoxTrans(housingNumeric$OpenPorchSF+1)
housingNumeric$TransformedOpenPorchSF <- log(housingNumeric$OpenPorchSF)
BoxCoxTrans(housingNumeric$TransformedOpenPorchSF+1)
# Original Distribution of OpenPorchSF
ggplot(housingNumeric, aes(x = OpenPorchSF)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  ggtitle("Original Distribution of OpenPorchSF") +
  xlab("OpenPorchSF") +
  ylab("Frequency")
# Transformed distribution of OpenPorchSF
ggplot(housingNumeric, aes(x = TransformedOpenPorchSF)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  ggtitle("Transformed distribution of OpenPorchSF") +
  xlab("TransformedOpenPorchSF") +
  ylab("Frequency")

 
# (b) (20 points) The variable LotFrontage has several missing values. Impute the missing values using:
#   i. mean value imputation
# ii. regression with error
# iii. predictive mean matching (Use the mice package and optionally see https://datascienceplus.
#                                com/imputing-missing-data-with-r-mice-package/ for help )
# iv. For all of the above show visual depictions of how the data was transformed (e.g., histogram
#    or density plots )

library(gridExtra)
# Mean imputation
mean_value <- mean(housingData$LotFrontage, na.rm = TRUE)
housingData$LotFrontage_mean <- ifelse(is.na(housingData$LotFrontage), mean_value, housingData$LotFrontage)
# Create histogram of LotFrontage before imputation
p1 <- ggplot(housingData, aes(x = LotFrontage)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogram of LotFrontage Before Imputation") +
  xlab("LotFrontage") +
  ylab("Frequency") 
# Create histogram of LotFrontage after imputation
p2 <- ggplot(housingData, aes(x = LotFrontage_mean)) + 
  geom_histogram(bins = 30, fill = "red", color = "black") +
  ggtitle("Histogram of LotFrontage After Mean Imputation") +
  xlab("LotFrontage") +
  ylab("Frequency")
# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)


# Check if 'LotArea' has missing values and impute if necessary
housingData$LotArea[is.na(housingData$LotArea)] <- mean(housingData$LotArea, na.rm = TRUE)
# Fit a linear model to predict 'LotFrontage' using 'LotArea'
model <- lm(LotFrontage ~ LotArea, data = housingData, na.action = na.exclude)
# Make predictions for the full dataset
predicted_values <- predict(model, newdata = housingData)
# Calculate the residuals and their standard deviation
residuals <- resid(model)
std_error <- sd(residuals, na.rm = TRUE)
# Impute missing values in 'LotFrontage'
missing_indices <- is.na(housingData$LotFrontage)
housingData$LotFrontage_reg <- housingData$LotFrontage
# Add normally distributed noise based on the residual standard deviation
housingData$LotFrontage_reg[missing_indices] <- predicted_values[missing_indices] + 
  rnorm(sum(missing_indices), mean = 0, sd = std_error)
# Visualization using a histogram to see the distribution after imputation
p3 <- ggplot(housingData, aes(x = LotFrontage_reg)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  ggtitle("Distribution of LotFrontage after Regression Imputation") +
  xlab("LotFrontage") +
  ylab("Frequency")
# Arrange the plots side by side
grid.arrange(p1, p3, ncol = 2)

library(mice)
mice_mod <- mice(housingData[, c("LotFrontage", "LotArea")],
                 method = 'pmm', m = 5, seed = 123)
summary(mice_mod)
# Perform the imputation
imputed_data <- complete(mice_mod, 1)  # We use the first completed dataset for simplicity
# Imputed Data Histogram
p4 <- ggplot(imputed_data, aes(x = LotFrontage)) + 
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  ggtitle("LotFrontage after Predictive Mean Matching") +
  xlab("LotFrontage") + 
  ylab("Frequency") 
# Arrange the plots side by side
grid.arrange(p1, p4, ncol = 2)




# (c) (10 points) Use the forcats package to do just that: Collapse the factor levels in the Exterior1st down to
# only five levels – the first four levels should be the most frequent levels and all other levels should
# be collapsed into a single “Other” level.

as.factor(housingData$Exterior1st)
fct_count(housingData$Exterior1st, sort = T)
fct_unique(housingData$Exterior1st)
# Collapse factor levels
housingData$Exterior1st <- fct_lump_n(housingData$Exterior1st, n = 4)
# Check the changes
table(housingData$Exterior1st)



# (d) (16 points) More fun with factors
# i. Use tidyverse packages to compute the average SalePrice for each Neighborhood factor level.
# ii. Create a parallel boxplot chart of this data, i.e., a boxplot associated with the sale prices for
# homes in each of the 18 neighborhoods.
# iii. You should notice that there is a lot of variation in price by neighborhood. Using forcats
# re-order the factor levels of the Neighborhood variable in descending order of the median price
# per neighborhood (i.e., the neighborhood with the highest median price is NoRidge, the next
#                   highest median is NridgHt, etc., so NoRidge should be the first level and NridgHt should be
#                   the second factor level, etc.)
# iv. If you have done re-ordering correctly, you should be able to produce a parallel boxplot of
# neighborhoods and sales prices in descending order (see Figure 5). Note: R orders values in
# graphs according to the ordering of the factors.


# Average SalePrice for each Neighborhood factor level
average_prices <- housingData %>%
  group_by(Neighborhood) %>%
  summarise(AverageSalePrice = mean(SalePrice, na.rm = TRUE))
print(average_prices)
# Parallel Boxplots before Sorting
housingData %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better visibility
housingData <- housingData %>%
  mutate(Neighborhood = fct_reorder(Neighborhood, SalePrice, median, .desc = TRUE))
# Verify the new order
housingData %>%
  group_by(Neighborhood) %>%
  summarise(MedianSalePrice = median(SalePrice, na.rm = TRUE))
# Parallel Boxplots after Sorting
housingData %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better visibility
