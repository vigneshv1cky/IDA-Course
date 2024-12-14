library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)

data(concrete,package="AppliedPredictiveModeling")

?concrete

# Compressive Strength of Concrete from Yeh (1998)
# 
# Data that can be used to model compressive strength of concrete formulations
# as a functions of their ingredients and age.
#
# From: http://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength
#
# "Concrete is the most important material in civil engineering. The 
# concrete compressive strength is a highly nonlinear function of age and 
# ingredients. These ingredients include cement, blast furnace slag, fly ash, 
# water, superplasticizer, coarse aggregate, and fine aggregate."

# Cement (component 1) -- quantitative -- kg in a m3 mixture -- Input Variable
# Blast Furnace Slag (component 2) -- quantitative -- kg in a m3 mixture -- Input Variable
# Fly Ash (component 3) -- quantitative -- kg in a m3 mixture -- Input Variable
# Water (component 4) -- quantitative -- kg in a m3 mixture -- Input Variable
# Superplasticizer (component 5) -- quantitative -- kg in a m3 mixture -- Input Variable
# Coarse Aggregate (component 6) -- quantitative -- kg in a m3 mixture -- Input Variable
# Fine Aggregate (component 7) -- quantitative -- kg in a m3 mixture -- Input Variable
# Age -- quantitative -- Day (1~365) -- Input Variable
# Concrete compressive strength -- quantitative -- MPa -- Output Variable 


glimpse(concrete)



#first -- a basic "lm" model from the stats package

fit_lm <- lm(data = concrete, CompressiveStrength ~ .)
fit_lm 
summary(fit_lm)

plot(fit$finalModel)
str(fit_lm)



#train command -- can access several models
#              -- performs resampling
#              -- performs hyperparameter tuning (when available)

# the "lm" model does not have any hyperparameters, 
# but we can use caret in a simple way

fit <- train(CompressiveStrength ~ .,
             data=concrete,
             method="lm")

fit

str(fit)

#note the "final Model" is an element of the "train" created object
#the exact data structure of the final model is based on the underlying 
#package that caret is using for the requested model 
#e.g., for method="lm", caret is just calling the "lm" method from the stats package


fit$finalModel
summary(fit$finalModel)
plot(fit$finalModel)
str(fit$finalModel)

#even though there are no hyperparameters to tune, we can still perform resampling

#you can adjust the resampling parameters using "trainControl"

?trainControl

fitControl <- trainControl(method="cv", number=10)
fitControl <- trainControl(method="cv", number=5)
#fitControl <- trainControl(method="repeatedcv", number=10, repeats=10)
fitControl <- trainControl(method="boot", number=100)

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="lm",
             trControl=fitControl)
fit

fit$resample


#if you wish to also train hyperparameters...
# (1) make sure you know what hyperparameters are available to tune
# (2) either use the default parameter search, or
# (3) more likely setup your own search parameters


# the lasso model has hyperparameters
# look for the correct "method" and parameter names on:
# https://topepo.github.io/caret/available-models.html



#one possibility is to use method name = "lasso" and parameter name is "fraction"

#using default training values

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="lasso",
             trControl=fitControl)

fit
plot(fit)

#or change the metric you want to plot

plot(fit, metric = "Rsquared")

#and inspect the finalModel...
plot(fit$finalModel)

#expanding the  default parameter search some

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="lasso",
             trControl=fitControl,
             tuneLength=100)

plot(fit)


#can easily try completely different models,
#e.g., let's try a decision tree

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="rpart",
             trControl=fitControl,
             tuneLength=10)

plot(fit)

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="gbm",
             trControl=fitControl,
             tuneLength = 3)

fit

plot(fit)

#back to lasso
#specifying exactly the hyperparameter values to use with "expand.grid"

lassoGrid <- expand.grid(fraction=seq(0.5,0.8,length=100))

fit <- train(CompressiveStrength~.,
             data=concrete,
             method="lasso",
             trControl=fitControl,
             tuneGrid=lassoGrid)
str(fit)

str(fit$finalModel)


plot(fit)


#now for a too complicated model

#specifying exactly the hyperparameter values to use with "expand.grid"

lassoGrid <- expand.grid(fraction=seq(0.35,0.95,length=30))

fit <- train(CompressiveStrength~.*.,   #<-- notice the change!
              data=concrete,
             method="lasso",
             trControl=fitControl,
             tuneGrid=lassoGrid)
fit

plot(fit)
plot(fit$finalModel)




#some models have multiple parameters to train
#e.g., elasticnet: method = "enet", parameters: fraction, lambda

enetGrid <- expand.grid(lambda=seq(0,0.5,length=5),
                        fraction=seq(0.1,1,length=5))


fit <- train(CompressiveStrength~.*.,
             data=concrete,
             method="enet",
             trControl=fitControl,
             tuneGrid=enetGrid)
fit
plot(fit)

#if there are two hyperparameters to be trained, you can look at the
# "level" plot to see what are good combinations of the values

plot(fit, plotType="level")

enetGrid <- expand.grid(lambda=seq(0.2,.02,length=5),
                        fraction=seq(0.4,.9,length=10))

