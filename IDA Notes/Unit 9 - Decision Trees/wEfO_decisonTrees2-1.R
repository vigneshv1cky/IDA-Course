#ISE 5103 R script for introducing tree ensembles 
#Author: Charles Nicholson
#November 2015

library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
library(rattle)        # for graphics
library(adabag)        # for boosting
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests
library(caret)         # for training and modeling
library(MASS)          # to obtain the sample data set "Glass"

#we have used this Glass data before
data(Glass)


#Point #1 --> Tree are unstable

#run the following bit of code -- it will produce 7 different trees
#based on slightly different samples of Glass data
#make sure to use the "back arrows" in the Plots window to view 
#all of the trees ---> they are VERY different from each other!

seeds <- c(3,67,71,67,78,2,1)

for (i in 1:length(seeds)){
  
  set.seed(seeds[i])                        #try different seeds
  s <- sample(dim(Glass)[1], 150)
  train <- Glass[s, ]
  test <- Glass[-s, ]
  
  #use CART tree
  fit1 <- rpart(data=train, Type ~.)
  fitTreeParty<-as.party(fit1)
  plot(fitTreeParty)
  print (fit1$variable.importance)
}

#not only are the tree structures different, but the above code also 
#printed out the "Variable importance" measures -->
#the results were different each time.



#Now,fix the seed so we can all take about the same example
set.seed(1)  
s <- sample(dim(Glass)[1], 150)
train <- Glass[s, ]
test <- Glass[-s, ]

#use CART to build a single tree
fit1 <- rpart(data=train, Type ~.)
fitTreeParty<-as.party(fit1)
plot(fitTreeParty)


#how well did our tree do?  (on training data?)
pred = predict(fit1, type="class")
confusionMatrix(pred, train$Type)

#Train Accuracy:   0.7553
#Kappa:            0.6438

#how well did our tree do?  (on test data?)
pred = predict(fit1, newdata=test, type="class")
confusionMatrix(pred, test$Type)

#Test Accuracy:   0.6094
#Kappa:           0.4087

#---> did not generalize so well!


summary(fit1)

#examine variable importance
fit1$variable.importance
barplot(fit1$variable.importance, main = "CART Variable Importance")


varImp(fit1)   #from caret package -- a slighter different calculation


#examine relative error and CV error 
#(note to get actual error scale by root misclassification rate)
#different error at each cost parameter level

# note the cost values shown are related to the "alpha" value from the lecture

fit1$cptable  #this extracts the complexity paramater cost 
              #larger values of CP are associated with less complex trees


#next look at complexity for pruning
plotcp(fit1)


#we can prune the tree accordingly

#prune tree
pfit<-prune(fit1,cp=0.12) #setting the cp to 0.12 will produce tree with 4 leaves
fancyRpartPlot(pfit)

pred = predict(pfit, newdata=test, type="class")
confusionMatrix(pred, test$Type)


pfit<-prune(fit1,cp=0.063)  #setting the cp to 0.063 will produce tree with 6 leaves
fancyRpartPlot(pfit)

predTree = predict(pfit, newdata=test, type="class")
confusionMatrix(predTree , test$Type)


#I found the accuracy on the partitioned test data to 
#be best with the smaller of the two cp values


#now for BAGGING!

#we will use the bagging from "ipred" package

?bagging
#notice some parameters: 
# -- nbagg: number of trees
# -- coob: compute out-of-bag estimates for error or not

fit2 <- bagging(Type ~ ., data = train, coob = T)  #coob=T --> compute oob error estimate
fit2

predBag = predict(fit2, newdata=test)
predBag = factor(pred, levels = levels(test$Type))
confusionMatrix(predBag, test$Type)

#Test Accuracy:   0.5781
#Kappa:           0.3528

#(not so good this time...)


#Random Forest 

?randomForest
#notice some parameters: 
# -- ntree: number of trees
# -- mtry: the value "m" from lecture


fit3 <- randomForest(Type ~ ., data = train, importance = T, ntrees=1500, mtry=3)
fit3


predRF = predict(fit3, newdata=test)
predRF = factor(predRF, levels = levels(test$Type))
confusionMatrix(predRF, test$Type)

#Test Accuracy:   0.8125
#Kappa:           0.7252

#really good results!!!


#The plot method traces the error rates (out-of-bag, and by each response
#category) as the number of trees increases. 

plot(fit3)



#The importance option in the randomForest function requests the assessment of 
#predictor importances. There are two global measures: 
#one is the mean descrease in accuracy over all classes,
#the other is the mean decrease in Gini index. 


par(mfrow = c(2, 1))
barplot(fit3$importance[, 7], main = "Importance (Dec.Accuracy)")
barplot(fit3$importance[, 8], main = "Importance (Gini Index)")
par(mfrow = c(1, 1))

varImpPlot(fit3)




#now for boosting
?boosting             
#notice some parameters: 
# -- coeflearn: formula for computing alpha; in lecture I talked about the 'Fruend' method
# -- mfinal the value "M" from lecture (number of iterations)
# -- boos: can use bootstrap sampling with boosted trees or not 

# please note boosting is more computationally intense than the other methods
fit4<-boosting(Type ~ ., data = train, boos = F, mfinal = 20)

predBoost = predict(fit4, test)$class
predBoost = factor(predBoost, levels = levels(test$Type))
confusionMatrix(predBoost, test$Type)

#Accuracy : 0.7812
#Kappa : 0.6934  


df<-data.frame(Truth = test$Type, 
               Tree = predTree,
               Bagging = predBag, 
               Forest = predRF,
               Boosted = predBoost)

#for each observation, you can look at the true Glass type,
#and the classifications from each competing technique
df


#and the total correct classifications by each
c(TreeCorrect=sum(df$Truth==df$Tree),
  BaggingCorrect=sum(df$Truth==df$Bagging),
  ForestCorrect=sum(df$Truth==df$Forest),
  BoostedCorrect=sum(df$Truth==df$Boosted))


# now, we can also do any type of cross-validation 
# that you want to do
# I am using the "errorest" function from the "ipred" package
# it is a little bit of an annoying technique, but I'll show 
# it here anyway 
# ---- another good option is the caret package (at bottomr)

mypredict.rpart <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")
}


mypredict.boosting <- function(object, newdata) {
  as.factor(predict(object, newdata = newdata)$class)
}


ctrl<-control.errorest(k = 5)


#note the full cross-validation for boosting takes a long time ~ 5 minutes or so
# you can remove the boosting CV if you want!

errors<-c(Tree = errorest(Type ~ ., data = Glass, model = rpart, estimator="cv",est.para=ctrl, predict = mypredict.rpart)$error,
  Bagging = errorest(Type ~ ., data = Glass, model = bagging, estimator="cv",est.para=ctrl)$error,
  Forest = errorest(Type ~ ., data = Glass, model = randomForest, estimator="cv",est.para=ctrl)$error,
  Boosted = errorest(Type ~ ., data = Glass, model = boosting, estimator="cv",predict = mypredict.boosting)$error
)

#and now for an apples to apples comparison
errors

#in my sample, random forest won -- the lowest error


# Another perfectly good option is to use the "caret" package
# for all your modeling, cross-validation needs, 
# and hyper-parameter tuning needs

# e.g., for random forest

rfGrid <-  expand.grid(mtry = 2:9)  #let's tune across mtry = 2,3,...,9

rf_model<-train(Type~., data=Glass,
                method="rf",             #random forest
                trControl=trainControl(method="cv",number=5),  #cross-validation 
                tuneGrid=rfGrid,   #hyper-parameter tuning
                allowParallel=TRUE)

#prints out the CV accuracy and kappa for each mtry value
print(rf_model)

#the best model (based on tuning grid)
print(rf_model$finalModel)


