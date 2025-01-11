library(e1071)   #for svm
library(mlbench) #for some data
library(ggplot2) #for graphics


#toy problem ------------
#recreate the example from the book -- or sort of..
x<- (5*runif(80)+1) * sample(c(-1,1),80,replace=TRUE)
y<- (5*runif(80)+1) * sign(x)

x<-c(x,1.0,-0.5,-1.5)  #I am creating these hoping they will be selected as the SV's
y<-c(y,1.0,-1.50,-.3)

response<-as.numeric(x>0)  #binary response variable

#take a look at the classes
qplot(x,y,color=factor(response))+scale_colour_manual(values = c("red", "blue"))

vals<-rep(c(0,1),c(80,3))  #just for graphics later on...

df<-data.frame(x,y,response) #creating a dataframe of the values and response

#using "svm" from e1071
#note: I am setting C to a really high value so that I won't have any misclassifications in this simple problem
svm_toy <- svm(factor(response)~x+y, data=df, 
               method="C-classification", kernel="linear", cost=1000)

#summary of results
svm_toy

#Call:
#  svm(formula = factor(response) ~ x + y, data = df, method = "C-classification", 
#      kernel = "linear", cost = 1000)

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1000 

#Number of Support Vectors:  3

#and the three support vectors --- in scaled form
svm_toy$SV

#look at them in the original form...
SV.orig = t(apply(svm_toy$SV, 1, function(r)r*svm_toy$x.scale$`scaled:scale` + svm_toy$x.scale$`scaled:center`))
  
SV.orig

#--exactly what I hoped for

#x  y.1
#81  1.0  1.0
#82 -0.5 -1.5
#83 -1.5 -0.3

#plot points again, but make the SV's larger so we can see them
df=data.frame(x,y,vals,response)
ggplot(data=df,aes(x,y,color=factor(response)))+geom_point(size = 2*vals+1)+
  scale_colour_manual(values = c("red", "blue"))

#create new points to predict on
dfNew<-data.frame(x=c(-1.8,3),y=c(1,-.5))

#plot everything
ggplot(data=df,aes(x,y,color=factor(response)))+
  geom_point(size = 2*vals+1)+
  scale_colour_manual(values = c("red", "blue"))+
  geom_point(data = dfNew, color = "black", size=3.5)


#training set predictions
predict(svm_toy,dfNew)



#next example using the Sonar dataset

data(Sonar) #load Sonar dataset
?Sonar

#The task is to discriminate between sonar signals bounced 
#off a metal cylinder and those bounced off a roughly cylindrical rock.
#208 obs; 60 predictors, two-class response: "M" and "R"

str(Sonar)

#set seed to ensure reproducible results
set.seed(42)

#basic TEST and TRAIN splitting (just to demo this quickly)

#split into training and test sets
Sonar[,"train"] <- ifelse(runif(nrow(Sonar))<0.8,1,0)

#separate training and test sets
trainset <- Sonar[Sonar$train==1,]
testset <- Sonar[Sonar$train==0,]

#get column index of train flag
trainColNum <- grep("train",names(trainset))

#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#get column index of predicted variable in dataset
typeColNum <- grep("Class",names(Sonar))

#build model - linear kernel and C-classification with default cost (C=1)
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="linear")

#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)

# 0.969697  --- great!

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)
# 0.6046512  --- not as good on the test data...

#now with RBF
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="radial")

#print params
svm_model$cost
svm_model$gamma

#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)
# 0.9878788  -- woo-hoo!

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)
# 0.7674419  --- better, but we might be able to improve with more tuning

#....let's use caret and do this right: cross-validation and more tuning

library(caret)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(3233)

svm_Linear <- train(Class~., data=trainset, method = "svmLinear2",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),  #scaling and centering data
                    tuneLength = 1)

#no tuning just yet, but you can still see we are doing many folds
svm_Linear$resample

#the average accuracy across the folds is about 76% or so
svm_Linear

#we can access the final model 
svm_Linear$finalModel


#now let's do a little tuning on the linear model
svm_Linear <- train(Class~., data=Sonar, method = "svmLinear2",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#results here, about the same
svm_Linear

svm_Linear$finalModel



#let's try kernlab and the RBF with caret
#--- for some reason you cannot train using RBF with e1071 in caret directly


library(kernlab)

data(Sonar)  #reload the data

svm_RBF <- train(Class~., data=Sonar, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#some of the resampled accuracies are quite good
svm_RBF$resample

svm_RBF  #we can look at the average accuracies and kappas by our C


#note the training error on the final model is 0
#the CV error is about 12%
svm_RBF$finalModel


#let's improve our tune...
RBFgrid <- expand.grid(C=seq(2,5,length=5), sigma=seq(0.01,0.03,length=5))

svm_RBF <- train(Class~., data=Sonar, method = "svmRadial",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = RBFgrid)


svm_RBF$finalModel

## get fitted values
fitted(svm_RBF$finalModel)

table(fitted(svm_RBF$finalModel),Sonar$Class)

#the best average accuracy is around 0.88 or so, not too bad!
svm_RBF


library(randomForest)

rf_model <- randomForest(Class~ ., data=trainset, importance=TRUE, ntree=1000)


#Get summary info
rf_model

pred_train_rf <-predict(rf_model,trainset)
mean(pred_train_rf==trainset$Class)

#test set predictions
pred_test_rf <-predict(rf_model,testset)
mean(pred_test_rf==testset$Class)

#SVM beats RF here!




#stole this from the internet 
#--- caret does not allow you to use the e1071 package for RBF
#--- so you have to write your own interface

svmRadial2ModelInfo <- list(
  label   = "Support Vector Machines with Radial Kernel based on e1071",
  library = "e1071",
  type    = c("Regression", "Classification"),
  parameters = data.frame(parameter = c("cost", "gamma"),
                          class = c("numeric", "numeric"),
                          label = c("Cost", "Gamma")),
  grid    = function(x, y, len = NULL, search = NULL) {
    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    return( expand.grid(gamma = mean(as.vector(sigmas[-2])),
                        cost  = 2 ^((1:len) - 3)) )
  },
  loop    = NULL,
  fit     = function(x, y, wts, param, lev, last, classProbs, ...) {
    if(any(names(list(...)) == "probability") | is.numeric(y))
    {
      out <- svm(x = as.matrix(x), y = y,
                 kernel = "radial",
                 cost  = param$cost,
                 gamma = param$gamma,
                 ...)
    } else {
      out <- svm(x = as.matrix(x), y = y,
                 kernel = "radial",
                 cost  = param$cost,
                 gamma = param$gamma,
                 probability = classProbs,
                 ...)
    }
    out
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict(modelFit, newdata)
  },
  prob    = function(modelFit, newdata, submodels = NULL) {
    out <- predict(modelFit, newdata, probability = TRUE)
    attr(out, "probabilities")
  },
  varImp = NULL,
  predictors = function(x, ...){
    out <- if(!is.null(x$terms)) predictors.terms(x$terms) else x$xNames
    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if(is.null(out)) out <-NA
    out
  },
  levels = function(x) x$levels,
  sort   = function(x) x[order(x$cost, -x$gamma),]
)



## so now I can use the e1071 library for radial basis functions

svmR <- train(Class~., data=Sonar,
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     method = svmRadial2ModelInfo,
                     tuneLength = 1)
svmR$resample
svmR

RBFgrid <- expand.grid(cost=seq(2,20,length=5), gamma=seq(0.005,0.1,length=12))
svmRtuned <- train(Class~., data=trainset,
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     method = svmRadial2ModelInfo,
                     tuneGrid = RBFgrid)
svmRtuned$resample
plot(svmRtuned)
svmRtuned



pred_train <-predict(svmRtuned,trainset)
mean(pred_train==trainset$Class)
# 1

#test set predictions
pred_test <-predict(svmRtuned,testset)
mean(pred_test==testset$Class)

# 0.8139535










