#ISE 5103 R script for introducing logistic regression diagnostics
#Author: Charles Nicholson
#October 2015


honors <- read.csv("c:/honors.csv")  #you will need to import your data


#we will use a simple model for the example
fit <- glm(data=honors, hon ~ math + read + female , family="binomial")
summary(fit)

#Note: deviance is similar to the idea of sum of squares of residuals in OLS

# there are three "types" of deviance:  NULL, Residul, and Saturated

# NULL DEVIANCE: deviance if you have an empty model
# RESIUDAL DEVIANCE: deviance based on your actual model
# SATURATED DEVIANCE: hypothetical best deviance (usally equals 0)


#the "fit" glm object has a lot of useful information
names(fit)

head(fit$data)             # all of the data is stored
head(fit$y)                # the "true" value of the binary target  
head(fit$fitted.values)    # the predicted probabilities
fit$deviance               # the residual deviance


#now let's take a look at the residuals

pearsonRes <-residuals(fit,type="pearson")
devianceRes <-residuals(fit,type="deviance")
rawRes <-residuals(fit,type="response")
studentDevRes<-rstudent(fit)
fv<-fitted(fit)

#let's go ahead and create a classification based on the probability
honors$pred<-as.numeric(fit$fitted.values>0.5)

  
predVals <-  data.frame(trueVal=honors$hon, predClass=honors$pred, predProb=fv, 
           rawRes, pearsonRes, devianceRes, studentDevRes)

tail(predVals)

#note: anytime the trueVal (hon) equals 1, the residuals are positive; 
#      otherwise, they they are negative

plot(studentDevRes) 
barplot(studentDevRes)

#let's look at logisitic regression residuals 

plot(predict(fit),residuals(fit))  #plot predicted value vs residuals
abline(h=0,lty=2,col="grey")

plot(predict(fit),residuals(fit),col=c("blue","red")[1+honors$hon])
abline(h=0,lty=2,col="grey")

# Why two lines of points? 

# We predict a probability (actually log odds) for Y taking values 0 or 1. 
# If y = 0, then we always predict more & residuals are negative (blue) 
# If y = 1, then we underestimate & residuals are positive (red). 

# There is nothing outstanding from this graph. 
# If we want to understand what is going on, we can run a local regression

rl=loess(residuals(fit)~predict(fit))
X<-data.frame(yHat=rl$x,predictedR=rl$fitted)
X<-X[order(X[,1]),]
lines(X,col="black",lwd=1)

# What are we looking for?  We'd like to know if this local regression line 
# is different from the 0 reference line?  

# Let's add some error bars...

rl=loess(residuals(fit)~predict(fit))
y=predict(rl,se=TRUE)
segments(predict(fit),y$fit+2*y$se.fit,predict(fit),y$fit-2*y$se.fit,col="green")


#standard plots available for logistic regression
plot(fit)


#now let's look at leverage and influence
barplot(cooks.distance(fit))

influence.measures(fit)

library(car)
influencePlot(fit)

vif(fit)


# There are a lot of things similar between OLS and logistic regression
# i.e., leverage, influence, variance inflation,
#       Deviance is kind of like residual sum of squares
#       p-values of predictors
#       AIC or BIC scores
#       stepwise regression (using AIC or BIC)
#       you can even perform a version of LASSO or elastic net -- use "glmnet" package


# Residuals however, are less informative for logistic regression than OLS;
# the whole 1/0 issue and the logit transformation makes them kind of pain!


# one of the most important things to do is to determine if the model has 
# good classificaion qualities

# PLEASE NOTE FOR SIMPLICITY: 
# I am skipping the cross-Validation and/or data-splitting strategies...
# In practice, remember you don't really care about the fit on training data,
# but on the test data!


# one semi-fancy SAS-like cross tabulation function
library(gmodels)
CrossTable(honors$pred, honors$hon, chisq=T)

# or the confustion matrix function and assessment metrics we have seen before
library(caret)
confusionMatrix(honors$pred, honors$hon, positive="1")

#of course you can change the threshold value...
confusionMatrix(as.numeric(fit$fitted.values>.25),honors$hon, positive="1")

#which is the point of the ROC curve!
library(ROCR) 

pred <- prediction(fit$fitted.values, fit$y)    #ROC curve for training data
perf <- performance(pred,"tpr","fpr") 

plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
abline(0, 1, col="red")  

?performance

# can also plot accuracy by average cutoff level 
perf <- performance(pred, "acc")
plot(perf, avg= "vertical",  
    spread.estimate="boxplot", 
    show.spread.at= seq(0.1, 0.9, by=0.1))

# can also look at cutoff based on different cost structure
perf <- performance(pred, "cost", cost.fp=1, cost.fn=5)
plot(perf); 



# one nice one to look at...
# difference in distribution of predicted probablities 
# for observations that were y=0 and y=1

plot(0,0,type="n", xlim= c(0,1), ylim=c(0,7),     
        xlab="Prediction", ylab="Density",  
        main="How well do the predictions separate the classes?")

 for (runi in 1:length(pred@predictions)) {
   lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "blue")
   lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
 }



#Concordant Pairs and AUC

Association=function(trueVal,predProb)
{
  Con_Dis_Data = cbind(trueVal, predProb) 
  
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])   #build a matrix of 0's 
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
           {conc[j,i]=1}
      
      else if (ones[i,2]<zeros[j,2])
           {disc[j,i]=1}
      
      else if (ones[i,2]==zeros[j,2])
           {ties[j,i]=1}
    }
  }
  
  Pairs=dim(zeros)[1]*dim(ones)[1]              #total number of pairs
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  AUC=PercentConcordance +(0.5 * PercentTied)
  
  return(list("Percent Concordance"=PercentConcordance,
              "Percent Discordance"=PercentDiscordance,
              "Percent Tied"=PercentTied,
              "Pairs"=Pairs,
              "AUC"=AUC))
}
#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#


Association(fit$y,fit$fitted.values)


#D statistic (2009)
honors.1<-predVals[predVals$trueVal==1,]
honors.0<-predVals[predVals$trueVal==0,]

mean(honors.1$predProb) - mean(honors.0$predProb)


#K-S chart  (Kolmogorov-Smirnov chart) 
# measures the degree of separation 
# between the positive (y=1) and negative (y=0) distributions

predVals$group<-cut(predVals$predProb,seq(1,0,-.1),include.lowest=T)
xtab<-table(predVals$group,predVals$trueVal)

xtab

#make empty dataframe
KS<-data.frame(Group=numeric(10),
              CumPct0=numeric(10),
              CumPct1=numeric(10),
              Dif=numeric(10))

#fill data frame with information: Group ID, 
#Cumulative % of 0's, of 1's and Difference
for (i in 1:10) {
    KS$Group[i]<-i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
}

KS  

KS[KS$Dif==max(KS$Dif),]

maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]

#and the K-S chart
ggplot(data=KS)+
   geom_line(aes(Group,CumPct0),color="blue")+
   geom_line(aes(Group,CumPct1),color="red")+
   geom_segment(x=maxGroup,xend=maxGroup,
                y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
  labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")


