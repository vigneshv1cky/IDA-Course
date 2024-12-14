#robust regression

library(ggplot2)  #for pretty plots

#we will use the LifeCycleSavings data from the package "datasets"
data(LifeCycleSavings)

head(LifeCycleSavings)

olsFit<-lm(data=LifeCycleSavings,sr ~ pop15 + pop75 + dpi + ddpi)
summary(olsFit)

#Coefficients:
#(Intercept)     pop15      pop75      dpi      ddpi 
#28.5661        -0.4612    -1.6915   -0.0003   0.4097


#now with robust regression
library(MASS)  #for M-estimation method of robust regression: rlm

?rlm

#-----first, huber loss function-------
huberFit <- rlm(data=LifeCycleSavings,sr ~ pop15 + pop75 + dpi + ddpi)
summary(huberFit)
str(huberFit)

#let's look at a few the top residuals and their weights
hweights <- data.frame(country = row.names(LifeCycleSavings), 
                      resid = huberFit$resid, weight = huberFit$w)
hweights <- hweights[order(huberFit$w), ]
hweights[1:15, ]

#notice that Zambia and Chile top our list!

#even though there is a lot of down-weighting occuring, the 
#coefficient estimates are still pretty close

#(Intercept)   pop15     pop75       dpi      ddpi 
#28.9447     -0.4735   -1.6554   -0.0004    0.3850 


#----now for bi-square-----
bisquareFit <- rlm(data=LifeCycleSavings,sr ~ pop15 + pop75 + dpi + ddpi, psi=psi.bisquare)

#and the corresponding residuals and weights are different
bweights <- data.frame(country = row.names(LifeCycleSavings), resid = bisquareFit$resid, weight = bisquareFit$w)
bweights <- bweights[order(bisquareFit$w), ]
bweights[1:15, ]

summary(bisquareFit)

#the coefficients are affected, but it's not too large of a departure from the OLS results

#(Intercept)   pop15     pop75        dpi      ddpi 
#28.6711     -0.4699   -1.5790    -0.0004    0.3756 

#and we can see that that the OLS predictions (gray) and bisquare predictions (blue)
#both line up pretty closely on a predictions vs. true value plot
qplot(LifeCycleSavings$sr, 
      olsFit$fitted.values)+geom_point(color="gray")+
      geom_point(aes(y=bisquareFit$fitted.values),color="blue")+ylab("fitted values")

#--------------------
#artificially make one outcome value a big outlier and then re-run
LifeCycleSavings2
LifeCycleSavings[order(LifeCycleSavings$pop75),]
LifeCycleSavings2<-LifeCycleSavings
LifeCycleSavings2[14,1]<-65   #outlier introduced here for France


qplot(LifeCycleSavings$sr, bins=20)   #look at histogram of original data
qplot(LifeCycleSavings2$sr, bins=20)  #look at new histogram

olsFit2<-lm(data=LifeCycleSavings2,sr ~ pop15 + pop75 + dpi + ddpi)
huberFit2 <- rlm(data=LifeCycleSavings2,sr ~ pop15 + pop75 + dpi + ddpi)
bisquareFit2 <- rlm(data=LifeCycleSavings2,sr ~ pop15 + pop75 + dpi + ddpi, psi=psi.bisquare)

round(coef(olsFit),4)
round(coef(olsFit2),4)  #big differences! especially for pop15 and pop75

round(coef(huberFit),4)
round(coef(huberFit2),4) #somewhat similar to previous results and original OLS

round(coef(bisquareFit),4)
round(coef(bisquareFit2),4) #similar to previous results and original OLS

#take a quick look at the weights for France in bisquare and see it is set to 0
bweights2 <- data.frame(country = row.names(LifeCycleSavings), resid = bisquareFit2$resid, weight = bisquareFit2$w)
bweights2 <- bweights2[order(bisquareFit2$w), ]
bweights2[1:5, ]

#now the OLS predictions (gray) and bisquare predictions (blue)
#do not line up closely on a predictions vs. true value plot
qplot(LifeCycleSavings$sr, olsFit2$fitted.values)+geom_point(color="gray")+geom_point(aes(y=bisquareFit2$fitted.values),color="blue")+ylab("fitted values")

#the bisquare weigting eliminated France from the analysis
#you can see this if you delete France and then re-run the OLS model,
#the results line up very closely!

LifeCycleSavings3<-LifeCycleSavings[-14,]  #remove France
olsFit3<-lm(data=LifeCycleSavings3,sr ~ pop15 + pop75 + dpi + ddpi)
round(coef(olsFit3),4)  
round(coef(bisquareFit2),4) 

olsPrediction<-predict(olsFit3,newdata=LifeCycleSavings)
qplot(LifeCycleSavings$sr, olsPrediction)+geom_point(color="gray")+geom_point(aes(y=bisquareFit2$fitted.values),color="blue")+ylab("fitted values")
