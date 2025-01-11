library(ggplot2)

honors <- read.csv("honors.csv")  #you will need to import your data


head(honors)     # look at data
summary(honors)  # quick summary

#get counts of female and counts of honors students
table(honors$female)
109/200

table(honors$hon)
49/200

#some histograms of data
par(mfrow=c(1,3))
hist(honors$read, main="Read Scores")
hist(honors$write, main="Write Scores")
hist(honors$math, main="Math Scores")

#first logisitic regression model: no predictors

fit <- glm(data=honors, hon ~ 1, family="binomial")
summary(fit)

table(honors$hon)

#logisitic regression model with one dichotomous predictor

fit <- glm(data=honors, hon ~ female, family="binomial")
summary(fit)


#i like the CrossTable function in the package "gmodels"  -- you can also use the "table" function in base R
library(gmodels)
CrossTable(honors$hon, honors$female, prop.chisq=F, prop.t=F, prop.c=F, prop.r =F)

#or just 
table(honors$hon, honors$female)


#logisitic regression model with one continuous predictor

fit <- glm(data=honors, hon ~ math, family="binomial")
summary(fit)



#logisitic regression model with multiple predictors

fit <- glm(data=honors, hon ~ math + female + read, family="binomial")
summary(fit)
exp(coef(fit))

fit <- glm(data=honors, hon ~ math * female , family="binomial")
summary(fit)
confint(fit)
exp(coef(fit))
exp(confint(fit))



#to produce the graph used in the lecture

fit<-glm(data=honors, hon ~ read + math, family = "binomial")
summary(fit)

honors$pred<-fit$fitted.values>0.5

qplot(data=honors, x=read, y=math, size=2) + theme(legend.position = "none")

qplot(data=honors, x=read, y=math, color=as.factor(hon), size=2) +
   scale_color_manual(values = c("red", "black", "dodgerblue2")) + 
  theme(legend.position = "none")

qplot(data=honors, x=read, y=math, color=as.factor(hon), size=2) +
   scale_color_manual(values = c("red", "black", "dodgerblue2")) + 
  geom_abline(intercept = 91, slope = -0.47) + 
  theme(legend.position = "none")

qplot(data=honors, x=read, y=math, color=as.factor(pred), size=2) +
   scale_color_manual(values = c("red",  "dodgerblue2")) + 
  geom_abline(intercept = 91, slope = -0.47) + 
  theme(legend.position = "none")





