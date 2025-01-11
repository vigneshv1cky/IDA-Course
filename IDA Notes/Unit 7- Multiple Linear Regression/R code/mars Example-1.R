#example of multiadaptive regression splines (MARS) for ISE/DSA 5103
#author: Charles Nicholson
#date: 10/23/2017

#library for mars is of course, earth
library(earth)
library(tidyverse)

library(ggplot2)  #for pretty graphs
library(rgl)      #for rotating graphics library

#let's create some really simple data to observe the impact 
#of using mars instead of OLS


n<-1000 #number of observations to create

#create data frame with two variables, x and y
#x is uniform random and y is normally distributed
df<-data.frame(x=9*runif(n), y= 1.2*rnorm(n))

#kind of relationship expected for OLS
df$y = df$y - 4*df$x
#and the relationship looks like this
ggplot(data=df, aes(x=x,y=y)) + geom_point() 

summary(olsFit<-lm(y~x,data=df))

#now make things complicated, by adding a "bend" in the data
df<-data.frame(x=9*runif(n), y= 1.2*rnorm(n))

#sort the data frame by x and then create some logical vectors
#to help us create some "bends" in our theoretical model
df <- df[order(df$x),]  
s1 <- df$x<4                 #x= 0 to 4 will have one slope
s2 <- df$x>=4 & df$x<9       #x= 4 to 9 will have different slope

#and here are the "bends" in the relationship of y ~ x
df[s1,"y"] <- df[s1,"y"] - 4*df[s1,"x"] 
df[s2,"y"] <- df[s2,"y"] + 17*(df[s2,"x"]-max(df[s1,"x"]))+df[which.max(df[s1,"x"]),"y"]

#and the relationship looks like this
ggplot(data=df, aes(x=x,y=y)) + geom_point() + ylim(-30, 75)

#so, what will OLS do with this?
olsFit<-lm(y~x,data=df)
summary(olsFit)  #<- a simple relationship, y = 6*x - 12

olsResults = data.frame(x=df$x, y=olsFit$fitted.values)

ggplot(data=olsResults, aes(x=x,y=y)) + geom_line(color="blue",size=2) +
      geom_point(data=df, aes(y=y), alpha=1) +   ylim(-30, 75)

hist(olsFit$residuals)

plot(olsFit)  #<- and these are wacky!

#so let's try mars
marsFit <- earth(y~x,data=df)
summary(marsFit, style="pmax")  #look at that r^2 value!  0.99+

marsResults = data.frame(x=df$x, y=marsFit$fitted.values)

#and how did it obtain such a high r^2?

#(Intercept)    -16.215392
#h(4.03407-x)     4.022613     #for x < 4, the slope is -4
#h(x-4.03407)    16.983535     #for x > 4, the slope is 17

ggplot(data=marsResults, aes(x=x,y=y)) + geom_line(color="red",size=3) +
  geom_line(data=olsResults, aes(x=x,y=y), color="blue",size=2,alpha=.25) +
  geom_point(data=df, aes(y=y), alpha=0.25)

hist(marsFit$residuals)



#######

#let's add a few more "bends" in the data to make sure we
#know how to interpret the coefficients

#x is uniform random and y is normally distributed
df<-data.frame(x=15*runif(n), y= 1.2*rnorm(n))

#sort the data frame by x and then create some logical vectors
#to help us create some "bends" in our theoretical model
df <- df[order(df$x),]  
s1 <- df$x<4                 #x= 0 to 4 will have one slope
s2 <- df$x>=4 & df$x<9       #x= 4 to 9 will have different slope
s3 <- df$x>=9 & df$x <= 12   #x= 9 to 12 will have different slope
s4 <- df$x>12                #x= 12+ will have different slope

#and here are the "bends" in the relationship of y ~ x
df[s1,"y"] <- df[s1,"y"] - 4*df[s1,"x"] 
df[s2,"y"] <- df[s2,"y"] + 17*(df[s2,"x"]-max(df[s1,"x"]))+df[which.max(df[s1,"x"]),"y"]
df[s3,"y"] <- df[s3,"y"] + max(df[s2,"y"])
df[s4,"y"] <- df[s4,"y"] - 15*(df[s4,"x"] - max(df[s3,"x"]))+df[length(df[s1,1])+length(df[s2,1])+which.max(df[s3,"x"]),"y"]

#and the relationship looks like this
ggplot(data=df,aes(x,y)) + geom_p

#so, what will OLS do with this?
olsFit<-lm(y~x,data=df)
summary(olsFit)  #<- a simple relationship
hist(olsFit$residuals)
plot(olsFit)  #<- and these are still wacky!

#so let's try mars again
marsFit <- earth(y~x,data=df)
summary(marsFit, style="pmax")  #again! look at that r^2 value!  0.99+


#and how did it do it?  look at the transformations of the variable x
#note (1) i rearranged these; (2) the numbers will be slightly differnt for you

#h(11.3692-x)     5.487042    ##<-- for all values less then 11.3; slope is -5.8
#h(x-3.44954)    18.215368    ##<-- for all values greater than 3.5, slope is 18-5.8 = 12.2
#h(x-5.80209)     4.831484    ##<-- but if also greater than 4.8, slope is 12.2+4.8 = 17  ****
#h(x-9.11266)   -18.267027    ##<-- but if also greater than 9, slope is 17-18 = -1  ***
#h(x-11.3692)    -2.870003    ##<-- but if greater than 11.3, slope is -1-2 = -3
#h(x-11.902)    -16.935078    ##<-- and if greater than 11.9, slope is -3-17 = -20  ***

plotmo(marsFit)  #this is what the transformation essentially looks like

plot(marsFit)#in bottom left quadrant, you can see that the first slope is a bit too aggressive 
                #estimated at -5.8 instead of -4; otherwise, the residuals have very little pattern


#and we kick OLS butt

#and then look at overlay of fitted values
#blue is MARS; gray is OLS
qplot(df$y, olsFit$fitted.values)+geom_point(color="gray")+
  geom_point(aes(y=marsFit$fitted.values),color="blue")+
  ylab("fitted values")

#residuals
fivenum(olsFit$residuals)
fivenum(marsFit$residuals)

#and then look at overlay of residuals values
#blue is MARS; gray is OLS
qplot(df$y, olsFit$residuals)+geom_point(color="gray")+
  geom_point(aes(y=marsFit$residuals),color="blue")+
  ylab("residuals")

#look at overlay of density of residuals for OLS and MARS
dat1<-data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals)))

dat <- rbind(data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals))),
             data.frame(y=olsFit$residuals,type=rep("O",length(olsFit$residuals))))

ggplot(dat, aes(x = y, fill = type)) + geom_density(alpha = 0.5)

#so let's add a little more...
#how about another variable: x1 and x2 as predictors of y
df<-data.frame(x1=15*runif(5000), x2=3*runif(5000), y= 1.2*rnorm(5000))
df <- df[order(df$x1),]

s1 <- df$x1<4
s2 <- df$x1>=4 & df$x1<9
s3 <- df$x1>=9 & df$x1 <= 12
s4 <- df$x1>12

df[s1,"y"] <- df[s1,"y"] - 4*df[s1,"x1"] 
df[s2,"y"] <- df[s2,"y"] + 17*(df[s2,"x1"]-max(df[s1,"x1"]))+df[which.max(df[s1,"x1"]),"y"]
df[s3,"y"] <- df[s3,"y"] + max(df[s2,"y"])
df[s4,"y"] <- df[s4,"y"] - 15*(df[s4,"x1"] - max(df[s3,"x1"]))+df[length(df[s1,1])+length(df[s2,1])+which.max(df[s3,"x1"]),"y"]

df <- df[order(df$x2),]
s5 <- df$x2 < 1.5
s6 <- df$x2 >= 1.5

df[s5,"y"] <- df[s5,"y"] - 20*df[s5,"x2"] 
val <- which.max(df[s5,"x2"])
df[s6,"y"] <- df[s6,"y"] + 20*(df[s6,"x2"]-max(df[s5,"x2"]))-30


qplot(df$x1,df$y)
qplot(df$x2,df$y)
plot3d(df)

olsFit<-lm(y~x1+x2,data=df)
summary(olsFit)
hist(olsFit$residuals)

plot(olsFit)


#plot the 3d data,
plot3d(df$x1, df$x2, df$y, type = "s", col = "red", size = .3)

#get the function for the OLS plane
coefs <- coef(olsFit)
a <- coefs["x1"]
b <- coefs["x2"]
c <- -1
d <- coefs["(Intercept)"]

#plot it on the 3d data
planes3d(a, b, c, d, alpha = 0.5)


#now for MARS again
marsFit <- earth(y~x1+x2,data=df)

summary(marsFit)
plotmo(marsFit)
plot(marsFit)


#a helper function for some plotting...
my_surface <- function(mod, xlim, ylim, n=100, ...) { 
  x1 <- seq(xlim[1], xlim[2], length=n)
  x2 <- seq(ylim[1], ylim[2], length=n)
  
  f <- function(x1, x2)
    predict(mod,newdata=data.frame(x1,x2))
  
  z = outer(x1,x2,f)
  surface3d(x1, x2, z, ...)
}

#plot it
plot3d(df$x1, df$x2, df$y, type = "s", col = "red", size = .3)
my_surface(mod=marsFit, xlim=c(0,15), ylim=c(0,3),alpha=.5 )



#look at overlay of density of residuals for OLS and MARS
dat1<-data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals)))

dat <- rbind(data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals))),
             data.frame(y=olsFit$residuals,type=rep("O",length(olsFit$residuals))))

ggplot(dat, aes(x = y, fill = type)) + geom_density(alpha = 0.5)



#so let's add a little more...
#how about another variable: x1 and x2 as predictors of y
#and an interaction term!
df<-data.frame(x1=15*runif(5000), x2=3*runif(5000), y= 1.2*rnorm(5000))
df <- df[order(df$x1),]

s1 <- df$x1<4
s2 <- df$x1>=4 & df$x1<9
s3 <- df$x1>=9 & df$x1 <= 12
s4 <- df$x1>12

df[s1,"y"] <- df[s1,"y"] - 4*df[s1,"x1"] 
df[s2,"y"] <- df[s2,"y"] + 17*(df[s2,"x1"]-max(df[s1,"x1"]))+df[which.max(df[s1,"x1"]),"y"]
df[s3,"y"] <- df[s3,"y"] + max(df[s2,"y"])
df[s4,"y"] <- df[s4,"y"] - 15*(df[s4,"x1"] - max(df[s3,"x1"]))+df[length(df[s1,1])+length(df[s2,1])+which.max(df[s3,"x1"]),"y"]

df <- df[order(df$x2),]
s5 <- df$x2 < 1.5
s6 <- df$x2 >= 1.5
s7 <- df$x2 <1.25 & df$x1 > 11

df[s5,"y"] <- df[s5,"y"] - 20*df[s5,"x2"] 
val <- which.max(df[s5,"x2"])
df[s6,"y"] <- df[s6,"y"] + 20*(df[s6,"x2"]-max(df[s5,"x2"]))-30

df[s7,"y"] <- df[s7,"y"] + 5*df[s7,"x1"]*df[s7,"x2"]

qplot(df$x1,df$y)
qplot(df$x2,df$y)
plot3d(df)


#OLS model
olsFit<-lm(y~x1*x2,data=df)  #OLS including the interaction term
summary(olsFit)

#and let's look at prediction surfaces
plot3d(df$x1, df$x2, df$y, type = "s", col = "red", size = .3)
my_surface(olsFit, xlim=c(0,15), ylim=c(0,3),alpha=.5 )

#MARS model
marsFit <- earth(y~x1+x2,data=df,degree=2)
summary(marsFit, style= "pmax")
plotmo(marsFit)
plot(marsFit)

#and let's look at prediction surfaces
plot3d(df$x1, df$x2, df$y, type = "s", col = "red", size = .3)
my_surface(marsFit, xlim=c(0,15), ylim=c(0,3),alpha=.5 )


#look at overlay of density of residuals for OLS and MARS
dat1<-data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals)))

dat <- rbind(data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals))),
             data.frame(y=olsFit$residuals,type=rep("O",length(olsFit$residuals))))

ggplot(dat, aes(x = y, fill = type)) + geom_density(alpha = 0.5)



#now for some real data

library(mlbench)
data("BostonHousing2")

BH2<-BostonHousing2  #renamed the data frame to reduce my typing...  i'm lazy like that...


#scaling the numeric data the old-fashioned way (before "tidyverse")
ind <- sapply(BH2, is.numeric)
BH2[ind] <- lapply(BH2[ind], scale)

#fit OLS model on some of the features
olsFit<-lm(cmedv~.-medv-town-tract-lon-lat,data=BH2)  #OLS including the interaction term
summary(olsFit)
hist(olsFit$residuals)

plot(olsFit)

#fit MARS model on same features + 3 degrees of interactions + 5-fold CV
marsFit <- earth(cmedv~.-medv-town-tract-lon-lat,
                data=BH2,
                degree=3,nk=50,pmethod="cv",nfold=5,ncross=5)
summary(marsFit)

summary(marsFit)
plotmo(marsFit)
plot(marsFit)


#prepare some data for plotting
plotDf <- data.frame(BH2$cmedv, olsFit$fitted.values, marsFit$fitted.values, 
                     olsFit$residuals, marsFit$residuals) %>% 
          rename(marsPred=cmedv, actual=BH2.cmedv, olsPred=olsFit.fitted.values,
                olsResiduals=olsFit.residuals, marsResiduals=cmedv.1)

#and plot results
ggplot(data=plotDf, aes(x=actual, y=olsPred)) +geom_point(alpha=0.4,color="blue")+
  geom_point(aes(y=marsPred),color="red", alpha=.6)+
  ylab("fitted values")+geom_abline(intercept = 0, slope = 1)

ggplot(data=plotDf, aes(x=actual, y=olsResiduals)) +geom_point(alpha=0.4,color="blue")+
  geom_point(aes(y=marsResiduals),color="red", alpha=.6)+
  ylab("residuals")+geom_hline(yintercept = 0)

#look at overlay of density of residuals for OLS and MARS
dat1<-data.frame(y=marsFit$residuals,type=rep("M",length(marsFit$residuals)))

str(marsFit$residuals)

dat <- rbind(data.frame(cmedv=marsFit$residuals,type=rep("M",length(marsFit$residuals))),
             data.frame(cmedv=olsFit$residuals,type=rep("O",length(olsFit$residuals))))

ggplot(dat, aes(x = cmedv, fill = type)) + geom_density(alpha = 0.5)


#you can also use "caret" with MARS, e.g.,
library(caret)
MARSfit2 <- train(data=BH2, cmedv~.-medv-town-tract-lon-lat,
                  method="earth",
                  trControl=trainControl(method = "cv",number = 5),
                  tuneGrid=expand.grid(degree = 1:3, nprune=c(10,20,30)))
MARSfit2
plotmo(MARSfit2$finalModel)
