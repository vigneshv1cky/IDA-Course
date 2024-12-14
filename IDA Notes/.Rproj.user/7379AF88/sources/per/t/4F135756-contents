#ISE 5103 R script for introducing multiple linear regression diagnostics
#Author: Charles Nicholson
#October 2015


library(car)      #for some extra diagnostic tools
library(ggplot2)  #for plotting


#example using the LifeCycleSavings data
data(LifeCycleSavings)

#basic linear model
fit<-lm(data=LifeCycleSavings,sr ~ pop15 + pop75 + dpi + ddpi)
summary(fit)


#test for non-constant variance
ncvTest(fit)


#residual plot
plot(fit$fitted.values,fit$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)

#histogram of residuals
qplot(fit$resid) + geom_histogram(binwidth=2)

#qq plot of residuals
qqnorm(fit$resid)
qqline(fit$resid)

# partial residual plots
par(mfrow=c(2,2))
plot(LifeCycleSavings$pop15,fit$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)
plot(LifeCycleSavings$pop75,fit$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)
plot(LifeCycleSavings$dpi,fit$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)
plot(LifeCycleSavings$ddpi,fit$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)
dev.off()


#index leverage plot
plot(hatvalues(fit),col = "black", pch = 21, bg = "red")      #index plot of leverages
abline(h=2*5/50)

hatvalues(fit)[hatvalues(fit)>0.2]

#plot residuals vs. hatvalues
plot(hatvalues(fit),fit$residuals,col = "black", pch = 21, bg = "red")    #leverages and residuals
abline(h=0,v=2*5/50)

#standardized residuals (index plot) ----- 
plot(rstandard(fit),col = "black", pch = 21, bg = "red")      # index standardized residual plot
abline(h=c(-2,2), lty = 2)

#we can interactively identify points
identify(1:50,rstandard(fit),labels=names(fit$fitted.values))  #interactive click and identify points


#standardized residuals vs. fitted values ----- 
plot(fit$fitted.values,rstandard(fit),col = "black", pch = 21, bg = "red")      #standardized residuals and fitted values
abline(h=c(-2,2), lty = 2)

identify(fit$fitted.values,rstandard(fit),labels=names(fit$fitted.values))


# studentized residuals vs. fitted values ---------
plot(fit$fitted.values,rstudent(fit),col = "black", pch = 21, bg = "red")      #standardized residuals and fitted values
abline(h=c(-2,2), lty = 2)                                                     # FYI -- in this case it looks almost the same as the previous plot
identify(fit$fitted.values,rstandard(fit),labels=names(fit$fitted.values))

#outlier test from car package
outlierTest(fit)


#studentized residals vs. Cook's D
plot(cooks.distance(fit),rstudent(fit),col = "black", pch = 21, bg = "red")

# automatic plots from R for linear models
plot(fit)


# several influence measures
influence.measures(fit)

# influence plot from car package
influencePlot(fit)

# residual plots from car package
residualPlots(fit)

#variation inflation factor from car package
vif(fit)





