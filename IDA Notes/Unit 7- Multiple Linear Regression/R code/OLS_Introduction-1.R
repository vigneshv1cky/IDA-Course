#ISE 5103 R script for introducing multiple linear regression
#Author: Charles Nicholson
#October 2015

#we will use the LifeCycleSavings data from the package "datasets"
data(LifeCycleSavings)

#the goal is to manually compute the regression coefficients
#and standard errors for this model: sr ~ pop15 + pop75 + dpi + ddpi


Y<-LifeCycleSavings$sr 

X<-as.matrix(LifeCycleSavings[,2:5])
X<-cbind(intercept=1,X) 

head(X)


#X'X
t(X)%*%X

#(X'X)^-1
solve(t(X)%*%X)

#beta = [(X'X)^-1]X'Y
(beta<-solve(t(X)%*%X)%*%t(X))%*%Y

fit<-lm(data=LifeCycleSavings,sr ~ pop15 + pop75 + dpi + ddpi)
summary(fit)

#residual variance from the regression output: 3.803^2

#also called "mean square residuals" in the anova function
anova(fit)

residualSE<-sqrt(anova(fit)[[3]][5])


#we can compute the standard error of the beta's
diag(residualSE *sqrt(solve(t(X)%*%X)))


#let's look at lm object "fit" in detail
str(fit)

#and the output
summary(fit)

#and the AIC and BIC scores
AIC(fit)
BIC(fit)


#if we modify the model by removing a variable, we can refit and compare...
fit2<-update(fit,~.-dpi)
summary(fit2)
AIC(fit2)
BIC(fit2)

#and again, we can refit and compare...
fit3<-update(fit2,~.-pop75)
summary(fit3)
AIC(fit3)
BIC(fit3)

#what about making a very complex model -- all effects and interations
fit4<-lm(data=LifeCycleSavings,sr~ pop15*pop75*dpi*ddpi)
summary(fit4)
AIC(fit4)
BIC(fit4)




LC<-LifeCycleSavings

LC$pop15_pop75 <- LC$pop15*LC$pop75         
LC$pop15_dpi  <- LC$pop15*LC$dpi              
LC$pop75_dpi  <- LC$pop75*LC$dpi            
LC$pop15_ddpi <- LC$pop15*LC$ddpi           
LC$pop75_ddpi <- LC$pop75*LC$ddpi           
LC$dpi_ddpi   <- LC$dpi*LC$ddpi           
LC$pop15_pop75_dpi   <- LC$pop15*LC$pop75*LC$dpi       
LC$pop15_pop75_ddpi  <- LC$pop15*LC$pop75*LC$ddpi       
LC$pop15_dpi_ddpi    <- LC$pop15*LC$dpi*LC$ddpi        
LC$pop75_dpi_ddpi    <- LC$pop75*LC$dpi*LC$ddpi    
LC$pop15_pop75_dpi_ddpi <- LC$pop15*LC$pop75*LC$dpi*LC$ddpi 


heatmap(cor(LC[-1]),col=grDevices::heat.colors(100))
      

#how many "dimensions" do we really have here?
pca<-prcomp(LC[-1],scale=T)
summary(pca)
plot(pca)


#digression ----- multicollinearity example
x1 <- rnorm(100)
x2 <- 10 * x1 + 0.001*rnorm(100)
y<-0.1*rnorm(100)+x1 
cor(x1,x2)
fitM<-lm(y~x1+x2)
summary(fitM)


#stepwise selection example

#we can start with this model and use a stepwise selection technique
fit5<-stepAIC(fit4,direction="both")
summary(fit5)
AIC(fit5)
BIC(fit5)

fit5$anova

#but maybe we can improve it?
fit6<-update(fit5,~.-ddpi)
summary(fit6)
AIC(fit6)
BIC(fit6)

#what if we tried to contstruct a new feature?
LifeCycleSavings$popRatio<-LifeCycleSavings$pop15/LifeCycleSavings$pop75

#and then repeat the above processes to 'search' for a better model...
#such as: 
fit7<-lm(data=LifeCycleSavings,sr~ pop15 + dpi + dpi:ddpi + popRatio)
summary(fit7)
AIC(fit7)
BIC(fit7)



