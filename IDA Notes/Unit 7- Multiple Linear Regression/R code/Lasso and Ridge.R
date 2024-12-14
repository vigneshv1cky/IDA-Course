# STAT 849, Theory and Applied Regression and ANOVA I
#  Fifteenth discussion, 12-14-2012
#  TA: Guilherme Ludwig
#  with minor edits by Charles Nicholson (cnicholson@ou.edu)
#
# Do yourself a favor and buy this book (Amazon has it on sale every now and then):
# http://www-stat.stanford.edu/~tibs/ElemStatLearn/
#
# The following dataset is from Hastie, Tibshirani and Friedman (2009), from a study 
# by Stamey et al. (1989) of prostate cancer, measuring the correlation between the level 
# of a prostate-specific antigen and some covariates. The covariates are
#
# * lcavol  : log-cancer volume
# * lweight : log-prostate weight
# * age     : age of patient
# * lbhp    : log-amount of benign hyperplasia
# * svi     : seminal vesicle invasion
# * lcp     : log-capsular penetration
# * gleason : Gleason Score, check http://en.wikipedia.org/wiki/Gleason_Grading_System
# * pgg45   : percent of Gleason scores 4 or 5
#
# And lpsa is the response variable, log-psa.


url <- "http://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data"
str(pcancer <- read.table(url, header=TRUE))

# There's a training sub-dataset that we will focus on. Later, we will try to predict
# the values of the remaining observations.

head(pcancer)

train <- pcancer[which(pcancer$train),1:9]
calibrate <- pcancer[-which(pcancer$train),1:9]

# The data looks like this

plot(train)

# Of course, given that this is a biological dataset, the covariates are correlated

round(cor(train),3)

# We fit a linear model and now focus on fixing multicollinearity
model.ls <- lm(lpsa ~ ., data=train)
coef(model.ls)
rss.ls <- sum(model.ls$resid^2)/model.ls$df.residual    #compute the mean of residual sum of squares

library(car)
vif(model.ls)   #look at VIF scores

# no individual VIF score is extremely bad, but the overall average is high enough that we might be concerned


# #######################
# # STEPWISE REGRESSION # 
# #######################


#backwards vs. forwards 


model.backward <- step(model.ls, direction="backward")
summary(model.backward)
rss.backward <- sum(model.backward$resid^2)/model.backward$df.residual

# So backward selection using AIC drops "gleason" from the model. The final AIC
# is -39.103.
#
# Note that for scope, the formula ~. means "as current model". Forward selection
# is poorly explained in R...
# Foward selection starts with an empty model and adds variables up to the "scope" model

scope <- list(upper=~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, lower=~.)
model.forward <- step(lm(lpsa ~ 1, data=train), scope, direction="forward")


?step
rss.forward <- sum(model.forward$resid^2)/model.forward$df.residual

# So we conclude that forward selection using AIC keeps lcavol, lweight, svi and lbph.
# The AIC is -37.825.
#
# Note that the paths are arbitrary and such model selection procedures are called 
# "myopic" sometimes, unlike Lasso Regression which has the "Oracle" property. 

#For example, we may compute R^2 and AICs for all possible subsets.

r2 <- list()
AICs <- list()
for(i in 1:8){
    indexes <- combn(8,i)  #combn(n,r) produces a list of all combinations: n choose r
    currentr2 <- NULL
    currentAIC <- NULL
    for(j in 1:dim(indexes)[2]){
        temp.model <- lm(lpsa ~ ., data=train[,c(indexes[,j], 9)])
        currentr2[j] <- summary(temp.model)$r.squared
        currentAIC[j] <- AIC(temp.model)
        }
    r2[[i]] <- currentr2
    AICs[[i]] <- currentAIC
    }

# let me find the corresponding r^2 and AIC entries for the paths chosen by
# backward and forward elimination... this code is a bit clumsy but it gets
# what we need.

compare <- function(set){
    s <- length(set)
    temp <- combn(8,s)
    check <- NULL
    for(i in 1:dim(temp)[2]){
        check[i] <- all(temp[,i]==set)
    }
    return(which(check))
}

backward <- compare(c(1:6,8))
forward <- c(compare(1), compare(1:2), compare(c(1,2,5)), compare(c(1,2,4,5)))

r2.b <- c(r2[[7]][backward], r2[[8]])
r2.f <- c(r2[[1]][forward[1]], r2[[2]][forward[2]], r2[[3]][forward[3]], r2[[4]][forward[4]])
AICs.b <- c(AICs[[7]][backward], AICs[[8]])
AICs.f <- c(AICs[[1]][forward[1]], AICs[[2]][forward[2]], AICs[[3]][forward[3]], AICs[[4]][forward[4]])

# We now can take a look at how backward/forward performs!

layout(matrix(1:2, ncol=2))
plot(0, xlim=c(0,9), ylim=c(0,0.8), type="n", ylab=expression(r^2), main="Fitting criteria")
for(i in 1:8){
    points(rep(i, length(r2[[i]])), r2[[i]], pch=21, bg="Grey")
    }
points(7:8, r2.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, r2.f, bg="Blue", col="Blue", pch=21, type="o")
plot(0, xlim=c(0,9), ylim=c(153,217), type="n", ylab="AIC", main="AIC")
for(i in 1:8){
    points(rep(i, length(AICs[[i]])), AICs[[i]], pch=21, bg="Grey")
    }
points(7:8, AICs.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, AICs.f, bg="Blue", col="Blue", pch=21, type="o")

# ####################
# # RIDGE REGRESSION # 
# ####################

library(ggplot2)
library(car)
library(MASS)

dev.off()  #reset the graphics window


model.ridge <- lm.ridge(lpsa ~ ., data=train, 
                        lambda = seq(0,10,0.1))  #ridge regression is run for several values of lambda

str(model.ridge)

# The optimal lambda is given by

qplot(model.ridge$lambda,model.ridge$GCV )        # plots generalized cross validated error (GCV) vs the tuning parameter  
which.min(model.ridge$GCV)   # identifies the value for lambda with the smallest associated GCV
lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]   #save the value of optimal lambda for future use


# the optimal lambda value is 4.9

#here are all of the coefficients for every lambda value  -- too much info!
coef(model.ridge)

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
    ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train)[-9], pos=4, col=colors)

beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),]
resid.ridge <- train$lpsa - beta.ridge[1] - as.matrix(train[,1:8])%*%beta.ridge[2:9]

# To find df
d <- svd(as.matrix(train[,1:8]))$d
df <- 67 - sum(d^2/(lambda.ridge+d^2))

rss.ridge <- sum(resid.ridge^2)/df     #compute the mean RSS for ridge regression ()

# ####################
# # LASSO REGRESSION # 
# ####################
#
# In Lasso Regression, the coefficients are penalized by the L1 norm. The 
# optimal value for lambda is chosen by cross-validation.


library(lars)

# the method "lars" has a slightly different interface than "lm" -- there is no formula input
# instead we supply a matrix of predictors and a vector of target values 

y <- as.numeric(train[,9])      #target variable
x <- as.matrix(train[,1:8])     #predictors


model.lasso <- lars(x, y, type="lasso")
plot(model.lasso)
model.lasso$lambda
summary(model.lasso)
lambda.lasso <- c(model.lasso$lambda,0)
beta <- coef(model.lasso)

str(model.lasso)

colors <- rainbow(8)

matplot(lambda.lasso, beta, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
    ylab=expression(hat(beta)), col=colors)
text(rep(-0, 9), beta[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)


# I'll keep the lambda=1.7305 betas

beta.lasso <- beta[4,]
resid.lasso <- train$lpsa - predict(model.lasso, as.matrix(train[,1:8]), s=4, type="fit")$fit
rss.lasso <- sum(resid.lasso^2)/(67-4)



#################
#side note:
 
#there are many ways to run LASSO, etc.

#for example -- this above example did not use any CV
# we can use cv.lars to help:

cvlas<-cv.lars(x,y,type="lasso",mode="fraction")
cvlas
opt.frac <- min(cvlas$cv) + sd(cvlas$cv) 
opt.frac <- cvlas$index[which(cvlas$cv < opt.frac)[1]]
lasso.path <- lars(x, y, type = "lasso")
summary(lasso.path)
lasso.fit <- predict.lars(lasso.path, type = "coefficients", mode = "fraction", s = opt.frac)
coef(lasso.fit)



#or a function that might even be easier (from the glmnet package)
library(glmnet)

cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit

# #########################
# # PARTIAL LEAST SQUARES #
# #########################
#
# Partial Least Squares is pretty much like principal components regression, but
# we use information from Y to select weights for the principal components of X.

library(pls)
model.pls <- plsr(lpsa ~ ., 8, data = train, method = "oscorespls", validation = "CV")
summary(model.pls)

plot(model.pls)

plot(model.pls, ncomp = 1, asp = 1, line = TRUE)
plot(model.pls, ncomp = 2, asp = 1, line = TRUE)
plot(model.pls, ncomp = 3, asp = 1, line = TRUE)
plot(model.pls, ncomp = 4, asp = 1, line = TRUE)
plot(model.pls, ncomp = 8, asp = 1, line = TRUE)

plot(RMSEP(model.pls),legendpos="topright")

# I'm eyeballing CV here, but fitting 4 components should be enough. So I'll update
# the model

model.pls <- plsr(lpsa ~ .,4, data = train, method = "oscorespls")


plot(model.pls, plottype = "scores", comps = 1:3)

beta.pls <- drop(coef(model.pls))
resid.pls <- drop(model.pls$resid)[,4]
rss.pls <- sum(resid.pls^2)/(67-4)



# #########################
# # COMPARISON OF FITTING #
# #########################
#
# This is as straightforward as it gets:

rss.ls
rss.backward
rss.forward
rss.ridge
rss.lasso
rss.pls

# ############################
# # COMPARISON OF PREDICTION #
# ############################
#
# We can also compare with the predicition dataset we saved from before. In this case

y.new <- calibrate$lpsa

pss.ls <- sum((y.new - predict(model.ls, calibrate[,1:8]))^2)
pss.backward <- sum((y.new - predict(model.backward, calibrate[,1:8]))^2)
pss.forward <- sum((y.new - predict(model.forward, calibrate[,1:8]))^2)
pss.ridge <- sum((y.new - beta.ridge[1] - as.matrix(calibrate[,1:8])%*%beta.ridge[2:9])^2)
pss.lasso <- sum((y.new - predict(model.lasso, as.matrix(calibrate[,1:8]), s=4, type="fit")$fit)^2)
pss.pls <- sum((y.new - drop(predict(model.pls, calibrate[,1:8], 4)))^2)

pss.ls
pss.backward
pss.forward
pss.ridge
pss.lasso
pss.pls

# In this case Forward AIC-stepwise Regression did the best job at predicting, followed by Lasso, then 
# Ridge regression.

