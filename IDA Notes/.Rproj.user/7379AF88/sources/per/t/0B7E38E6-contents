#Example of single imputation techniques for ISE 5103 Intelligent Data Analytics
#Charles Nicholson
#September 2015

#load appropriate libraries
library(VIM)
library(mice)


# CREATE A SET OF FAKE DATA  (y ~ x) ------------
x<-rexp(1000)
y<-0.5*rnorm(1000) + 0.5*x       
z<-runif(1000)

alpha<-runif(1000) # not included in dataframe
beta<-runif(1000)  # not included in dataframe

df<-data.frame(x,y,z)

xmax<-ceiling(max(df$x))
ymax<-ceiling(max(df$y))
ymin<-floor(min(df$y))


#scatterplot would look like this if there were NO MISSING INFORMATION
plot(df$x,df$y,ylim=c(ymin,ymax), xlim=c(0,xmax) , xlab="x", ylab="y")

# now lets create some missing values....
dfMiss <- df

dfMiss[df$y>1.30,"y"]<-NA           #MNAR
dfMiss[alpha<0.2,"z"]<-NA           #MCAR
dfMiss[beta>0.90,"y"]<-NA           #MCAR
dfMiss[df$x>2.65,"y"]<-NA           #MAR

missing <- is.na(dfMiss$y)
sum(missing)
dfMiss$missing <- missing

#scatterplot now looks like this...
plot(dfMiss$x,dfMiss$y,ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")


#imputaion by "hotdeck" --------------------------------------------------------
dfHD.imp <- dfMiss

#sample m values from from the non-missing data (with replacement)
hotdeck <- dfHD.imp[!missing,"y"]  # create sample pool

n <- length(hotdeck)    #size of sample pool
m <- sum(missing)    #how many samples do I need?

hotdeck <- hotdeck[sample(n,m,replace=TRUE)]

dfHD.imp[missing,"y"]<-hotdeck

plot(df$x,df$y,ylim=c(-1.3,4.25))    #plot of all data (no missings)

#plot data with hotdeck imputation -- imputed values in red
plot(dfHD.imp$x, dfHD.imp$y, col = factor(dfHD.imp$missing), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")

par(mfrow=c(2,1))   #setup graphics device to make two plots on the screen

hist(df$y, xlim=c(-1,xmax), main="All Data", xlab="x")   #histogram of all data
trueMV<-round(mean(df$y),3)                               
trueVar<-round(var(df$y),3)
abline(v = trueMV, col = "blue", lwd = 2)                    # add a line for the mean
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar)) # add text for mean and var

hist(dfHD.imp$y, xlim=c(-1,xmax), main="Hot Deck", xlab="x")
mv<-round(mean(dfHD.imp$y),3)
svar<-round(var(dfHD.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 100, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))   # reset graphics device to the default 1 plot



#imputation by mean ---------------------------------------------------------

dfMean.imp<-dfMiss  #copy of the data with missings

dfMean.imp[missing,"y"]<-mean(dfMean.imp$y,na.rm=T)   #imputation by mean


par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfMean.imp$y, xlim=c(-1,xmax), main="Mean Imputation", xlab="x")
mv<-round(mean(dfMean.imp$y),3)
svar<-round(var(dfMean.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",mv, "  Var:", svar))
par(mfrow=c(1,1))

plot(dfMean.imp$x, dfMean.imp$y, col = factor(dfMean.imp$missing), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")



#imputation by "regression"  ---------------------------------------------

fit<-lm(dfMiss$y~dfMiss$x)    # fit a linear model to the data
f<-summary(fit)
print (f)  

str(f)

c<-f[[4]]                     # extract the coefficients 
se<-f[[6]]                    # extract the model standard error

dfReg.imp <- dfMiss
dfReg.imp[missing,"y"]<- (c[1] + c[2]*dfReg.imp[missing,"x"])   #imputataion with regression


par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfReg.imp$y, xlim=c(-1,xmax), main="Regression Imputation", xlab="x")
mv<-round(mean(dfReg.imp$y),3)
svar<-round(var(dfReg.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))

plot(dfReg.imp$x, dfReg.imp$y, col = factor(dfReg.imp$missing),ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")



# USE THE mice PACKAGE FOR Predictive Mean Matching (PMM) -----------------------
dfPMM.imp <- dfMiss

#imputation by PMM
dfPMM.imp[missing,"y"] <- mice.impute.pmm(dfPMM.imp$y, !dfPMM.imp$missing, dfPMM.imp$x)

plot(dfPMM.imp$x, dfPMM.imp$y, col = factor(dfPMM.imp$missing), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")

par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax),main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfPMM.imp$y, xlim=c(-1,xmax), main="Predictive Mean Matching", xlab="x")
mv<-round(mean(dfPMM.imp$y),3)
svar<-round(var(dfPMM.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 80, label=paste("Mean:",mv, "  Var:", svar))
par(mfrow=c(1,1))



#imputation by "regression" plus random error -------------------------

dfRegErr.imp <- dfReg.imp

#imputation by regression with error (remember that se = standard error of model)
dfRegErr.imp[missing,"y"] <- dfRegErr.imp[missing,"y"] + rnorm(sum(missing),0,se**2)

par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfRegErr.imp$y, xlim=c(-1,xmax), main="Regression Imputation with Error", xlab="x")
mv<-round(mean(dfRegErr.imp$y),3)
svar<-round(var(dfRegErr.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))

plot(dfRegErr.imp$x, dfRegErr.imp$y, col = factor(dfRegErr.imp$missing), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")



# k-nearest neighbor from VIM package (kNN imputation) ----------------------------

dfKNN.imp <- kNN(dfMiss[,1:3],k=5)
plot(dfKNN.imp$x, dfKNN.imp$y, col = factor(dfKNN.imp$y_imp), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")

par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfKNN.imp$y, xlim=c(-1,xmax), main="k-Nearest Neighbor", xlab="x")
mv<-round(mean(dfKNN.imp$y),3)
svar<-round(var(dfKNN.imp$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 100, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))


# for fun, try kNN with 400 neighbors....  it takes a few seconds...

dfKNN400.imp <- kNN(dfMiss[,1:3],k=400)
plot(dfKNN400.imp$x, dfKNN400.imp$y, col = factor(dfKNN400.imp$y_imp), ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")



#in summary...

par(mfrow = c(2,2))
plot(df$x,df$y,ylim=c(-1.3,4.25), main="All Data")
plot(dfMean.imp$x, dfMean.imp$y, col = factor(dfMean.imp$missing), main="Mean", ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")
plot(dfHD.imp$x, dfHD.imp$y, col = factor(dfHD.imp$missing), main="Hot Deck", ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")
plot(dfReg.imp$x, dfReg.imp$y, col = factor(dfReg.imp$missing), main="Regression", ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")

plot(df$x,df$y,ylim=c(-1.3,4.25), main="All Data")
plot(dfPMM.imp$x, dfPMM.imp$y, col = factor(dfPMM.imp$missing),  main="Predictive Mean Matching",ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")
plot(dfKNN.imp$x, dfKNN.imp$y, col = factor(dfKNN.imp$y_imp),  main="k-Nearest Neighbors", ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")
plot(dfRegErr.imp$x, dfRegErr.imp$y, col = factor(dfRegErr.imp$missing) , main="Regression with Random Error",ylim=c(ymin,ymax), xlim=c(0,xmax), xlab="x", ylab="y")
   

     
#there are other methods as well, e.g.,

#Sequential imputation for missing values ----------------------- 
# need to install library rrcovNA first

library(rrcovNA)   

dfSeq.imp <- data.frame(impSeq(dfMiss[,1:3]))
dfSeq<-merge(dfSeq.imp,dfMiss[,c(1,4)],by.x="x", by.y="x")

par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfSeq$y, xlim=c(-1,xmax), main="Sequential Imputation")
mv<-round(mean(dfSeq$y),3)
svar<-round(var(dfSeq$y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 100, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))

plot(df$x,df$y,ylim=c(ymin,ymax), xlim=c(0,xmax), main="All Data", xlab="x", ylab="y")
plot(dfMiss$x,dfMiss$y,ylim=c(ymin,ymax), xlim=c(0,xmax), main="Data without Imputation", xlab="x", ylab="y")
plot(dfSeq$x,dfSeq$y,ylim=c(ymin,ymax), xlim=c(0,xmax), col = factor(dfSeq$missing), main="Sequential Imputation", xlab="x", ylab="y")


#robust sequential imputation ----------------------------------
dfSeq.imp <- data.frame(impSeqRob(dfMiss[,1:3]))
dfSeq<-merge(dfSeq.imp,dfMiss[,c(1,4)],by.x="x.x", by.y="x")

par(mfrow=c(2,1))
hist(df$y, xlim=c(-1,xmax), main="All Data")
abline(v = trueMV, col = "blue", lwd = 2)
text(4, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfSeq$x.y, xlim=c(-1,xmax), main="Robust Sequential Imputation")
mv<-round(mean(dfSeq$x.y),3)
svar<-round(var(dfSeq$x.y),3)
abline(v = mv, col = "blue", lwd = 2)
text(4, 100, label=paste("Mean:",mv, "  Var:", svar))

par(mfrow=c(1,1))

plot(df$x,df$y,ylim=c(ymin,ymax), xlim=c(0,xmax), main="All Data", xlab="x", ylab="y")
plot(dfSeq$x.x,dfSeq$x.y,ylim=c(ymin,ymax), xlim=c(0,xmax), col = factor(dfSeq$missing), main="Robust Sequential Imputation", xlab="x", ylab="y")
