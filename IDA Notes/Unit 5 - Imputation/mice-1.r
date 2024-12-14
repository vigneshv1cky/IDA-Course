# Example code to demonstrate multivariate imputation by chained equations (mice)
# ISE 5103 Intelligent Data Analytics
# Charles Nicholson
# September 2015


# the package mice: multivariate imputation by chained equations
library(mice)


# create some random sample data 
#-------------------------------------------------
n=100   #n equals the number of observations

#four variables
x1<-5*runif(n)
x2<- rnorm(n) + runif(n) - 0.5*x1
x3<-x1+2*rexp(n) + rnorm(n)
x4<-x1+x2+2*runif(n) + rnorm(n)

# let y be some function of x1, x2, and x3
y<-5*x1+4*x2+2*x3+rnorm(n)


# create a data frame from the vectors
df<-data.frame(y,x1,x2,x3,x4)

dfFull<-df  #save the full data for later use
#-------------------------------------------------


# introduce some missingness in the data for multiple variables using different rules
#-------------------------------------------------
df[y<10,"x1"]<-NA

u<-runif(n)
df[u*y>10,"x1"]<-NA

df[y+5*x3-x4 > 50,"x2"]<-NA

u<-runif(n)
df[(y*u+x3+x2) > 15,"x1"]<-NA

df[x3+x1<3,"y"]<-NA

u<-runif(n)
df[((y+x3+x1)*u > 15 & (y+x3+x1)*u < 50),"x4"]<-NA

#check the percent missing per variable
myfun<-function(x) mean(is.na(x))
apply(df,2,myfun)
#-------------------------------------------------


# perform the first two steps of MI using the "mice" command 
# create m=6 data sets and impute missing values 
imp<-mice(df,m=6,meth="norm.nob")

# the output object is quite complex!
str(imp)

#take a look at how the means and variances of the imputed values are (hopefully) converging 
imp$chainMean
imp$chainVar

#can plot those means and variances
plot(imp)

# perform the third step of MI using the "with" command
# to perform a standard analysis (in this case, a linear regression) on each data set 
fit<-with(imp, lm(y~x1+x2+x3+x4))

#perfrom the fourth step of MI, recombination, using the "pool" command 
est<-pool(fit)


plot(dfFull)      #pairs plot of full data
plot(df)          #pairs plot of available cases
plot(na.omit(df)) #pairs plot for complete cases


#coefficient estimates based on full data (before creating missingness)
summary(fullfit<-lm(data=dfFull,y~x1+x2+x3+x4))

#coefficient estimates based on complete cases (no imputation)
summary(missfit<-lm(data=df,y~x1+x2+x3+x4))

#coefficient estimates based on MICE (recombined estimates)
summary(est)



