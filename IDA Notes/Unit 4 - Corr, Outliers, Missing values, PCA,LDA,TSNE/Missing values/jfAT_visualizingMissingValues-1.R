
# Question: 
# How can I look at the proportion missing for each factor or group level?

# To illustrate, let me create some 'missingness' in the Iris data:

data(iris)

iris[1:15,1] <- NA        #setting column 1 to NA for observations 1 thru 15 
iris[1:35,2] <- NA        #setting column 2 to NA for observations 1 thru 35 
iris[70:85,3] <- NA       #setting column 3 to NA for observations 70 thru 85
iris[110:120,4] <- NA     #setting column 4 to NA for observations 110 thru 120


# One solution is a creative use of the "aggregate" function
# USAGE: 
# df<-aggregate(DATAFRAME, by=list(GROUPING LIST), function(x) USER-DEFINED FUNCTION)

# where DATAFRAME is the data set iris; 
# GROUPING LIST is the set of values we use as factor levels
# and the USER-DEFINED FUNCTION is a function we create to take 
# the average of the is.na() logical vector.

# quick digression:  -- we can perform sums and means on logic vectors, e.g.
mean(is.na(iris$Sepal.Length))  # --  overall missing proportion for Sepal.Length

# so we can do now do the following:
aggregate(iris, by=list(iris$Species), function(x) mean(is.na(x)))



# Question: 
# How can I look patterns of missingness among variables?

# let's look at some packages that deal with Missing Values explicitly
# two packages: mice and VIM

install.packages("mice")   #install and load the MICE
library(mice)              #load the MICE package:  Multivariate Imputation by Chained Equations 


data(mtcars)             #some data to play with

#let's just grab a few of the variables
mtcars<-mtcars[,c("mpg","hp","qsec")]

# set a few variable cases to missing 
mtcars[1:9,"mpg"]<-NA    
mtcars[7:11,"hp"]<-NA
mtcars[c(2,3,10,19,31),"qsec"]<-NA

# can use the mice function "md.pattern" 
# and "md.pairs" to get a quick overview of missingness
 
#check out ?md.pairs and ?md.pattern for details
?md.pairs
?md.pattern

md.pairs(mtcars)
md.pattern(mtcars)       




# Question: 
# What about visualizing missingness?

library(VIM)  #package for "Visualization and Imputation of Missing Values"

# can use VIM's "aggr" function to also get overall information on missing
a<-aggr(mtcars)
summary(a)

# use VIM function "marginplot" to get a scatter plot that includes information on missing values
marginplot(mtcars[c("mpg","qsec")], col = c("blue", "red", "orange"))

# can also look at all of the plots with Missing Information
scattmatrixMiss(mtcars)



#----------------------------------------------------------------------------------


#create some more fake data and examine different kinds of MVM

x<-rexp(1000)
y<-rnorm(1000) 
z<-runif(1000)


df<-data.frame(x,y)
scattmatrixMiss(df)   #no missing data

df[z>0.9,"y"]<-NA  #MCAR
sum(is.na(df[,"y"]))
scattmatrixMiss(df) 


df<-data.frame(x,y)
df[df$x>2.1,"y"]<-NA  #MAR
sum(is.na(df[,"y"]))
scattmatrixMiss(df)


df<-data.frame(x,y)
df[df$y>1.10,"y"]<-NA  #MNAR
sum(is.na(df[,"y"]))
scattmatrixMiss(df)


