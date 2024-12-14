#script used in ISE 5103 class to introduce R


# mtcars is a data frame that is included with the R distribution
data(mtcars)  

# since this is an R packaged dataset, it comes with "help"
help(mtcars)

# lists the first few records
head(mtcars)  

# lists the last few records
tail(mtcars) 

# look at the "structure" of the object
str(mtcars)

# To retrieve data in a cell, 
# enter its row and column coordinates 
# in the single square bracket "[]" operator. 
mtcars[1,6]


# to extract one or more rows of data -----------------

mtcars[7,]   # extracts the 7th row of data

mtcars[7:10,] # extracts the 7th-10th rows of data

mtcars[c(3, 24),]  #extracts rows 3 and 24

mtcars[mtcars$mpg<15,]  # extracts rows where mpg < 15

# extracts rows where mpg > 15 and horsepower > 200
mtcars[mtcars$mpg>15 & mtcars$hp>200,]  


#can also extract rows based on other vectors, e.g. a logical vector

# create a new logical vector that is TRUE if automatic, 
# and FALSE if standard transmission
L <- mtcars$am == 0 
L 

 
#use this to return only rows with automatic transmission
mtcars[L,]

#and this to return only rows that do NOT have automatic transmission
mtcars[!L,]

mtcars[!L,]$mpg  # here is their mpg



#you can also extract columns of data  ---------

mtcars[,1]

x <- mtcars[,1]       # can save the results into a variable
head(x)

x <- mtcars[,"mpg"]
head(x)

x <- mtcars$mpg   # the $ is used to delineate the datframe and var name
head(x)

x <- mtcars[,1:3]     # extracts the first 3 columns of data
head(x)

x <- mtcars[,c("mpg","hp")]   # extracts two specific columns of data
head(x)

x <- mtcars[1:5,2:4]    #extracts rows 1-5, and columns 2-4
head(x)



# returns the number of rows in mtcars
nrow(mtcars)

# returns the number of columns in mtcars
ncol(mtcars) 

#basic stats for each variable
summary(mtcars)  


#and individual stats
mean(mtcars$mpg)
mad(mtcars$mpg)
sd(mtcars$mpg)


library()   #shows all installed packages

search()  #show all packages currently loaded in the library

#to display all of the data that is available in loaded packages
data()


library(help = "stats")

#can also access help through the "packages" window

# the "?" command is a shortcut for getting help
?fivenum

fivenum(mtcars$mpg)   #Tukey's five number for mpg

boxplot(mtcars$mpg)   #same values depicted on a boxplot

?boxplot   #get help for the boxplot function

#using the formula input
boxplot(mtcars$mpg ~ mtcars$am)


#the basic graphics package has several useful tools

# histograms
hist(mtcars$mpg)
hist(mtcars$hp)

#scatterplots
plot(mtcars$mpg ~ mtcars$hp)



#the basic packages are not enough!  
#install packages in the "Packages" window

#for example -- install the Hmisc package 

library(Hmisc)             # and load them as well...
describe(mtcars)           # package Hmisc contains the "describe" function



