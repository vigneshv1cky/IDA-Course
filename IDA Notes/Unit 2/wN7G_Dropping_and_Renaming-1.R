# examples on how to drop variables from a data frame and rename variables

#using the mtcars data set that comes standard with the R installation
data(mtcars)
head(mtcars)  # just take a peek at the data before we start annihilating it...


# remove a variable by it's column position -----------------------  (two ways)
mtcars[2] <- NULL  # this sets the second column of mtcars to NULL; thus removing it from the data frame
head(mtcars)       # after running the above code, we see the second column "cyl" is gone 

mtcars <- mtcars[-1]   # this is another way of doing the same thing.  
                       # this time the first column is dropped.  
                       # the negative sign in the index indicates that you DON'T want column 1 in the results
                       # to make the results permanent, you have to overwrite mtcars
                       # that is, mtcars is then REDEFINED based on the output of mtcars[-1]

head(mtcars)           # "mpg" is now gone

#but don't worry!  if you want the original data back, just reload it!
data(mtcars)

head(mtcars)   #and voila! it's all back!



# remove a variable by name ----------------------  
mtcars["drat"] <- NULL    # this sets the column "drat" to NULL; thus removing it from the data frame
head(mtcars)              # after running the above code, we see the second column "cyl" is gone 


#You can remove multiple columns as well:

mtcars[3:4] <- list(NULL)   # remove the 3rd and 4th columns from the data
head(mtcars)                # and you can see they are gone

mtcars[c("disp","am")] <- list(NULL)   #you can do this by the column names as well
head(mtcars)               

#We are down to only 4 columns now...  so let's reload the data and do a few more examples
data(mtcars)


#you can remove columns using the "c" function
mtcars <- mtcars[ -c(1, 3:6, 10) ]
head(mtcars)  


#finally you can use another base R function: subset()

# subset usually returns a subset of a data frame based on a logical condition 
# (see ?subset for details)
# however, you can also use it to drop columns.  

# drop the columns named "cyl" and "qsec" using subset():
mtcars <- subset(mtcars, select = -c(cyl,qsec) )
head(mtcars)




#renaming variables in a data frame

#The easiest way to rename variables, is to first install and load the "reshape" package: 
library(reshape)

#and use the "rename" function
#usage: rename(mydata, c(oldname="newname"))

rename(mtcars, c(mpg = "MilesPerGallon"))

#to overwrite the mtcars data frame with the update:
mtcars <- rename(mtcars, c(mpg = "MilesPerGallon"))
	
mtcars[1:5,1:5]  #and there is the updated variable name.




