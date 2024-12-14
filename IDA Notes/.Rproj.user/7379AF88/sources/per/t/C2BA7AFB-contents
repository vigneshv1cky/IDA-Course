#outliers example and some R functionality
#including basic bivariate outlier labeling

library(MASS)    # to get the "Animals" dataset
data(Animals)    # load the "Animals" dataset from the MASS package

head(Animals)    # look at the first few records

plot(Animals)      #plots animal's brain weight vs. body weight
identify(Animals)  #use the interactive functionality to identify 
                   #a few data points you think might be outliers
                   #- just scroll the mouse over the plot and left-click on the points
                   #- press ESC when finished

# do it again, but this time the names of the animals will display
plot(Animals)
v<-identify(Animals,labels=row.names(Animals))   # can label, and save indices in vector

Animals[v,]      # these are the records you selected with the mouse

#we could easily delete all of these values
Animals<-Animals[-v,]    

#and repeat the process...
plot(Animals)    #plots animal's brain weight vs. body weight
v<-identify(Animals,labels=row.names(Animals))  #interactivley point and click

Animals[v,] #display animals selected when finished 


#let's try something else...

data(Animals)    # re-load full data set and  examine the outliers 

library(outliers)           #load "outliers" package to 'test' for outliers

grubbs.test(Animals$brain)  #univariate test for 'brain' outliers
grubbs.test(Animals$body)   #univariate test for 'body' outliers

outlier(Animals$brain)      #what is the most extreme value for brain weight?
outlier(Animals$body)       #what is the most extreme value for body weight?

Animals[Animals$brain==outlier(Animals$brain),]   #which records identified?
Animals[Animals$body==outlier(Animals$body),]


#let's remove the body weight outlier

Animals<-Animals[Animals$body!=outlier(Animals$body),]  # return all records in Animals EXCEPT 
                                                        # for the record that has the body weight
                                                        # equal to the value of outlying body weigth


plot(Animals)
abline(lm(Animals$brain ~ Animals$body))         # add a trend line based on a linear model 
                                                 #  between brain and body weight

# the scatter plot with a simple regression line allows us to visualize 
# bivariate outliers -- that is, data points that are far from the trend line

#pick the four furthest points from the line..
v<-identify(Animals$body,Animals$brain,labels=row.names(Animals))   
Animals[v,]

Animals<-Animals[-v,]                  # and delete if you want...                        

plot(Animals)                                      # plot what ever is left, and 
abline(lm(Animals$brain ~ Animals$body))           #the new model looks like this

#now please identify the most extreme bivariate data point in the resulting plot
v<-identify(Animals$body,Animals$brain,labels=row.names(Animals)) 
Animals[v,]
