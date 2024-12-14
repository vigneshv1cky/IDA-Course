#Example of decision trees and tree visualization for ISE 5103 Intelligent Data Analytics
#Charles Nicholson
#November 2015


library(rpart)          # for decision tree modeling
library(party)          # for visualizing trees
library(partykit)       # for visualizing trees
library(ggplot2)        # for graphics
library(ROCR)           # for graphics
library(rattle)      		# fancy tree plot
library(rpart.plot)			# enhanced tree plots
library(RColorBrewer)		# color selection for fancy tree plot

# rpart is one of the more common decision tree packages (and functions) for R
# take a look at the documentation for rpart:
# notice in particular the formula interface, and the "parms" parameter,
# and the "control" parameter (check out the default settings of "rpart.control")
?rpart

#to demonstrate, we will work with some simple data that we have seen before
data(iris)
head(iris)

#the goal is to classify iris species based on the four measurements

fitDT<-rpart(Species~.,data=iris)  #defaults to gini, 10-fold CV

fitDT<-rpart(Species~.,data=iris,                   
            parms=list(split="information"),   #can change to information gain
            control=rpart.control(xval=20)  )  #can change k-fold CV 

# the induced tree 
print(fitDT)    # unfortunately, the default "print" command is ugly

#the tree with a lot more details including surrogates, etc.
summary(fitDT)

# the cost-parameter results associated with different tree complexities
printcp(fitDT)


#    CP nsplit rel error xerror     xstd
#1 0.50      0      1.00   1.25 0.045644
#2 0.44      1      0.50   0.67 0.060888
#3 0.01      2      0.06   0.11 0.031927

# Essentially this tells you that if you set the CP value to 0.5,
# there will be no splits; but if you set it to 0.44, there will be one
# split, and so on

# For each CP level (and tree complexity), the CV error is produced

# WE can plot these results...
plotcp(fitDT)

# it is easy to extract out the cost-parameter associated with lowest CV error
fitDT$cptable[which.min(fitDT$cptable[,"xerror"]),"CP"]

# visualize tree (ugly!)
plot(fitDT)   
text(fitDT)

# better...
prp(fitDT,type=2,extra=4)

# and even better...
prp(fitDT, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE,
    faclen=0,varlen=0, shadow.col="grey", branch.lty=3)


# some packages include functions to make them even better, 
# e.g. from Rattle
?fancyRpartPlot
fancyRpartPlot(fitDT)

# or the party and partykit packages  (my favorite)
fitDTparty<-as.party(fitDT)
plot(fitDTparty)



#now, let's look at another data source
set.seed(999)   #just to make sure we all get the same results!


data(weather) #weather observations from a number of locations around Australia,
?weather

ds<-weather[,-c(1,2,23)]   #take out a few variables that we should exclude...

# there are a lot of predictors -- our goal is to predict "RainTomorrow"
head(ds)

#overwrite the default CP value to create a potentially overally complex tree
fitDT<-rpart(RainTomorrow~.,data=ds,control=rpart.control(cp=0.001),xval=20)   

fancyRpartPlot(fitDT)
printcp(fitDT)
plotcp(fitDT)

# based on the plot:  the min CV error is with cp=0.037 
# (which produces a tree with 3 leaf nodes)

pfit<-prune(fitDT,cp=0.037)  #and we can prune to this level
fancyRpartPlot(pfit)


# or if we want to use the 1 standard error rule 
# (choose the least complex model whose error is within 1 standard error
# of the most accurate model)
pfit<-prune(fitDT,cp=0.13)   #using the 1-se rule, we could reduce to cp=0.13
fancyRpartPlot(pfit)



# but for now let's use the complex one
fitDT<-rpart(RainTomorrow~.,data=ds)  

# use the "predict" function to apply the decision tree to 
# either newdata or as in this case, the training data
# to get the class probabilities associated with each observation
ds$pred<-predict(fitDT, newdata=ds, type = "prob")[,2]     #note the prediction returns a matrix...


# once we have the probabilities, we can analyze as usual!
# e.g., an ROC curve...
pred <- prediction(ds$pred, ds$RainTomorrow)    #ROC curve for train
perf <- performance(pred,"tpr","fpr") 
plot(perf,colorize=TRUE); 
abline(0, 1, col="red")  



#another example...this type "fake" data

x1<-runif(1000)
x2<-runif(1000)

y<-as.factor(x1+x2>0.65)  #y is the target and it is perfectly predictable!
df<-data.frame(y,x1,x2)

# visualize the data
qplot(data=df, x1, x2, color=y)  + scale_color_manual(values=c("red", "blue"))

# and for fun, let's run a logistic regression... which will fail!
fitLR<-glm(data=df, y ~ x1 + x2, family="binomial")

#now for a decision tree
fitDT<-rpart(data=df, y ~ x1 + x2)


# assess the classifier
df$pred<-predict(fitDT, newdata=df, type = "prob")[,2]     

pred <- prediction(df$pred, df$y)    #ROC curve for train
perf <- performance(pred,"tpr","fpr") 
plot(perf,colorize=TRUE); 
abline(0, 1, col="red")  

table(df$pred, df$y)

#yep, we have great accuracy!


# but Wow! the tree seems to be a bit complex!
fancyRpartPlot(fitDT)



#Point #1:  we still need feature engineering
x3<-x1+x2

df<-data.frame(y,x1,x2,x3)
fitDT<-rpart(data=df, y ~ x1 + x2 + x3, control=rpart.control(minsplit=1,cp=0))
fancyRpartPlot(fitDT)  #much better!





library(caret)   #want to access a dataset from this library and possibly some functions

data(GermanCredit)
?GermanCredit
# These data have two classes for the credit worthiness: good or bad.
# There are predictors related to attributes, such as: 
# checking account status, duration, credit history, 
# purpose of the loan, amount of the loan,
# savings accounts or bonds, employment duration, etc.

# We will try to build a tree to classify "good" or "bad" credit worthiness


#for this example, we will just look at a few predictors
variables<-c(1:2,4:6,10,19,40,42,57,62)
GC<-GermanCredit[,variables]          #select out just what we want to work with (for convenience)
GC$Class<-as.factor(GC$Class=="Bad")  


# Make big tree
tree.1 <- rpart(data=GC,Class~.,control=rpart.control(minsplit=5,cp=0))

# 
plot(tree.1)  				# Will make a mess of the plot
text(tree.1)
# 
prp(tree.1,varlen=3) 	# will plot the tree  (with shortened variable names)
                      #(this takes a minute or so...) 


# Interactively prune the tree

# once you run the next command, you will click on the tree
# with your mouse at different inner nodes to "prune" the true manually
# when done, click on the "QUIT" on the graph
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1,varlen=3) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(data=GC,Class~.)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle

tree.2.party<-as.party(tree.2)  #another visualization of the same thing
plot(tree.2.party)
#
#-------------------------------------------------------------------


