# Example code to demonstrate skewed distribution transformations
# ISE 5103 Intelligent Data Analytics
# Charles Nicholson
# September 2015


library("car")        #<-- used to get Prestige dataset; and 'symbox' function
library("EnvStats")   #<-- used to get "boxcox" function

data(Prestige)   #Prestige of Canadian Occupations based on Pineo-Porter prestige score
?Prestige

head(Prestige)

hist(Prestige$income)        # distribution before transformation
boxplot(Prestige$income)     # boxplot before transformation


# symbox from the car package
# produces parallel boxplots for different values of lambda
symbox(Prestige$income, data=Prestige, powers=c(3,2,1,0,-0.5,-1,-2))

hist(log(Prestige$income))  # histogram of the log transformed Prestige$income variable


# can do the same thing on a more heavily skewed data sets
x1<-rlnorm(1000)                          #create lognormal distributed random data set                          
symbox(x1, powers=c(3,2,1,0,-0.5,-1,-2))  #and look at symbox output

hist(x1)
hist(log(x1))


x<-rexp(10000,rate=0.5)   # some more fake data... this time exponential

symbox(x, powers=c(3,2,1,0,-0.5,-1,-2))      #and look at symbox output
hist(x)
hist(log(x))


boxcox(x)     # produces a PPCC (probability plot correlation coefficient) value 
              # for several values of lambda -- the highest PPCC value is the better lambda

par(mfrow=c(2,2))  # setup the output area to plot the histograms in 2 rows of 3 columns
                   # and then look at several options of transformations

hist(x)
hist(log(x))
hist((x**0.5-1)/0.5)  


# can use boxcox to search for the optimal lambda value
boxcox(x, optimize = TRUE, lambda=c(-3,3))    # if optimize = TRUE, then you must tell the function the 
                                              # the search range, e.g. search on the interval (-3,3)

hist((x**0.2652646-1)/0.2652646)  





dev.off()   # turn off output device and reset the graphical parameters

# optional piece to show how different power transforms affect the data ------------

# create vector of x-coordinates
x <- seq(from=0,to=4,by=.01)

# Use the command 'plot' to plot the first line. 
#   type = what type of plot should be drawn ("l" stands for lines)
#   lty = line type, lwd = line width, ylim = range on y-axis
plot(x, (x^(-1)-1)/(-1), col=1, lty=1, lwd=2, type="l", ylim=c(-4,6), 
     main="Family of powers and roots",xlab="x", ylab="x^{(p)}")

# Use the command 'lines' to add the other lines to the plot
lines(x, log(x), col=2, lty=2, lwd=2)
lines(x, (x-1)/1, col=3, lty=3, lwd=2)
lines(x, (x^2-1)/2, col=4, lty=4, lwd=2)
lines(x, (x^3-1)/3, col=5, lty=5, lwd=2)

# Add a legend:
legend(0,6,c("p=-1", "p=0 (log)", "p=1", "p=2", "p=3"), col=c(1,2,3,4,5), 
       lty=c(1,2,3,4,5), lwd=2)


# ------------------------------------------------------------------------------------
