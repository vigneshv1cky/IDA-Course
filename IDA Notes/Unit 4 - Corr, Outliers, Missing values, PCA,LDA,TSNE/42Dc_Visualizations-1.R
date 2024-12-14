
data(iris)     # make sure data is loaded   (the iris data is part of the standard R distribution)
head(iris)     # look at the first few records
  
?iris          # access 'help' on the iris data  

#perform a frequency count for the Species
table(iris$Species)


#box plots -------------------------------------------------

boxplot(data=iris, Sepal.Length ~ Species,           # boxplot of Sepal.Length by Species 
        main = "Iris Sepal Length by Species ",      # main plot title
        xlab = "Species",                            # x-axis label   
        ylab = "Sepal Length (cm)")                  # y-axis label   


# if you want to save the plot as an image, you can 
# either use the "export" functionality in the Plots tab window in RStudio, 
# or you can use do this programmatically...  e.g. to save as a pdf use: pdf("filename.pdf")
# and then run the plot.  This redirects all graphics output to the pdf file.
# You can set the size (in inches) for the pdf output.
# To redirect back to the screen, turn off the "pdf device" using: dev.off()
# See example:


pdf("irisBoxplot.pdf",width=8, height=6)    #this will re-direct your graphic output to a pdf file

boxplot(data=iris, Sepal.Length ~ Species,           # boxplot of Sepal.Length by Species 
        main = "Iris Sepal Length by Species ",      # main plot title
        xlab = "Species",                            # x-axis label   
        ylab = "Sepal Length (cm)")                  # y-axis label   

dev.off()




#histograms and densities --------------------------------------


#some very simple to code, quick and dirty histograms
# perfect for quickly exploring the data

par(mfrow=c(2,2))  #OPTIONAL: change the graphical parameters so the histograms are produced 4 to a page
                   #see ?par for more details on setting graphical parameters

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)


#NOTE:  we could have made these look better, e.g. with better titles and labels


hist(iris$Sepal.Length, main = "Sepal Length", xlab = "Sepal Length")
hist(iris$Sepal.Width,  main = "Sepal Width", xlab = "Sepal Width")
hist(iris$Petal.Length, main = "Petal Length", xlab = "Petal Length")
hist(iris$Petal.Width,  main = "Petal Width", xlab = "Petal Width")


# but, usually when I am exploring the data, I will use the simple verison


par(mfrow=c(1,1))  #RESET graphical parameters to 1 plot per page


#for a final report or publication, I would use better graphics: ggplot2
#here is a basic example...

# ggplot2 is the "grammar of graphics" plot library and produces excellent graphics
library(ggplot2)

#qplot is one of the main functions in ggplot2 -- it is short for "quick plot"
#qplot allows you to do histograms, scatterplots, boxplots, line plots, etc.

qplot(data=iris, Petal.Length)


#or you can set several options to modify the output

qplot(data=iris, Petal.Length,                       #identify data & variable
                 geom="histogram",                   #set the "geometry"
                 binwidth=0.2,                       #option for histogram
                 main= "Histogram for Petal Length", #title
                 xlab = "Petal Length",              #x-axis label
                 fill=I("blue"),                     #fill color
                 alpha=I(0.45))                      #set fill transparency

#ggplot is the primary function in ggplot2
#it allows for much more control over the graphics than qplot does

#for the next chart, I want to produce a density

library(reshape2)  #this package allows us to reform the data from a "wide" format to a "long" format
iris2<- melt(iris)

#identify data and set the aesthetics         
ggplot(iris2[iris2$variable=="Petal.Length",], aes(x=value, fill=Species)) +
  geom_density(alpha=0.45) +        #set geometry and transparency    
  labs(x = "Petal Length",          #set x-label and title
       title = "Densities for Petal Length of Iris Species")


#we can also use gplot to produce more advanced boxplots
ggplot(iris2,aes(x=variable, y=value, fill=Species)) + geom_boxplot()



#scatter plots --------------------------------------

# create scatter plots for the numerical data in the iris data set
plot(iris)


# qplot and ggplot allow you to add many options and control many settings
# in graphs -- this can look quite confusing at first
# however, most of the parameter settings are optional and have defaults if not set
# the following bit of code might seem a bit overwhelming at first,
# but most of the complexity is related to setting up colors, sizes, styles, and labels


# using qplot a.k.a "quickplot" to produce scatter plot
qplot(data=iris, x=Sepal.Length,y=Sepal.Width,size=I(5)) +   # point size=5 
  theme_bw() +                                               # using black and white background theme
  labs(y = "Sepal Width (cm)",                               # x-axis labels    
       x = "Sepal Length (cm)")                              # y-axis labels


# using ggplot a.k.a "grammar of graphics plot" to produce scatter plot
ggplot(data=iris, aes(x=Sepal.Length,y=Sepal.Width)) +        # set data and aesthetics
  geom_point(aes(fill=Species),                               # add points (fill color based on "Species")
             colour="black",                                  # -- outline set to black
             pch=21,                                          # -- shape = 21, a filled circle
             size=5) +                                        # -- size = 5
  theme_bw() +                                                # using black and white background theme
  labs(y = "Sepal Width (cm)",                                # x-axis labels 
       x = "Sepal Length (cm)") +                             # x-axis labels    
  theme(legend.position = "none")                             # turn legend off



ggplot(data=iris, aes(x=Sepal.Length,y=Sepal.Width)) +  
  geom_point(aes(fill=Species), colour="black",pch=21, size=5) +
  theme_bw() +
  labs(y = "Sepal Width (cm)",
       x = "Sepal Length (cm)") +
  theme(legend.key=element_blank())                         # legend is on, but the outline is off


ggplot(data=iris, aes(x=Petal.Length,y=Petal.Width)) +  
  geom_point(aes(fill=Species),   
             alpha=I(.85),                               # alpha (i.e. opacity) is set to 0.85
             colour="black",pch=21, size=5) +
  theme_bw() +
  labs(y = "Petal Width (cm)",
       x = "Petal Length (cm)") +
  theme(legend.key=element_blank(),
        axis.title = element_text(size = 14))            # set axis title font size to 14


ggplot(data=iris, aes(x=Petal.Length,y=Petal.Width)) + 
  geom_point(aes(fill=Species), 
             alpha=I(.75),                               # alpha (i.e. opacity) is set to 0.75
             position = "jitter",                        # "jitter" the position of the points
             colour="black",pch=21, size=5) +
  theme_bw() +
  labs(y = "Petal Width (cm)",
       x = "Petal Length (cm)") +
  theme(legend.key=element_blank(),
        axis.title = element_text(size = 14))


# a "pairs" plot that incorporates densities, scatterplots, and correlations


library(GGally)   #adds some more functionality to ggplot2 -- including pairs and parallel plots


ggpairs(iris[, 1:5], lower=list(continuous="smooth", params=c(colour="blue")),
  diag=list(params=c(colour="blue")), 
  upper=list(params=list(corSize=6)), axisLabels='show')



#parallel plots --------------------------------------

library(lattice)                                            #load the "lattice" library for parallel plots
parallelplot(~iris[1:5], data=iris,                         # create parallel plot of iris data;
               groups = Species,                            # use "Species" to define groups (and colors)
horizontal.axis = FALSE)                                    # defaults to horizontal axis, set to vertical                           


#parallelplot help documentation -- the input is unfortunately a little different with the ~ symbol
?parallelplot


parallelplot(~iris[1:4] | Species, data = iris,             #same as above, except condition the plot by Species
              groups = Species,   
              horizontal.axis = FALSE, 
              scales = list(x = list(rot = 90)))            #and rotate the labels on the x-axis


# you can kind of go crazy with some of this stuff too...
# parallel plots + boxplots = maybe too messy to be useful?  let's see

# underlay univariate boxplots, add title, using a function from GGally
ggparcoord(data = iris,columns = c(1:4),groupColumn = 5,
  boxplot = TRUE,title = "Parallel Coord. Plot of Diamonds Data")



#my embarassingly bad radar plot in R...
install.packages("fmsb")
library(fmsb)
radarchart(iris[,1:4], maxmin=FALSE, centerzero=TRUE)


#my pitiful looking stars plot....
stars(iris[,1:4], radius=TRUE, key.loc = c(30,15), ncol=10, nrow= 15, col.stars = iris$Species)


