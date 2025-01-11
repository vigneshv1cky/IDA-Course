#Example of clustering for ISE 5103 Intelligent Data Analytics
#Charles Nicholson
#November 2015
#Revised: November 2021, November 2024


library(ggplot2)    #for graphics and the dataset for this example session
library(cluster)    #provides more cluster algorithms than base R (e.g. PAM)
library(useful)     #provides a plot function for clusters and "FitKMeans" and "PlotHartigan" functions
library(NbClust)    #provides tons of tools for identifying the "right" number of clusters
library(rgl)        #for 3D rotating plots

#check out the input parameters for k-means and various options
?kmeans

#note: the parameter "centers" is either:
#- a set of initial centroids that you want to use
#- or more likely, the value of k


#to start let's look at some really simple data and just look at 2 dimensions, Car price and mileage 
data (cars,package="caret")
#price and mileage for several cars listed for resale in 2005 Kelly Blue Book
cars<-cars[,c("Price","Mileage")]   
head(cars)
summary(cars)

#let's scale the data..
carScaled<-data.frame(scale(cars))  #default is mean centering with scaling by standard deviation


#kmeans is a function in the standard R package "stats"
#input parameters include: k, number of restarts
carsKM <- kmeans(carScaled,3, nstart=100)          

#note above: nstart=10   -- this performs the clustering 10 times with 10 different initial
#seeds; the clustering result with the minimum error is kept.

str(carsKM)

#we can take a look at the cluster results
carsKM$centers  # the centroids of the final clusers (remember, these are scaled)

carsKM$size #and the size of each cluster

cars$Cluster<-carsKM$cluster

#we can see that the cluster number is assigned to each observation
head(cars)

#first plot all the data on 2D plot without cluster information
# a ggplot of the original 2D data color-coded by the cluster number
p<-ggplot(data=cars, aes(x=Price, y=Mileage)) + geom_point()  #plot the 2 variables 
p

#do you see any clusters?  what are they? 
#let's see what k-means does for us....

# a ggplot of the original 2D data color-coded by the cluster number
p<-ggplot(data=cars, aes(x=Price, y=Mileage, color=factor(carsKM$cluster))) + geom_point(size=2)  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...

#are those the clusters you saw?  probably not... 

# look at two example cars from different clusters...
p<-ggplot(data=cars, aes(x=Price, y=Mileage, color=factor(carsKM$cluster))) + geom_point(size=2)  #plot the 2 variables and the cluster color
p <- p+  geom_point(data=cars[c(23,576),], aes(x=Price, y=Mileage), size=4,colour="black")
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...

#these are the cars we plotted -- 
# -- even though they may be in different clusters, they are right next to each other
cars[c(23,576),]

#create our own plot function to look for "Within cluster Sum of square error 'elbow' plot"
#defaults to 15 as clusters max

wssplot <- function(data, nc=15){                    

  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
     kclus <- kmeans(data, centers=k, iter.max=50, nstart=60)
     wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
     pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")

  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}


#unfortunaely, there is no obivous "elbow"
wssplot(carScaled,nc=30)


# "Hartigan's rule is one statistical technique to "find" the elbow
#FYI: Hartigan rule is: 
# ADD CLUSTER if (sum(k$withinss)/sum(kplus1$withinss)-1)*(nrow(x)-k-1) > 10

clusFit<-FitKMeans(carScaled,max.clusters=30,nstart=20,iter.max=40)   #evaluates k using the "Hartigan" rule
clusFit
PlotHartigan(clusFit)

?FitKMeans

#unfortunately, with even with this technique, it seems to want to add too many clusters for this data
# -- in the plot I see, it want to have more than 20 clusters!

#let's try it with a lot of clusters and see what we get!
carScaled<-data.frame(scale(cars))
carsKM <- kmeans(carScaled,26, nstart=10)          

#plotting  -- this looks pretty artificial to me...
p<-ggplot(data=cars, aes(x=Price, y=Mileage, color=factor(carsKM$cluster))) + geom_point(size=2)  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...


#The silhouette value is a measure of how similar an object is to 
#its own cluster (cohesion) compared to other clusters (separation). 

#The silhouette ranges from -1 to +1, where a high value indicates the object is well matched in its own cluster
#and poorly matched to neighboring clusters. 

#If most objects have a high value, then the clustering configuration is appropriate. 
#If many points have a low or negative value, then the clustering configuration may have too many or too few clusters.

#Let's use PAM and evaluate with the silohouette method

#i'm just using a 100 cars to do this since
#it is a bit computationally/visually burdensome otherwise

carSample <- carScaled[sample(nrow(carScaled),100),c("Price", "Mileage")]

pr4 <- pam(carSample, 5)   

#structure of a PAM object
str(pr4)

#create a silhouette object from the PAM results
si <- silhouette(pr4)
plot(si) # silhouette plot
summary(si) #and numerical summary

#depending on your results, you might see that k=5 is too many or too few.

#now check out NbClust...
?NbClust

# the NbClust package contains several techniques for evaluating the 'right' value for k
# the package identifies the value of k indicated by each statistical procedure
# some of the techniques agree, others don't

 
#since it takes a long time to run, demo on a small sample
(results<-NbClust(carSample,method="kmeans"))

#index = "alllong"

p<-qplot(data=carSample, x=Price, y=Mileage, color=factor(results$Best.partition))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    

#demo: let's 'apply' a cluster solution to new data

#here is some "new" data
newCarData <- data.frame(Price=c(1.5,-3.2,0.4), 
                         Mileage=c(1.8,0.2,2.9))
  
carsKM2 <- kmeans(carSample,4, nstart=10)          


#we can do this by writing our own little function (borrowed from the web)
#this function simply assigns the points to the closest centroid from the
#k-means results

# NOTE to the progammatically inclined:
# this function is really poorly written in that it requires a "global" name for the cluster object (carsKM2)
# Sorry.


closest.cluster <- function(x) {
  cluster.dist <- apply(carsKM2$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
(clusters2 <- apply(newCarData, 1, closest.cluster))

#or you can use existing k-NN packages to do the same thing, e.g.,
library(FNN)

(pred.knn <- get.knnx(carsKM2$center, newCarData, 1)$nn.index[,1])



############### irisdata ###################

#let's get a little bit more complex, and see how we do with "ground truth" available data

# we know that there really are 3 different species of in the Iris data
# let's see how many clusters are identified using these techniques

set.seed(42)
data(iris)
iris$Species <- factor(iris$Species,
                       levels = c("versicolor","virginica","setosa"))

iris[,1:4]<-scale(iris[,1:4])  # first scale the data
head(iris)

wssplot(iris[,1:4],nc=5)  #again, maybe an elbow at k=3...

#let's try NbClust...
NbClust(iris[,1:4],method="kmeans")

#we will use 3 clusters for k-means
cl <- kmeans(iris[,1:4],3)
iris$cluster <- as.factor(cl$cluster)
head(iris)



#to visualize this 4d data in 3d let's use PCA, and then color code base on clusters
pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
summary(pc)

plot3d(pc$scores[,1:3], size=5, col=iris$cluster, main="k-means clusters")

#and a quick check to see how our 'natural clusters' align with the species data
table(iris$Species, iris$cluster)



dev.off()

#let's try hierarchical clustering for the iris data
#hierarchical clustering requires a distance matrix
di <- dist(iris[,1:4], method="euclidean")   # with hiearchical clustering, only need distance matrix

hc <- hclust(di, method="ward.D2")
plot(hc, labels=FALSE)

rect.hclust(hc, k=3, border="red")     #if we were to "cut" at k=3, what are the groups?
iris$hcluster <- as.factor(cutree(hc, k=3))   #cutting at k=3, here are the assignments

head(iris)

#and a quick check to see how our 'natural clusters' align with the species data
table( iris$Species, iris$hcluster)


dev.off()

############## mtcars data ##################


#Some easier data to play around with for hiearchial clustering
#use small data set for example...
#mtcars: 1974 Motor Trend US magazine data
# fuel consumption, 10 aspects of design and performance 

data(mtcars)   
head(mtcars)


#daisy provides different distance functions
#different distance functions will produce different results

d<-daisy(mtcars,metric="manhattan",stand=T)

hclus<-hclust(d,method="single")   #notice the long chains (e.g., very unbalanced)
plot(hclus)

d<-daisy(mtcars,metric="euclidean",stand=T)
dev.off()


#NOTE: the 'daisy" function in package "cluster" can handle mixed data for distances using the Gower's Distance method
str(mtcars)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)

d<-daisy(mtcars,metric="gower",stand=T)


#look at the different shapes of dendrograms based on the linkage techniques

hclus<-hclust(d,method="single")   #notice the long chains (e.g., very unbalanced)
plot(hclus)

hclus<-hclust(d,method="complete")
plot(hclus)

hclus<-hclust(d,method="average")
plot(hclus)

hclus<-hclust(d,method="ward.D2")  # notice how balanced the clusters are
plot(hclus)




#let's go back and look at the first example again

dev.off()


carScaled<-data.frame(scale(cars))


#k-means
carsKM <- kmeans(carScaled,4, nstart=10)          
p<-ggplot(data=cars, aes(x=Price, y=Mileage, color=factor(carsKM$cluster))) + geom_point(size=2)  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...


#hierarhical clustering
d<-daisy(carScaled)

hclus<-hclust(d,method="complete")   
plot(hclus)

rect.hclust(hclus, k=4, border="red")     #if we were to "cut" at k=4, what are the groups?
carScaled$hcluster <- as.factor(cutree(hclus, k=4))   #cutting at k=4, here are the assignments

p<-ggplot(data=carScaled, aes(x=Price, y=Mileage, color=factor(hcluster))) + geom_point(size=2)  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...





#for fun -- take a look at movie data from imdb.com

#################### movies data ##################
#we will use "movie rating data" for the example from the "ggplot2movies" library
library(ggplot2movies)

?movies

#title. Title of the movie.
#year. Year of release.
#budget. Total budget (if known) in US dollars
#length. Length in minutes.
#rating. Average IMDB user rating.
#votes. Number of IMDB users who rated this movie.
#r1-10. Multiplying by ten gives percentile (to nearest 10%) of users who rated this movie a 1.
#mpaa. MPAA rating.
#action, animation, comedy, drama, documentary, romance, short. Binary variables representing if movie was classified as belonging to that genre.

data(movies)





#but let's clean it up a bit first...
movies2<-movies[complete.cases(movies),c(1:6,17)]    #keep only complete cases; keep some variables
movies2<-movies2[movies2$votes>2500,]                #exclude movies with only a few ratings

mDD<-duplicated(movies2[,c("title")])
movies2<-movies2[!mDD,]

dat<-round(scale(movies2[,-c(1,7)]),3)               #scale the numeric data (and round the results)
row.names(dat)<-movies2$title                        #keep the movie titles as row.names
dat<-as.data.frame(dat)

head(dat)

#we will start by looking at partitions
wssplot(dat,nc=30)   #-- again, no clear elbow!  


set.seed(100)      #just so that we will all get the same results!

kclus<-kmeans(dat, 6, nstart=5)    #how about trying k=6

#this plot function comes from the "useful" libary uses PCA 
plot(kclus,data=dat) 


clusInfo<-data.frame(kclus$centers,kclus$size)
clusInfo

#by evaluating the centroids of each variable, we can discuss the 'meaning' of the cluster

#         year     length      budget     rating       votes kclus.size
#1 -2.22851913 -0.2312077 -0.91936612  0.9339563 -0.20458470        183
#2  0.20304230 -0.1427341 -0.45749396  0.3648474 -0.14853776        662
#3  0.01190123  0.9218765  0.27377778  1.3547160  3.47887654         81
#4  0.56625879  0.3389201  1.56101917 -0.1473259  0.17708626        313
#5  0.38614789 -0.5685563 -0.04270892 -1.2621502 -0.48717136        426
#6 -1.06658462  3.0500462 -0.33092308  0.9478769  0.09375385         65

# in this example, 
#cluster #1: the oldest movies, with smaller budgets, and above average ratings
#cluster #4: big budget newer movies with lower ratings

#(your results might be different!)


movieClus <- data.frame(movies2, clust=kclus$cluster, dat)
head(movieClus[movieClus$clust==2,c("title","rating","budget","year","mpaa")])



#helper function to evaluate "representative data points within clusters"
#and possibly outliers
#-- calculate the distance between the given cluster centers
#-- and the data, and return the distance between the data and the closest cluster center


closest.cluster <- function(x) 
{
  cluster.dist <- apply(kclus$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(cluster.dist[which.min(cluster.dist)[1]])
}


clusters2 <- apply(df2, 1, closest.cluster)

movieClus$dist <- apply(dat, 1, closest.cluster)   #apply the "distance to nearest cluster function"
movieClus <-movieClus [order(movieClus$dist),]         #sort the data by this new distance

#now we can look at top 10 most representative titles in the cluster

head(movieClus[movieClus$clust==1,c("title","rating","budget","year","dist")],10) 
head(movieClus[movieClus$clust==2,c("title","rating","budget","year","dist")],10)  
head(movieClus[movieClus$clust==3,c("title","rating","budget","year","dist")],10) 
head(movieClus[movieClus$clust==4,c("title","rating","budget","year","dist")],10) 
head(movieClus[movieClus$clust==5,c("title","rating","budget","year","dist")],10) 
head(movieClus[movieClus$clust==6,c("title","rating","budget","year","dist")],10) 




#just for fun -- hierarchical clustering with movie data
d<-daisy(dat)

hclus<-hclust(d,method="complete")   
plot(hclus, labels=FALSE)

rect.hclust(hclus, k=6, border="red")     #if we were to "cut" at k=3, what are the groups?
pcaMovies<-prcomp(dat,scale=T)

dat$hcluster <- as.factor(cutree(hclus, k=6))   #cutting at k=3, here are the assignments


p<-qplot(data=data.frame(pcaMovies$x), x=PC1, y=PC2, color=factor(dat$hcluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...






#another thing: variable clustering

str(dat)

library(Hmisc)
v<-varclus(as.matrix(dat[,1:5]),similarity="pearson")    
plot(v)

corMat<-cor(dat[,1:5],method="pearson")
heatmap(corMat)


#similarity=c("spearman","pearson","hoeffding","bothpos","ccbothpos"),
data(mtcars)
v<-varclus(as.matrix(mtcars),similarity="hoeffding")    
plot(v)

corMat<-cor(mtcars,method="spearman")
heatmap(corMat)



#density based clustering
library(dbscan)

?dbscan
data(iris)
iris2 <- as.matrix(iris[,1:4])

kNNdist(iris2, k=4, search="kd")
kNNdistplot(iris2, k=4)
## the knee is around a distance of .5

abline(h=.5, col="red")

db <- dbscan(iris2, eps = .5, minPts = 4)
pairs(iris2, col = db$cluster+1L)

table(iris$Species, db$cluster)


db <- dbscan(iris2, eps = .4, minPts = 4)
pairs(iris2, col = db$cluster+1L)

table(iris$Species, db$cluster)



#look at the movie data again
x=dat[,1:5]
str(x)
?kNNdist
kNNdistplot(x, k=9)
abline(h=1.05, col="red")

set.seed(1234)
db = dbscan(x, .95,5)
db
hullplot(x, db$cluster)

pairs(x, col = db$cluster + 1L)

table(db$cluster)

opt <- optics(x, eps = .51, minPts = 10)
opt
opt <- extractDBSCAN(opt, eps_cl = .51)
plot(opt)

