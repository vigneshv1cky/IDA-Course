library(datasets)
library(scatterplot3d)
library(rgl)
library(ggplot2)


#to install ggbiplot, first install and load "devtools"
#then download ggbiplot from CRAN then load
----------------------------------------------------------
#install.packages("devtools")
#library(devtools)
#install_github("ggbiplot", "vqv")

library(ggbiplot)



x<-80*runif(1000)-40    #uniform random between -40 and 40
y<-40*runif(1000)-20    #uniform random between -20 and 40
z<-6*runif(1000)-3      #uniform random between -3 and 3



df<-data.frame(x,y,z)   #create df
df<-as.matrix(df)       #convert to matrix

scatterplot3d(df,xlim=c(-40,40),ylim=c(-40,40),zlim=c(-40,40))


#notice the axes views --
plot3d(df, col="red", size=4, xlim=c(-40,40),ylim=c(-40,40),zlim=c(-40,40))

#what do you expect the eigenvectors to be?
pc<-prcomp(df,center=T)
pc



summary(pc)
plot(pc)

str(pc)    #look at component pieces of the prcomp obj

?prcomp    #notice defaults regarding centering and scaling

pc$rotation

head(pc$x)


df2<-scale(df,scale=F)
mat<-df2%*%pc$rotation

mat[1:5,]


#for convenience:
newX<-pc$x[,1]
newY<-pc$x[,2]
newZ<-pc$x[,3]

scatterplot3d(newX,newZ,newY,xlim=c(-40,40),ylim=c(-40,40),zlim=c(-40,40),xlab="PC1",ylab="PC3",zlab="PC2")

#now the 2D plots

plot(newX,newY,xlim=c(-40,40),ylim=c(-40,40))          #plot x vs y
plot(newX,newZ,xlim=c(-40,40),ylim=c(-40,40))          #plot x vs z
plot(newY,newZ,xlim=c(-40,40),ylim=c(-40,40))          #plot y vs z


#now the biplots

biplot(pc)
ggbiplot(pc,circle=T,obs.scale=1,varname.size=20)
ggbiplot(pc,circle=T,choices=c(1,3),obs.scale=1,varname.size=20)





#re-run the above with the following inputs
x<-1*rnorm(1000)
y<-5*rnorm(1000)
z<-y+3*rnorm(1000)




#now for the example from lecture using the "state.x77" data

#let's see what we have to start with...

?state.x77

plot(state.x77[,8],state.x77[,2])


#PC with and without scaling...

prcomp(state.x77,scale=F)
prcomp(state.x77,scale=T)


#let's look at the results in more detail...


state.pca<-prcomp(state.x77,scale=T)

summary(state.pca)
plot(state.pca)

ggbiplot(state.pca)

ggbiplot(state.pca,obs.scale = 1, var.scale = 1, 
         varname.size = 4, labels.size=10, circle = TRUE)

#PC1 distinguishes between cold states with educated, harmless,
#long-lived populations, and warm, ill-educated, short-lived, violent states. \\

#PC2 distinguishes big rich educated
#states from small poor ignorant states, which tend to be a bit warmer,
#and less murderous.





