library(mlbench)
library(tidyverse)
library("Rtsne")

iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])

set.seed(42) # Set a seed if you want reproducible results

tsne_out <- Rtsne(iris_matrix) # Run TSNE

?Rtsne

# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=iris_unique$Species)


df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], type=iris_unique$Species)
ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point()



data(Glass)
str(Glass)

Glass<-Glass[!duplicated(Glass),]

summary(Glass$Type)


tsne_out <- Rtsne(Glass[,-10],
                  perplexity=8,
                  theta=0.0, 
                  max_iter = 3000) # Run TSNE

df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], type=Glass[,10])

ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point()






