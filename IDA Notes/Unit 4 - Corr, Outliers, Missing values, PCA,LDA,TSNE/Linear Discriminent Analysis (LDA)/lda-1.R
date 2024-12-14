library(tidyverse) 
library(caret)
library(MASS)


#use the trusty old “iris” data for an example
data(iris)

preproc.param <- iris %>% preProcess(method = c("center", "scale")) 

# Transform the data using the estimated parameters 
transformed <- preproc.param %>% predict(iris)

# Fit the model 
lda.model <- lda(Species~., data = transformed) 

predictions <- lda.model %>% predict(transformed) 

names(predictions)

tail(predictions$x)
tail(predictions$posterior) 
tail(predictions$class)


table(Original=iris$Species,Predicted=predictions$class)
mean(predictions$class==transformed$Species)





library("Rtsne")
iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])
set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(iris_matrix) # Run TSNE

# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=iris_unique$Species)


df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], type=iris_unique$Species)
ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point()



data(Glass)

summary(Glass$Type)

Glass<-Glass[!duplicated(Glass[,-10]),]
Glass<-cbind(scale(Glass[,-10]),Glass[10])

preproc.paramGlass <- Glass %>% preProcess(method = c("center", "scale")) 

# Transform the data using the estimated parameters 
transformedGlass <- preproc.paramGlass %>% predict(Glass)


tsne_out <- Rtsne(transformedGlass[,-10],pca=FALSE,perplexity=10,theta=0.0,max_iter = 1000, num_threads=6) # Run TSNE

df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], type=Glass[,10])

ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point()









