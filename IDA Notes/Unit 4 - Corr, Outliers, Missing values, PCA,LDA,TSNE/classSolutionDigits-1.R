#R script related to the digit image recognition problem using PCA
#the image data will be 16x16 B&W images of digits

library(jpeg)     #to read and write JPG image files
library(ggplot2)  #to make pretty plots


#a function to use ggplot2 to create scree plots
ggscreeplot <- function(pcobj, k=10, type = c('pev', 'cev')) 
{
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))

  yvar.lab <- switch(type,
                     pev = 'proportion of explained variance',
                     cev = 'cumulative proportion of explained variance')

  df <- data.frame(PC = 1:length(d), yvar = yvar)

  ggplot(data = df[1:k,], aes(x = PC, y = yvar)) + 
    xlab('principal component number') + ylab(yvar.lab) +
    geom_bar(stat="identity",alpha=0.4,fill="blue",color="grey") + geom_line()
}

#read in raw images

setwd("C:/Users/nich8038/Documents/1 - Teaching/Courses/Graduate Courses/ISE-DSA 5103/Lectures/Slides and R code/Unit 04 Data Understanding/PCA for Images")
classDigits <- read.csv("digits/classDigits.csv")

str(classDigits)



#make a jpg from a raw image
imageNum=15
v<-as.numeric(classDigits[imageNum,2:785]/255)
imageOriginal<- matrix(v,28,28,byrow=TRUE)
writeJPEG(imageOriginal,target=paste0("digit",classDigits[imageNum,1],"-",imageNum,".jpg"))



head(classDigits)
#compute the PCA on the raw image data
p<-prcomp(classDigits[,2:785],center=TRUE)
summary(p)

#look at screeplots
ggscreeplot(p,type="pev",k=100)
ggscreeplot(p,type="cev",k=100)

#it looks like k <= 60 will be plenty, maybe fewer

#make the jpeg of the "mean digit"
v<-as.numeric(p$center)
meandigit<- matrix(v/255,28,28,byrow=TRUE)
writeJPEG(meandigit,target="meandigit.jpg")

#create a mean centered version of the original images
meanCentered <- sweep(classDigits[,2:785], 2, p$center, check.margin = FALSE)


#i dont remember if I asked them to do this or not, but here are the eigendigit images
imageList<-list()  #to hold all eigendigits
#look at firt K eigenFaces
for (k in 1:5)
{
  v<-as.numeric(p$rotation[,k])  
  
  #need to do some scaling since the PCAs can be negative, byt jpeg values cannot be
  v <- v-min(v)
  v<-v/max((abs(v)))
  
  PCdigit<- matrix(v,28,28,byrow=TRUE)

  writeJPEG(PCdigit,quality=1,target=paste0("PC",k,"digit.jpg"))
    
  #imageList[[k]]<-PCdigit  
}




#reconstruct a digit from only a few eigenvectors
#look at firt K eigenFaces
imageNum<-15
for (g in c(5,20,100))
{
  reconstruction<-p$center
  for (k in 1:g)
  {
    weight<-as.matrix(meanCentered[imageNum,])%*%p$rotation[,k]
    reconstruction<-reconstruction+weight*p$rotation[,k]
  }
  reconstruction<- matrix(reconstruction,28,28,byrow=TRUE)
  writeJPEG(reconstruction/255,quality=1,target=paste0("reconstructedD",imageNum,"-",g,".jpg"))
}



#apply to new data
#score new faces

class7test <- read.csv("digits/class7test.csv")


testImage<-5
xNew <- class7test[testImage,3:786]

#here's what it is supposed to be:
class7test[testImage,2]

#mean centered test image
meanCenteredX <- xNew -  p$center

#choose value for k
k<-33

#here is what the image actually looks like
v<-as.numeric(class7test[testImage,3:786])
bb<- matrix(v,28,28,byrow=TRUE)/255
writeJPEG(bb,target=paste0("image",testImage,".jpg"))

#here are all the weights of all the 30000 images
v1<-as.matrix(meanCentered)%*%p$rotation[,1:k] 

str(v1)

#and the covariance matrix
S <-  cov(v1)

#here are the weights of the test image
xWeights<-as.matrix(meanCenteredX)%*%p$rotation[,1:k]


#now for the distances
D2<-mahalanobis(v1,xWeights,S)

#mean distance
mean(D2)

#best image
bestImage=which(D2 == min(D2), arr.ind = TRUE)
c(bestImage=bestImage,distanceToBest=D2[bestImage],prediction=classDigits[bestImage,1],trueVal=class7test[testImage,2])




