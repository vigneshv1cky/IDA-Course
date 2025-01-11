#ISE 5103 R script for introducing multiple linear regression and dummy variables
#Author: Charles Nicholson
#October 2015

#we will use the "census2000.csv" file from the course website

C <- read.csv("census2000.csv")   #you will have to make sure you have the right path to the read the file

head(C)

## A good habit: build a dataframe with your relevant variables.
YX <- data.frame(log.WR = log(C$income/C$hours))
YX$age <- C$age
YX$age2 <- C$age^2
YX$sex <- C$sex

## Use relevel to make "White" and "Married" the intercept
YX$race <- relevel(C$race, "White")
YX$marital <- relevel(C$marital, "Married")

## create a bunch of education indicator variables
YX$hs <- C$edu == "3.hsgrad"
YX$assoc <- C$edu == "4.assoc"
YX$coll <- C$edu == "5.bachs"
YX$grad <- as.numeric(C$edu) > 6

## only include folks working more than 500 hours AND
## earning more than $5000 AND less than age 60
YX <- YX[(C$hours > 500)  & (C$income > 5000)  & (C$age < 60), ]


#look at some visualizations...
par(mfrow=c(1,2))
boxplot(data=YX[YX$sex=="M",], log.WR~age, col=2, main="Men", xlab="education", ylab="log Hourly Rate")
boxplot(data=YX[YX$sex=="F",], log.WR~age, col=3, main="Women", xlab="education", ylab="log Hourly Rate")

#look at mean logWR for men and women w.r.t. to age
dev.off()
men <- YX$sex == "M"
malemean <- tapply(YX$log.WR[men], YX$age[men], mean) 
femalemean <- tapply(YX$log.WR[!men], YX$age[!men], mean)
## pdf("inccurve.pdf", width=7, height=4.5)
plot(18:59, malemean, type="l", lwd=2, col=4, xlab="age",
     ylab="mean log wage rate", main="", xlim=c(19,60), ylim=c(1.8,3))
lines(18:59, femalemean, lwd=2, col=6)
text(x = rep(60,2), y = c(malemean[42],femalemean[42]),
     labels=c("M","F"), col=c(4,6))


#first model -----------------------------------
wage1<-lm(log.WR ~ age, data=YX)
summary(wage1)

#here's the plot for first model
grid <- 18:59
plot(grid, wage1$coef[1] + wage1$coef[2]*grid, type="l", lwd=2,
     main="", xlab="age", ylab="predicted log wagerate") 


#second model ----------------------------------
wage2<-lm(log.WR ~ age + sex, data=YX)
summary(wage2)

#here's the plot for second model
plot(grid, wage2$coef[1] + wage2$coef[2]*grid +
       wage2$coef[3], type="l", lwd=2, col=4,
     main="", xlab="age", ylab="predicted log wagerate", ylim=c(2,3.1)) 
lines(grid, wage2$coef[1] + wage2$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")


#third model ----------------------------------
wage3<-lm(log.WR ~ age*sex , data=YX)
summary(wage3)

#here's the plot for third model
plot(grid, wage3$coef[1] + (wage3$coef[2]+wage3$coef[4])*grid +
       wage3$coef[3], type="l", lwd=2, col=4, main="", xlab="age",
     ylab="predicted log wagerate", ylim=c(2.2,3.2)) 
lines(grid, wage3$coef[1] + wage3$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")


#fourth model ----------------------------------
wage4<-lm(log.WR ~ age*sex + age2, data=YX)
summary(wage4)


#here's the plot for fourth  model
plot(grid, wage4$coef[1] + (wage4$coef[2]+wage4$coef[5])*grid +
       wage4$coef[3] + wage4$coef[4]*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age",
     ylab="predicted log wagerate", ylim=c(2,3)) 
lines(grid, wage4$coef[1] + wage4$coef[2]*grid +
        wage4$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")



#fifth model ----------------------------------
wage5<-lm(log.WR ~ age*sex + age2*sex , data=YX)
summary(wage5)


#here's the plot for fifth model
plot(grid, wage5$coef[1] + (wage5$coef[2]+wage5$coef[5])*grid +
       wage5$coef[3] + (wage5$coef[4]+wage5$coef[6])*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age", ylab="log wagerate", ylim=c(2,3)) 
lines(grid, wage5$coef[1] + wage5$coef[2]*grid +
        wage5$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=2, legend=c("M fitted","F fitted"), bty="n")
lines(grid, malemean, col=4, lty=2)
lines(grid, femalemean, col=6, lty=2)
legend("bottomright", col=c(4,6), lwd=2, lty=2,
       legend=c("M data mean","F data mean"), bty="n")
