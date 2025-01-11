library(tidyverse)

#load data
data(mpg)
?mpg

#glimpse data
glimpse(mpg)

#see which variables are numeric, factor, character
mpg %>% select_if(is.numeric) %>% names()
mpg %>% select_if(is.factor) %>% names()
mpg %>% select_if(is.character) %>% names()

#convert all character variables to factors
mpg <- mpg %>% mutate_if(is.character, as.factor)

#look at levels of each factor
mpg %>% select_if(is.factor) %>% sapply(levels) 
mpg %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)


#build a simple model to predict city mpg
mdl.LM1 <- lm(data=mpg, cty ~ class)
summary(mdl.LM1)

mpg$mdl1pred <- predict(mdl.LM1, mpg)  #apply model to data to get predictions

#look at some results stats
mpg %>% group_by(class) %>% 
  summarize(avgCityMPG=mean(cty),
            predMin=min(mdl1pred), predMean=mean(mdl1pred), predMax=max(mdl1pred))

#graph the observations and predicted value for each class
mpg %>% ggplot(aes(x=class, y=cty)) + 
        geom_point(alpha=0.10) + 
        geom_point(aes(y=mdl1pred), color="blue", size=2) +
        theme_minimal()


############-----------------------------
#----> do not do the following
#---> it is inappropriate to create a categorical variable as a numerical variable

mpgBad <- mpg %>% mutate(class = as.numeric(class))   #<--- do NOT do this

mdl.LMBad <- lm(data=mpgBad, cty ~ class)   #<-- class is incorrectly evaluated as numeric, not categorical so dummy variables will NOT be created
summary(mdl.LMBad)

mpg$mdlBadpred <- predict(mdl.LMBad, mpgBad)

#graph the observations and predicted value for each class
mpg %>% ggplot(aes(x=class, y=cty)) + 
  geom_point(alpha=0.10) + 
  geom_point(aes(y=mdlBadpred), color="red", size=2) +
  theme_minimal()

#######################---------------------------


######  -- not in video
#build a another model to predict city mpg: two categorical variables
mdl.LM2 <- lm(data=mpg, cty ~ class + manufacturer)
summary(mdl.LM2)

mpg$mdl2pred <- predict(mdl.LM2, mpg)

#since the model is more flexible, we can see variation in the predictions per class
mpg %>% ggplot(aes(x=class, y=cty)) + 
  geom_point(alpha=0.10) + 
  geom_point(aes(y=mdl2pred), color="blue", size=2, alpha=.15) +
  theme_minimal()


#### back to the video...

#build another  simple model to predict city mpg: one categorical and one numerical var
mdl.LM3 <- lm(data=mpg, cty ~ displ + class)
summary(mdl.LM3)

mpg$mdl3pred <- predict(mdl.LM3, mpg)

mpg %>% ggplot(aes(x=displ, y=mdl3pred, group = class, color=class)) + 
               stat_smooth(method = "lm", se=FALSE) +
               theme_minimal()


#### now for interactions

#build another  simple model to predict city mpg: one categorical and one numerical var
mdl.LM4 <- lm(data=mpg, cty ~ displ * class)
summary(mdl.LM4)

mpg$mdl4pred <- predict(mdl.LM4, mpg)

mpg %>% ggplot(aes(x=displ, y=mdl4pred, group = class, color=class)) + 
  stat_smooth(method = "lm", se=FALSE) +
  theme_minimal()





