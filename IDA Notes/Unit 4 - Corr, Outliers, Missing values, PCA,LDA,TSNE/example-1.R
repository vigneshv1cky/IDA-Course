library(tidyverse)
library(lme4)

data(sleepstudy,package="lme4")

str(sleepstudy)

ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  geom_point() +
  geom_smooth()

linModel <- lm(data=sleepstudy, Reaction~Days)
summary(linModel)

sleepstudy$res <- residuals(linModel)
sleepstudy$fit <- predict(linModel)

ggplot(sleepstudy, aes(x=fit, y=res)) +
  geom_point() +
  geom_smooth()

ggplot(sleepstudy, aes(x=Subject, y=res)) +
  geom_point() +
  stat_summary(color="red", size=1)

p <- ggplot(sleepstudy, aes(x=Days, y=Reaction, color=Subject)) +
  geom_point() +
  geom_smooth(se=F,span=1.5)

p

p + facet_wrap(~Subject,nrow=3)

mixed <- lmer(Reaction ~ Days + (1+Days|Subject), data=sleepstudy)
summary(mixed)

sleepstudy$res_mix <- residuals(mixed) #residuals mixed model

ggplot(sleepstudy, aes(x=Subject, y=res_mix)) +
  geom_point() +
  stat_summary(color="red", size=1) +
  stat_summary(aes(y=res), color="blue", size=1)

mean_int <- fixef(mixed)[1] #mean intercept for the mixed model
mean_slope <- fixef(mixed)[2] #mean slope for the mixed model

sleepstudy$fit_mix <- predict(mixed) #fitted values from the mixed model

ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  geom_point() +
  facet_wrap(~Subject, nrow=3) +
  geom_line(aes(y=fit_mix), size=.75) +
  geom_abline(intercept=mean_int, slope=mean_slope, color="red", size=.75)

re <- ranef(mixed)$Subject
re$Subject <- factor(rownames(re))
colnames(re)[2] <- "rand_slope"
re

sleepstudy <- merge(sleepstudy, re, by="Subject")

mean_slope <- fixef(mixed)[2]
sleepstudy$myslope <- sleepstudy$rand_slope + mean_slope
sleepstudy$myslope <- format(sleepstudy$myslope, digits=2)
sleepstudy <- arrange(sleepstudy, myslope, Days)


ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  geom_point(color="white") +
  facet_wrap(~myslope, nrow=3) +
  geom_abline(intercept=mean_int, slope=mean_slope,
              color="black", size=.75) +
  geom_line(aes(y=fit_mix), size=.75, color="white") +
  scale_x_continuous(breaks=c(0,3,6,9)) +
  labs(title="increase in reaction time (ms) per day of deprivation",
       x="days of deprivation",
       y="reaction time (ms)") +
  theme_dark(base_family="serif") +
  theme(plot.title=element_text(size=10),
        axis.title=element_text(size=10),
        strip.text.x=element_text(size=8),
        axis.text=element_text(size=8))
