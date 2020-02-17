library(ggplot2)
library(dplyr)
library(tidyverse)

library(readr)

#Question 1
incumbents_subset <- read_csv("GitHub/QTM200Spring2020/problem_sets/PS3/incumbents_subset.csv")

fitmodel <- lm(voteshare ~ difflog, data=incumbents_subset)
summary(fitmodel)

plot(incumbents_subset$voteshare ~ incumbents_subset$difflog)
fitmodel
abline(fitmodel)

incumbents_subset$predicted <- predict(fitmodel)
incumbents_subset$residuals1 <- residuals(fitmodel)

plot_residuals <- ggplot(incumbents_subset, aes(x=difflog, y=voteshare))+
  geom_point(alpha = I(0.5))+
  geom_point(aes(y= predicted), shape =1, alpha = I(0.1))+
  geom_segment(aes(xend = difflog, yend = predicted), alpha=I(0.1))+
  geom_smooth(method="lm", se=F, color = "lightgrey")
plot_residuals

# Question 2
fitmodel2 <- lm(presvote~difflog, data = incumbents_subset)
summary(fitmodel2)

plot2<- ggplot(incumbents_subset, aes(x=difflog, y=presvote))+
  geom_point(alpha=I(0.5))+
  geom_smooth(method = "lm", se = F, color = "lightgrey")
plot2

incumbents_subset$prediction <- predict(fitmodel2)
incumbents_subset$residuals2 <- residuals(fitmodel2)

plot2_residuals <-ggplot(incumbents_subset, aes(x=difflog, y=presvote))+
  geom_point(alpha = I(0.5))+
  geom_point(aes(y= prediction), shape =1, alpha = I(0.1))+
  geom_segment(aes(xend = difflog, yend = prediction), alpha=I(0.1))+
  geom_smooth(method="lm", se=F, color = "lightgrey")
plot2_residuals               
  
# Question 3
fitmodel3 <- lm(voteshare ~ presvote, data=incumbents_subset)
summary(fitmodel3)

plot3<- ggplot(incumbents_subset, aes(x=presvote, y=voteshare))+
  geom_point(alpha=I(0.5))+
  geom_smooth(method = "lm", se=F, color = "lightgrey")
plot3

incumbents_subset$predicted3 <- predict(fitmodel3)
incumbents_subset$residuals3 <- residuals(fitmodel3)

plot3_residuals <-ggplot(incumbents_subset, aes(x=presvote, y=voteshare))+
  geom_point(alpha = I(0.5))+
  geom_point(aes(y= predicted3), shape =1, alpha = I(0.1))+
  geom_segment(aes(xend = presvote, yend = predicted3), alpha=I(0.1))+
  geom_smooth(method="lm", se=F, color = "lightgrey")
plot3_residuals   


#Question 4
fitmodel4 <- lm(residuals1 ~ residuals2, data = incumbents_subset)
summary(fitmodel4)

plot4 <- ggplot(incumbents_subset, aes(x=residuals2, y=residuals1))+
  geom_point()+
  geom_smooth(method = "lm", se=F, color = "lightgrey")
plot4

#Question 5
multiplefit<- lm(voteshare~difflog+presvote, data = incumbents_subset)
summary(multiplefit)
