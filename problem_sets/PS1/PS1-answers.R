#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tikzDevice)

lapply(c(),  pkgTest)

# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

##finding the confidnce interval 
#first the mean was found
m <- mean(y)
#then the standard deviation
s <- sd(y)
#n is the number of samples
n <- 25
#finding the error using t distribution as n is less than 30
error <- qt(0.95, df=n-1)*s/sqrt(n)
#left confidence interval
left <- m-error
#right confidence interval
right <- m+error
left
right
# the 90% CI is (93.96, 102.92)


#####################
# Problem 2
#####################
#Conducting a test with 0.05 significance level 
#assumptions are: continous data, random sample, the sample size is bigger (shown through formula)
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#null hypothesis Ho: pi < pio 
pio <- 100
pi <- mean(y)
pi
ts <- qt(.95, df=n-1)*s/sqrt(n)
ts
p = pt(abs(ts), df=n-1, lower.tail=F)
p
#the p value is well below the threshold of 0.05, therefore you reject the null and state that the average IQ of the students in her school is equal or higher to the average IQ score among all the schools in the country. 

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)

expenditure <- read.table("expenditure.txt", header=T)
str(expenditure)

ggplot(expenditure, aes(x=X1, y=Y))+
  geom_point()

ggplot(expenditure, aes(x=X2, y=Y))+
  geom_point()

ggplot(expenditure, aes(x=X3, y=Y))+
  geom_point()

ggplot(expenditure, aes(x=Region, y=Y))+
  geom_point()

ggplot(expenditure, aes(x=X1, y=Y))+
  geom_point()
typeof(expenditure$Region)
expenditure[,'Region'] <- factor(expenditure[,'Region'])
class(expenditure$Region)

ggplot(expenditure, aes(x=X1, y=Y, color=Region, group=Region))+
  geom_point(aes(shape=Region))

