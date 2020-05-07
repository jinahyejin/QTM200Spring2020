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

lapply(c("sjPlot", "googleVis"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS7")



library("lme4")
library("ggplot2")
library("googleVis")
library("sjPlot")
library("Matrix")
#####################
# Problem 1
#####################

mexico_elections <- read_csv("GitHub/QTM200Spring2020/problem_sets/PS7/MexicoMuniData.csv")
mexicomodel <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
summary(mexicomodel)
coefficients(mexicomodel)

estimate <- exp(-3.9304467 + -0.4594186+ -0.2073147)
estimate

#####################
# Problem 2
#####################

sleepstudy <- sleepstudy
pooling1 <- lm(Reaction ~ Days, data = sleepstudy)
summary(pooling1)
pooling2 <- lm(Reaction ~ Days + Subject -1, data = sleepstudy)
fitted.values(pooling2)
pooling3 <- lm(Reaction ~ Days * Subject, data = sleepstudy)
fitted.values(pooling3)
summary(pooling3)
pooling3predict <- lm(Reaction ~ (Days|Subject), data = sleepstudy)
pooling4 <- lm(Reaction ~ Days + Subject + Days:Subject, data = sleepstudy)
fitted.values(pooling4)
pooling5 <- lmer(Reaction ~ Days, data = sleepstudy)
summary(pooling5)
