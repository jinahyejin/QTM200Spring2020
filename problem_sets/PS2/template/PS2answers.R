# Question 1

#Chi-squared test of indepndence
tbl <- matrix(c(14, 6, 7, 7, 7, 1), nrow=2, ncol=3)
rownames(tbl) = c("Upper class", "Lower class")
colnames(tbl) = c("Not Stopped", "Bribe requested", "Stopped/given warning")
tbl
chisq<- chisq.test(tbl)
pchisq(3.4125, df=2, lower.tail = FALSE)

#standardized residuals 
observed <- chisq$observed
observed
expected <- chisq$expected
expected

se <- sqrt(13.333*(1-(27/42))*(1-(21/42)))
(14-13.33333)/se
chisq$stdres

# Question 2
women123 <- read.csv("women.csv.txt")
women123

#plotting water versus GP
plot(women123$water~women123$GP)
#adding the linear regression model
temp.model <- lm(women123$water~women123$GP)
temp.model
abline(temp.model)

c(round(mean(women123$water), 2), round(sd(women123$water),2)) #the mean and standard deviation of water
standardized.x <- ((women123$water - mean(women123$water)))/sd(women123$water) #create standardized distance for each observation of water
round(standardized.x, 2) #vecot of standaridized values
r <- (1/(322-1))*sum(standardized.x) #computing correlation coefficient
r

#Question 3
#importing data
library(readr)
fruitfly <- read_csv("~/GitHub/QTM200Spring2020/problem_sets/PS2/fruitfly.csv")
View(fruitfly)
summary(fruitfly)

#lifespan vs thorax
ggplot(fruitfly, aes(x= thorax, y=lifespan))+
  geom_point()+
  geom_smooth(method=lm)

#correlation coefficient 
#for y value
c(round(mean(fruitfly$lifespan), 2), round(sd(fruitfly$lifespan),2))
#for x value
c(round(mean(fruitfly$thorax), 2), round(sd(fruitfly$thorax),2))

standardized.lifespan <- (fruitfly$lifespan - mean(fruitfly$lifespan))/sd(fruitfly$lifespan)
standardized.thorax <- (fruitfly$thorax- mean(fruitfly$thorax))/sd(fruitfly$thorax)
r_fruitfly <- (1/(125-1))*sum(standardized.lifespan*standardized.thorax)
r_fruitfly

#slope
plot(fruitfly$lifespan~fruitfly$thorax)
lst.model <- lm(fruitfly$lifespan~fruitfly$thorax)
lst.model
abline(lst.model)

#significant linear relationship between lifespan and thorax
#beta_1
beta <- sum((fruitfly$lifespan - mean(fruitfly$lifespan))* (fruitfly$thorax - mean(fruitfly$thorax)))/sum((fruitfly$thorax - mean(fruitfly$thorax))^2)
beta
reg1 <- lm(lifespan~thorax, data = fruitfly) # checking regression to check if our estimates are correct
reg1

sd_estimate <- sqrt(sum(resid(reg1)^2/(dim(fruitfly)[1]-2)))
sd_estimate

sigma(reg1)

# SE for beta_1
beta_se <- sd_estimate/sqrt(sum((fruitfly$thorax - mean (fruitfly$thorax))^2))
beta_se

2*pt((beta-0)/beta_se, dim(fruitfly)[1]-2, lower.tail = F)

#to check the right p-value
summary(lm(fruitfly$thorax~fruitfly$lifespan))

#90% confidence interval for the slope
summary(lst.model)
#slope
b1 <- 144.33
#standard error of slope value
s <- 15.77
# size
n <- 125
# t-value
t <- abs(qt(0.1/2, df = n-2)) #because the test is a two-tailed test
t
#left and right confidence intervals
left <- b1 - s*t
right <- b1 + s*t
left 
right 


#using confint
confint(lst.model, level = 0.9)

#Question 6
#individual fruitfly's lifespan when thorax=0.8
new_fruitfly<- fruitfly; 
new_fruitfly$thorax <- 0.8
p <- as.data.frame(predict(lst.model, newdata = new_fruitfly, interval = "prediction"))
mean(p$fit)

#average lifespan of fruitflies 
c<- as.data.frame(predict(lst.model, newdata = new_fruitfly, interval = "confidence"))
mean(c$upr)
mean(c$lwr)

# Question 7

ggplot(fruitfly, aes(y=lifespan, x= thorax))+
  geom_point()+
  geom_line(aes(y=lwr), data=new_fruitfly, color = "red", linetype="dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")
