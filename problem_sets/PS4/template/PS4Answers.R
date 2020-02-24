library(car)
data(Prestige)
help(Prestige)
Prestige
#creating new variable professional coded as 1, blue and white collar workers as 0
ifelse(Prestige$type == "prof", 1, 0)

#linear model with income and professional
lm(prestige ~ income + type + type:income, data = Prestige)

13.90 + 0.004032*(0) + 45.019 - 0.003178*(0)
13.90 + 0.004023*1000 + 45.019 - 0.003178*1000
59.764-58.919

13.90 + 0.004032*(6000) - 0.003178*(6000)*(0)
13.90 + 0.004032*(6000) + 45.019 - 0.003178*(6000)

64.043 - 38.092

intercept <- 0.302
coefficient_assigned <- 0.042
coefficient_assigned$se <- 0.016
coefficient_adjacent <- 0.042

#part A
xbar = 0.042 #precinct assigned law sign coefficient
mu0 = 0 #null hypothesis
sigma = 0.016*(sqrt(30)) # finding standard deviation
n = 131 #number of samples
z = (xbar-mu0)/(sigma/sqrt(n)) #z-score
z
alpha = 0.05 #given alpha
2*pnorm(-abs(z)) #determining the p value 
p = 4.13e-8
  
#Part b
sigma2 = 0.013*(sqrt(76))
z2 = (xbar-mu0)/(sigma2/sqrt(n))
z2
alpha = 0.05 #given alpha
2*pnorm(-abs(z2)) #determining the p value 
p = 3.328e-5
