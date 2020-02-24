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
mbar = 0.42 
mu0 = 0.302
sigma = 0.016*(sqrt(30))
n = 131
z = (mbar-mu0)/(sigma/sqrt(n))
z
alpha = 0.05
z0.5alpha = qnorm(1-alpha/2)
c(-z0.5alpha, z0.5alpha)

#Part b
sigma2 = 0.13*(sqrt(76))
z2 = (mbar-mu0)/(sigma2/sqrt(n))

z0.5alpha2 = qnorm(1-alpha/2)
c(-z0.5alpha2, z0.5alpha2)

