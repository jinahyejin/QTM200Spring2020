library(readr)
library(nnet)
library(MASS)

cholesterol <- read_csv("GitHub/QTM200Spring2020/problem_sets/PS6/cholesterol.csv")
model1 <- glm(cholCat ~ sex + fat, data = cholesterol, family = binomial(link = "logit"))
summary(model1) 

model2 <- glm(cholCat ~ sex * fat, data = cholesterol, family = binomial(link = "logit"))
summary(model2)

chol_interaction <- glm(cholCat ~ sex * fat, family = binomial(link = "logit"), data = cholesterol)
chol__null <- glm(cholCat ~ 1, family = binomial(link = "logit"), data = cholesterol)


gdpChange <- read_csv("GitHub/QTM200Spring2020/problem_sets/PS6/gdpChange.csv")
gdpChange$GDPWdiff <- as.factor(as.character(gdpChange$GDPWdiff))

class(gdpChange$GDPWdiff)
mulinom_model1 <- multinom(GDPWdiff ~ REG + OIL, data = gdpChange)
summary(mulinom_model1)
exp(coef(mulinom_model1)[,c(1:3)])

ordered_model2 <- polr(GDPWdiff ~ REG + OIL, data = gdpChange, Hess = T)
summary(ordered_model2)
exp(cbind(OR = coef(ordered_model2), confint(ordered_model2)))

