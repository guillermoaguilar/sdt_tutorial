## Signal Detection Theory tutorial in R
## Exercise 5 solution

setwd("~/git/sdt_tutorial_ecvp17/dist")


## 1. Reading data
df1 <- read.csv("data1.csv")
df1$Cond <- "A"
df2 <- read.csv("data2.csv")
df2$Cond <- "B"

df <- rbind(df1, df2)

# Unique model
fit1 <- glm(Resp ~ Stim, data=df, family=binomial('probit'))

# model.matrix(fit1) # provides the design matrix used in the GLM

# Different c, same d'
fit2 <- glm(Resp ~ Stim + Cond, data=df, family=binomial('probit'))

# Different c, different d'
fit3 <- glm(Resp ~ Stim + Cond + Stim:Cond, family=binomial('probit'), data=df)


anova(fit1, fit2, fit3, test='Chisq')

# fit2 explains the data sig. better than fit1, but fit3 does not improve it further

summary(fit2)

coef(fit2)

c_a <- -coef(fit2)[[1]]
dp <- coef(fit2)[[2]]
c_b <- -(coef(fit2)[[1]]+coef(fit2)[[3]])


# EOF
