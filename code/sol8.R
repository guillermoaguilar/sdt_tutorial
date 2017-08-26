## Signal Detection Theory tutorial in R
## Exercise 8 solution

#setwd("~/git/sdt_tutorial/code")

## 1. Reading data
df1 <- read.csv("data_small_1.csv")
df1$Cond <- "A"
df2 <- read.csv("data_small_2.csv")
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

# fit3 explains the data best

summary(fit3)

coef(fit3)

c_a <- -coef(fit3)[[1]]
dp_a <- coef(fit3)[[2]]
c_b <- -(coef(fit3)[[1]] + coef(fit3)[[3]])
dp_b <- coef(fit3)[[2]] + coef(fit3)[[4]]

ci <- confint(fit3)

c_a_ci <- -ci[1,]
c_b_ci <- -(ci[1,] + ci[3,])

dp_a_ci <- ci[2,]
dp_b_ci <- ci[2,] + ci[4,]



# EOF