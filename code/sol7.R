## Signal Detection Theory tutorial in R
## Exercise 7 solution

#setwd("~/git/sdt_tutorial/code")

## 1. Reading data
df1 <- read.csv("data1.csv")
df1$Cond <- "A"
df2 <- read.csv("data2.csv")
df2$Cond <- "B"
df3 <- read.csv("data3.csv")
df3$Cond <- "C"
df4 <- read.csv("data4.csv")
df4$Cond <- "D"

df <- rbind(df1, df2, df3, df4)

# Unique model
fit1 <- glm(Resp ~ Stim, data=df, family=binomial('probit'))

# model.matrix(fit1) # provides the design matrix used in the GLM

# Different c, same d'
fit2 <- glm(Resp ~ Stim + Cond, data=df, family=binomial('probit'))

# model.matrix(fit2) # provides the design matrix used in the GLM

# Different c, different d'
fit3 <- glm(Resp ~ Stim + Cond + Stim:Cond, family=binomial('probit'), data=df)
# model.matrix(fit3) 

anova(fit1, fit2, fit3, test='Chisq')

# fit2 explains the data sig. better than fit1,
# and fit3 better than fit2 
# thus, we select fit3

summary(fit3)

coef(fit3)

## Derivation of estimated parameters d' and c for each condition
c_a <- -coef(fit3)[[1]]
c_b <- -(coef(fit3)[[1]]+coef(fit3)[[3]])
c_c <- -(coef(fit3)[[1]]+coef(fit3)[[4]])
c_d <- -(coef(fit3)[[1]]+coef(fit3)[[5]])

dp_a <- coef(fit3)[[2]]
dp_b <- coef(fit3)[[2]]+coef(fit3)[[6]]
dp_c <- coef(fit3)[[2]]+coef(fit3)[[7]]
dp_d <- coef(fit3)[[2]]+coef(fit3)[[8]]


## Confidence intervals
ci <- confint(fit3)

c_a_ci <- -ci[1,]
c_b_ci <- -(ci[1,] + ci[3,])
# .. and so on

dp_a_ci <- ci[2,]
dp_b_ci <- ci[2,] + ci[6,]
dp_c_ci <- ci[2,] + ci[7,]
dp_d_ci <- ci[2,] + ci[8,]


# EOF