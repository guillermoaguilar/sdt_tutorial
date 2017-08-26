## Signal Detection Theory tutorial in R
## Exercise 9 solution - 2AFC in two conditions

#setwd("~/git/sdt_tutorial/code")

## 1. Reading data
df1 <- read.csv("datatwoafc_2.csv")
df1$Cond <- "A"
df2 <- read.csv("datatwoafc_3.csv")
df2$Cond <- "B"

df <- rbind(df1, df2)

names(df)


fit1 <- glm(Resp ~ Stim, data=df, family=binomial('probit'))
fit2 <- glm(Resp ~ Stim + Cond , data=df, family=binomial('probit'))
fit3 <- glm(Resp ~ Stim * Cond , data=df, family=binomial('probit'))

anova(fit1, fit2, fit3, test='Chisq')

# fit3 explains better the data 

dp_a <- coef(fit3)[[2]] / sqrt(2)

dp_b <- (coef(fit3)[[2]] + coef(fit3)[[4]])/ sqrt(2)



# EOF

