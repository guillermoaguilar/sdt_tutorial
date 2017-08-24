## Signal Detection Theory tutorial in R
## Exercise 6 solution - 2AFC

setwd("~/git/sdt_tutorial_ecvp17/dist")

## 1. Reading data
df <- read.csv("data2afc.csv")

head(df)


## Parsing data for different type of trials
# Trials where signal was in the 1st interval/position
C_1 <- sum(df[df$Stim=='SN',]$Resp)  # number of 1st interval responses for trials were it was in the 1st interval
T_1 <- length(df[df$Stim=='SN',]$Resp)

# Trials where signal was in the 2nd interval/position
I_2 <- sum(df[df$Stim=='NS',]$Resp)  # number of 1st interval responses for trials were it was in the 2nd interval
T_2 <- length(df[df$Stim=='NS',]$Resp)
C_2 <- T_2 - I_2
  

# calculating percentage correct (Pc)
Pc <- (C_1+C_2)/(T_1+T_2)

# and d' from Pc
dp_1 <- qnorm(Pc)*sqrt(2)


# using GLM
fit <- glm(Resp ~ Stim, data=df, family=binomial('probit'))

# inspect the fit
summary(fit)

dp_2 <- coef(fit)[[2]] / sqrt(2)
c <- -coef(fit)[[1]] / sqrt(2)

pH <- C_1/T_1
pFA <- I_2/T_2

c_center <- -(qnorm(pH)+qnorm(pFA))/sqrt(2)


# EOF

