## Signal Detection Theory tutorial in R
## Exercise 1 solution

#setwd("~/git/sdt_tutorial_ecvp17/dist")


######  Part 1a
## 1. Reading data
df <- read.csv("data1.csv")

head(df)
summary(df)

# signal-present trials
H <- sum(df[df$Stim=='S',]$Resp)  # number of hits is the number of times vector is == 1
Ns <- length(df[df$Stim=='S',]$Resp)
M <- Ns - H  # number of misses

pH_1 <- H/Ns

# signal-absent trials
FA <- sum(df[df$Stim=='A',]$Resp)  # number of hits is the number of times vector is == 1
Nn <- length(df[df$Stim=='A',]$Resp)
CR <- Nn - FA  # number of misses

pFA_1 <- FA/Nn

# showing total numbers
res <- matrix(c(H, M, FA, CR), nc=2, dimnames = list(c('Yes', 'No'), c('S', 'N')))
as.table(res)


######  Part 1b
dp_1 <- qnorm(pH_1) - qnorm(pFA_1)
c_1 <- -qnorm(pFA_1)

## c_center = C
c_center <- -(qnorm(pH_1)+qnorm(pFA_1))/2

######  Part 1c. fitting procedure using GLM
fit <- glm(Resp ~ Stim, family=binomial('probit'), data=df)
# m <- model.matrix(~Stim) # this command can be used to explore the design matrix X

summary(fit)

c_2 <- -coef(fit)[[1]]
dp_2 <- coef(fit)[[2]]

# transforming d' and c parameters to pFA and pH
pFA_2 <- 1 - pnorm(c_2)
pH_2 <- 1 - pnorm(c_2 - dp_2)

# both methods are numerically equivalent

## Confidence Intervals
confint(fit)


# EOF

