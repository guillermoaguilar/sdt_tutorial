## Signal Detection Theory tutorial in R
## Exercise 10 solution - MLDS

#setwd("~/git/sdt_tutorial/code")

## 1. Reading data
df <- read.csv('datamlds.csv')

head(df)

## 2. Analyse MLDS experiment with MLDS package
# install.packages("MLDS") # if needed
library('MLDS')

stim <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

results <- as.mlbs.df(results, st=stim)
fit <- mlds(results, lnk='probit')

plot(fit)

fit$obj # -> contaings GLM 

model.matrix(fit$obj)

 # EOF