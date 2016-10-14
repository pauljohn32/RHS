library(foreign)
library(lme4)
library(car)
library(ordinal)
library(reshape2)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/aggression.dta")

#1
m1 <- clmm(factor(y) ~ do_want + other_self + blame + express  + (1|person), data = dat, link = "logit", nAGQ = 5, threshold = "flexible")
summary(m1)

#2
m2 <- clmm(factor(y) ~ do_want + other_self + blame + express + anger + gender + (1|person), data = dat, link = "logit", nAGQ = 5, threshold = "flexible")
summary(m2)

#3
#Not sure how to do this one in R.
