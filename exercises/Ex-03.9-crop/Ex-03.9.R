library(reshape)
library(foreign)
library(lme4)
library(plyr)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/cropareas.dta")

#1
m1 <- lmer(cornhec ~ cornpix + soypix + (1|county), data = dat, REML = FALSE)
summary(m1)

a <- unname(coef(summary(m1))[ , "Estimate"][1])
b1 <- unname(coef(summary(m1))[ , "Estimate"][2])
b2 <- unname(coef(summary(m1))[ , "Estimate"][3])
re <- unlist(unname(ranef(m1)[[1]][1]))

hecpred <- a + b1*dat$mn_cornpix+ b2*dat$mn_soypix + re#bingo - Cerro Gord is identical with what is should be according to book (p. 179). 

#3 
cv <- ranef(m1, condVar= T)
sqrt(attr(cv[[1]], "postVar"))

#4. I would argue no. Some clusters have only 1 observation. 