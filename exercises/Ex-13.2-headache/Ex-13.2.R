library(foreign)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/headache.dta")

dat$lndays <- log(dat$days)

#1
m1 <- glmer(y ~ period + aspartame + offset(lndays) + (1|id), data  = dat, family = poisson(link = "log"))
summary(m1)

#2
m2 <- glmer(y ~ period + aspartame + belief + belief*aspartame + offset(lndays) + (1|id), data  = dat, family = poisson(link = "log"))
summary(m2)

#3
library(DataCombine)
dat_lag <- slide(dat, Var = "aspartame", GroupVar = "id", slideBy = -1)
names(dat_lag)[9] <- "aspar_lag"

dat_lag <- dat_lag[order(dat_lag$id), ]
dat_lag[is.na(dat_lag)] <- 0

m3 <- glmer(y ~ period + aspartame + belief + belief*aspartame + aspar_lag + offset(lndays) + (1|id), data  = dat_lag, family = poisson(link = "log"))
summary(m3)

#4
m4 <- glmer(y ~ period + aspartame + belief + belief*aspartame + aspar_lag + offset(lndays) + (aspartame|id), data  = dat_lag, family = poisson(link = "log"))
summary(m4)
#I think this is the correct specification of this model
#Based on the problem in the book.
#The model does not converge. Not sure what the misspecification is. 

#5
fitted(m4)
#For expected values after link function applied.
#I don't really understand the plot being requested for this question.

#6
library(plm)
##Not sure how to run this modell
#plm is the only package with a fe option,
#but, to my knowledge, it does not allow you run models
#using the poisson distribution
