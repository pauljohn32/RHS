library(foreign)
library(DataCombine)
library(lme4)
#handy package for long./time series data

dat <- read.dta("http://www.stata-press.com/data/mlmus3/wheeze.dta")

#1
dat <- slide(dat, Var = "y", TimeVar = "age", slideBy = -1, NewVar = "ylag")
#WARNING: This function leaves a 0 for the lag variable at time 1.
#This does not make sense. Stata uses a "." indicating missing vals
#in this case. Not correcting for this creates discrepant results from
#Stata. 

dat[dat$age==-2, 6] <- NA

m1 <- glm(y ~ age + smoking + ylag, family = binomial(link = "logit"), data = dat)
summary(m1)
exp(coef(m1))

#2
m2 <- glmer(y ~ age + smoking + (1|id), family = binomial(link =        
      "logit"), nAGQ = 30, data = dat)
summary(m2)
exp(fixef(m2))

#3
library(gee)

m3 <- gee(y ~ age + smoking, 
  id,
  dat,
  family = binomial(link = "logit")
  )
summary(m3)

m4 <- gee(y ~ age + smoking, 
  id,
  dat,
  family = binomial(link = "logit"),
  Mv = 1,
  corstr = "unstructured"
  )
summary(m4)

m5 <- gee(y ~ age + smoking, 
  id,
  dat,
  family = binomial(link = "logit"),
  Mv = 1,
  corstr = "AR-M"
  )
summary(m5)

exp(sapply(list(m3,m4,m5), coef))