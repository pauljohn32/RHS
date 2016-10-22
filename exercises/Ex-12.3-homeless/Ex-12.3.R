library(foreign)
library(survival)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/homeless.dta",
  missing.type = T)

d1 <- read.dta("http://www.stata-press.com/data/mlmus3/travel1.dta",
missing.type = T)

d2 <- read.dta("http://www.stata-press.com/data/mlmus3/travel2.dta",
missing.type = T)

d1exp <- data.frame(name=rep(d1$traveler, each=4),group = rep(d1$alt, 4))



#1
# Not sure what NA is coded as in the .dta file. Does zero represent missing?
summary(dat)

#2
dat$mo_6 <- ifelse(dat$time=="1", 1,0)
dat$mo_12 <- ifelse(dat$time=="2", 1,0)
dat$mo_24 <- ifelse(dat$time=="3", 1,0) 

dat$int_6 <- dat$mo_6*dat$section8
dat$int_12 <- dat$mo_12*dat$section8
dat$int_24 <- dat$mo_24*dat$section8

#3
library(nnet)

dat$housing1 <- relevel(factor(dat$housing), ref= "0")
m1 <- multinom(housing1 ~ section8 + mo_6 + mo_12 + mo_24 + int_6 +
  int_12 + int_24, data = dat)
summary(m1)
# Not sure why results are different from Stata
# They are very different. 

#4
#Not sure how to get a good solution to
#expand table. I tried untable() in reshpae
# and there may be some good solutions in data.table. 
  
            