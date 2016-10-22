library(foreign)
library(survival)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/hybrid.dta")

#1
m1 <- clogit(chosen ~ price + cost + range + electric + hybrid + highperf + medhiperf + strata(id), data = dat)
summary(m1)

#3-4 
#NA

