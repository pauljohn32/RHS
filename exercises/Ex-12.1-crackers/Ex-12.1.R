library(foreign)
library(survival)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/crackers.dta")

#1
dat1 <- transform(dat, resp = as.numeric(interaction(id, occ, drop = TRUE)))
dat1$Sunshine <-ifelse(dat1$brand=="Sunshine", 1, 0)
dat1$Keebler <- ifelse(dat1$brand == "Keebler", 1, 0)
dat1$Nabisco <- ifelse(dat1$brand == "Nabisco", 1, 0)
dat1$Private <- ifelse(dat1$brand == "Private", 1, 0)

#2
m1 <- clogit(choice ~ feature + display + price + Sunshine + Nabisco + Keebler + strata(resp), data = dat1)
summary(m1)
#4
#NA

#5-8
# I don't believe there is an R equivalent for these models. 