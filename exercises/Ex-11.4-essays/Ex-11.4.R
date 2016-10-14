library(foreign)
library(lme4)
library(car)
library(ordinal)
library(reshape2)
library(car)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/essays.dta")

#1
dat$grade1 <- recode(dat$grade, "1:2=1; 3:4=2; 5:6=3; else=4")

m1 <- clmm(factor(grade1) ~ 1 + (1|essay), data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m1)

#ICC
2.96/(2.96 + (pi^2/3))

#2
table(dat$grader)
dat$grader1 <- ifelse(dat$grader == 1, 0, 1)

m2 <- clmm(factor(grade1) ~ grader1 + (1|essay), data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m2)
anova(m1,m2)# yes model improved!

#3
m3 <- clmm(factor(grade1) ~ grader1 + wordlength +
 + sqrtwords + commas + errors + prepos + sentlength +  (1|essay)
, data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m3)

m4 <- clmm(factor(grade1) ~ grader1 + wordlength +
 + sqrtwords + commas + errors + prepos + sentlength + grader1*sqrtwords + (1|essay)
, data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m4)
anova(m3, m4)