library(foreign)
library(lme4)
library(car)
library(ordinal)
library(reshape2)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/respiratory.dta")

#1
dat1 <- melt(dat, id.vars = c("center", "male", "drug", "age", "bl", "patient"))
dat1 <- dat1[order(dat1$patient), ]

#2
m1 <- clmm(factor(value) ~ drug + male + age + bl + (1|patient), data = dat1, link = "logit", nAGQ = 10, threshold = "flexible")
summary(m1)#estimates match STATA

#3 
dat1$variable <- recode(dat1$variable, "'v1'=1; 'v2'=2; 'v3'=3; 'v4'=4") 
dat1$variable <- as.numeric(dat1$variable)

m2 <- clmm(factor(value) ~ variable + drug + male + age + bl + (1|patient), data = dat1, link = "logit", nAGQ = 10, threshold = "flexible")
summary(m2)

m3 <- clmm(factor(value) ~ variable + factor(drug) + variable*drug + male + age + bl + (1|patient), data = dat1, link = "logit", nAGQ = 10, threshold = "flexible")
summary(m3)

anova(m2, m3)

#no slope X tx difference, although they differ at BL
# time is also not a predictor, so no linear trend. 

#4
#fitted(m1, newdata = dat1)
#Not sure how to obtain the cumulative probabilties in R