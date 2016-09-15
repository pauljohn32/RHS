library(foreign)
library(lme4)
library(lmerTest)
library(multcomp)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/gcse.dta") 

#1
m1<- lmer(gcse ~ lrt + (lrt | school), data = dat, REML = FALSE)
summary(m1)

#2
m2 <- lmer(gcse ~ lrt + schgend + girl + schgend:girl + (lrt | school), data = dat, REML = FALSE)

#a. 
H0 = Beta(girl_school) - (Beta(girlsi) * (Beta(mixed_schl)) = 0

#b.
H0 = Beta(boy_schl) - (Beta(boysi) * (Beta(mixed_schl)) = 0

#dat$schgend <- factor(dat$schgend)
#dat$girl <- factor(dat$girl)

#matrix is rank deficient
m3 <- lmer(gcse ~ schgend + lrt + schgend:lrt + girl + (lrt | school), data = dat, REML = FALSE)
summary(m3)

dat$boy <- 1-dat$girl
dat$sc1 <- ifelse(dat$schgend == "1", 1, 0)
dat$sc2 <- ifelse(dat$schgend == "2", 1, 0)
dat$sc3 <- ifelse(dat$schgend == "3",1 , 0)

dat[,10:14] <- lapply(dat[,10:14], factor)

m4 <- lmer(gcse ~ girl + sc1 + sc2 + sc3 + girl:sc1 + lrt + girl.o:lrt + boy.o:lrt + (lrt|school), data = dat, REML = FALSE)
summary(m4)

dat$boys <- dat$schgend2
dat$girls <- dat$schgend3
dat$boy_mixed <- dat$boy * dat$schgend1
dat$girl_mixed <- dat$girl * dat$schgend1

m5 <- lmer(gcse ~ 0 + boys + girls + boy_mixed + girl_mixed + lrt + boys:lrt + girls:lrt + (lrt|school), data = dat, REML = FALSE)
summary(m5)

library(multcomp)
summary(glht(m5, linfct=c("boys - boy_mixed = 0")))
summary(glht(m5, linfct=c("girls - girl_mixed = 0")))