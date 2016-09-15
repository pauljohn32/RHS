library(foreign)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/army.dta")

#1
m1 <- lmer(wbeing ~ hrs + cohes + lead + (1 | grp), data = dat, REML = TRUE)
summary(m1)

#2
mn <- aggregate(dat[c("hrs", "cohes", "lead")], by = list(grp = dat$grp), mean, na.rm = TRUE)
## Inspect ses3: How to join that back to dat? Various ideas, none smooth
rownames(mn) <- mn$grp
colnames(mn) <- c("grp", "hrs_mn", "cohes_mn", "lead_mn")
dat[,6:8] <- mn[dat$grp, c("hrs_mn", "cohes_mn", "lead_mn")]

m2 <- lmer(wbeing ~ hrs + cohes + lead + hrs_mn + cohes_mn + lead_mn + (1 | grp), data = dat, REML = TRUE)
summary(m2)

#3
m3 <- lmer(wbeing ~ hrs + cohes + lead + hrs_mn + lead_mn + (1 | grp), data = dat, REML = TRUE)
summary(m3)

vc <- as.data.frame(VarCorr(m3))

#ICC
vc[1,4]/(vc[1,4] + vc[2,4]) 
#very small proportion of the total variane 
#accounted for by between effects

#4
#P. 153, if vars are mean centered, cluster means represent between-cluster effects. If not, mean represents difference between b/w & w/i effects.
library(multcomp)
summary(glht(m3, linfct=c("hrs + hrs_mn = 0")))
summary(glht(m3, linfct=c("lead + lead_mn = 0")))
#For both variables, between and within effects are significantly different. 