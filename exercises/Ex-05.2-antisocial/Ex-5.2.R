library(lme4)
library(nlme)
library(foreign)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/antisocial.dta") 

#1
m1 <- lmer(anti ~ pov + momage + female + childage + hispanic + black + momwork + married + (1|id), data = dat, REML = FALSE)
summary(m1)

#2
nm1 <- gls(anti ~ pov + momage + female + childage + hispanic + black + momwork + married, method = "ML", data = dat)
summary(nm1)

nm2 <- lme(anti ~ pov + momage + female + childage + hispanic + black + momwork + married, random = ~1 | id, method="ML",dat)
summary(nm2)

anova(nm1, nm2)
#Matches Stata! 
# Is there no way to do this in LME4?

#3
#Not sure what 'expression' RHS is referring to(p. 284).
1.47^2/(1.47^2 + 1.013^2)

#4
mn_pov<- aggregate(dat$pov, by = list(id = dat$id),mean, na.rm = TRUE) 
rownames(mn_pov) <- mn_pov$id
colnames(mn_pov) <- c("id", "povmn")
dat$pov_mn <- mn_pov[dat$id, "povmn"]
dat$pov_dev <- dat$pov - dat$pov_mn

m2 <- lmer(anti ~ pov_mn + pov_dev + momage + female + childage + hispanic + black + momwork + married + (1|id), data = dat, REML = FALSE)
summary(m2)

library(multcomp)
summary(glht(m2, linfct = c("pov_mn - pov_dev = 0")))#Identical to Stata. 
#p. 149 - explanation
#Sig evidence that there is an omitted L2 explanatory variable, ie random int.,
# is correlated with the L1 outcome variable. 