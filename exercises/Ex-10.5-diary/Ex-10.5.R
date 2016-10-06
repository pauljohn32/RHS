library(foreign)
library(lme4)
library(car)
library(nlme)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/dairy.dta")

#1
m1 <- glmer(fscr ~ lncfs + ai + heifer + (1 | cow), 
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
summary(m1)#failed to converge

#2
cc <- confint(m1, parm = "beta_")
tab <- cbind(estimate = fixef(m1), cc)
etab <- exp(tab)

#3
0.2082/(0.2082 + (pi^2/3))
#only 6% variance attributable to within cow differences

#4
exp(sqrt(2*0.2082)*qnorm(.75))