library(foreign)
library(lme4)
library(lattice)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/thailand.dta")

#1.
m1 <- glmer(rep ~ pped + male + mses + (1|schoolid),
            weights = wt1, nAGQ = 30, data = dat, family =        
            binomial(link="logit"))
summary(m1)

#2
cc <- confint(m1, parm = "beta_")
tab <- cbind(estimate = fixef(m1), cc)
etab <- exp(tab)

#ICC
1.686/(1.686 + (pi^2/3))

#3
re1 <- ranef(m1, condVar = T, drop = TRUE)
pred1 <- predict(m1, re.form = ~(1|schoolid), type = "response", newdat = dat)
#pred1 is close to mu in as estimated in Stata

#library(pbkrtest)
#library(sjPlot)

#sjp.glmer(m1, type = "eff",facet.grid = T)

#a
dat$pred <- pred1
dat[dat$schoolid=="10104", ]
dat[dat$schoolid=="10105", ]

#b
library(ggplot2)
p <- ggplot(dat, aes(factor(male), pred))
p + geom_boxplot() + facet_grid(.~pped)
#my plot looks as if male is coded oppositely to the one 
# in the .do file in Stata. not sure what that is about. 