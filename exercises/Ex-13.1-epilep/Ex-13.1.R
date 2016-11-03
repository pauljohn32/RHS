library(foreign)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/epilep.dta")

#1 
m1 <- glmer(y ~ lbas + treat + lbas_trt + lage + v4 + (1|subj), data = dat, family = poisson(link = "log"))
summary(m1)
#estimates identical to glamm

#2
m2 <- glmer(y ~ lbas + treat + lbas_trt + lage + visit + (visit|subj), data = dat, family = poisson(link = "log"))
summary(m2)
#I'm pretty sure this is the correctly specified model.
# The loglik and the parameter estimates are essentially identical
# to glamm.

#3
p.mean <- fitted(m2)
dat$p.mean <- p.mean

trt0 <- subset(dat, subj <= 28)
trt1 <- subset(dat, subj >= 28)

library(ggplot2)

#Treatment group = 0
p <- ggplot(trt0[1:48, ], aes(x = visit, y = p.mean, group = subj)) + geom_line()
p + facet_wrap(~subj)

#Treatment group = 1
p <- ggplot(trt1[1:48, ], aes(x = visit, y = p.mean, group = subj)) + geom_line()
p + facet_wrap(~subj)