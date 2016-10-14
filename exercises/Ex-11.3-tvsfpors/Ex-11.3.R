library(foreign)
library(lme4)
library(car)
library(ordinal)
library(reshape2)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/tvsfpors.dta")

#1
m1 <- clmm(factor(thk) ~ cc + tv + cc*tv + prethk  + (1|school), data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m1)

#a.
exp(coef(m1))

#b.
.07351/(.07351 + (pi^2/3))
# School effect is negligible

#2
m2 <- clmm(factor(thk) ~ cc + tv + cc*tv + prethk  + (1|class), data = dat, link = "logit", nAGQ = 30, threshold = "flexible")
summary(m2)

#a.
exp(coef(m2))

#b.
.1886/(.1886 + (pi^2/3))

#c. 
#It;s like class does, but the extent is still small. 