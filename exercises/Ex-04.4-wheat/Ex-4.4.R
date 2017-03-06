## Paul Johnson <pauljohn@ku.edu>
## 20160224
library(foreign)
dat <- read.dta("http://www.stata-press.com/data/mlmus3/wheat.dta")
## write.dta(dat, file = "whead.dta12")
## dat <- read.dta("wheat.dta12")
summary(dat)
## Variety should be an integer, so should plot, but we don't use it
dat$varietyf <- as.factor(dat$variety)

library(lme4)

## This is obviously overidentified/incoherent.
## lmer should bounce the user out.
m.wrong <- lmer(yield ~ moist*varietyf + (moist|varietyf), data = dat)
summary(m.wrong)

## Continue with the homework
m1 <-  lmer(yield ~ moist + (1|varietyf), data = dat)
summary(m1)

m2 <- lmer(yield ~ moist + (moist|varietyf), data = dat)
summary(m2)

library(lattice)
dotplot(ranef(m2, condVar = TRUE))

## Here's a shocker
anova(m2, m1)
## Really? Look at the pictures! 

re1 <- ranef(m2, condVar=TRUE, whichel = "varietyf")
dotplot(re1)

## Here's a spaghetti plot. 
plot(m2)
m2coef <- coef(m2)[["varietyf"]]
m2coef$variety <- rownames(m2coef)
plot(yield ~ moist, data = dat, col = dat$variety)
apply(m2coef, 1, function(x){ browser();  abline(a = x[1], b = x[2], col = x["variety"])})

## If Var(e) is small enough, even trivial slope differences are
## "statistically significant".

xyplot(yield ~ moist | varietyf, dat)

## Gave up trying to figure how to draw regression lines from lmer
## xyplot(yield ~ moist | varietyf, dat,
##        xlab = "Moisture",
##        ylab = "Yield",
##        panel = function(x, y){
##            points(x,y)
##            lines(x,y)
##        }
##        )
