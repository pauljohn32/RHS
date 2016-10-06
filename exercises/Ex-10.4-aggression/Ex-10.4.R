library(foreign)
library(lme4)
library(car)
library(nlme)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/aggression.dta")

#1
dat$y1 <- factor(dat$y)
dat$y1 <- recode(dat$y1, "0='no'; else='perhaps/yes'")

#2
m1 <- glmer(y1 ~ do_want + other_self + blame + express + (1 | person), 
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
summary(m1)
exp(fixef(m1))

#3 
m2 <- glmer(y1 ~ do_want + other_self + blame + express + anger + gender +
            (1 | person), 
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
summary(m2)
exp(fixef(m2))

#4
xnam <- paste("i", 1:24, sep = "")
xadd <- paste(xnam, collapse = "+")
xform <- paste(c("0", "(1|person)", xadd), collapse="+")
form <- as.formula(paste("y1 ~", xform))
m3 <- glmer(form,
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
#model does not coverge
summary(m3)

#5
diffs <-fixef(m3)
item <- unique(dat$description)

plot(diffs ~ factor(item), cex.axis = .20)
abline(h=0, col = "red")

#6
#? Not sure how to do this in R

#7
m4 <- glmer(y1 ~ 0+offset(i1+i2+i3+i4+i5+i6+i7+i8+i9
  +i10+i11+i12+i13+i14+i15+i16+i17+
  i18+i19+i20+i21+i22+i23+i24)+(1|person),
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
summary(m4)
#could not get the estimates from this model
#and the one from Stata to match
#Misspecification in R? not sure how to write the offset
#piece of the model.

re4 <- ranef(m4, condVar = T, drop = T)
se <- sqrt(attr(re4[[1]], which = "postVar"))
eb <- unname(re4[[1]][1:316])
fixed <- coef(m4)[[1]][1]
sum.p <- aggregate(dat$y, list(person=dat$person), sum)[,2]

library(arm)
se4 <- se.ranef(m4)

plot(se ~ sum.p, col = "red", xlab = "Sum of Y by Person", main = "SE by Total Score")
points(se3[[1]] ~ sum.p, col = "green")
legend(3, .85, col=c("red", "green", "grey"), pch = c(1,1,NA), lty = c(NA,NA, 3), c("Conditional. SE", "SE of ML Estimates", "Mean sum of Y"), bty = 'n', cex = .75)
abline(v = mean(sum.p), lty = 3, col = "grey")
