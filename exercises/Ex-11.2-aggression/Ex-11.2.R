## Paul Johnson
## 20170502

library(foreign)
library(lme4)
library(ordinal)
library(rockchalk)

dat <- read.dta("aggression.dta12")

table(dat$y)
dat$yf <- ordered(dat$y, levels = 0L:2L, labels = c("no", "perhaps", "yes"))
summarize(dat)

#1
m1 <- clmm(yf ~ do_want + other_self + blame + express + (1|person),
           data = dat, link = "logit", nAGQ = 5, threshold = "flexible")
summary(m1)
m1.re <- ranef(m1)
## previous is a list
m1.re <- ranef(m1)[["person"]][ , "(Intercept)"]

## all possible predictor values
X <- as.matrix(expand.grid(do_want = c(0, 1),
                 other_self = c(0, 1),
                 blame = c(-1, 0.5),
                 express = c(-1, 0.5)))

(m1beta <- m1$beta)
(m1alpha <- m1$alpha)
m1alpha <- unname(m1alpha)

lp <- X[ , names(m1beta), drop=F] %*% m1beta
## Now silly dance to preserve column name "lp" within X
lp <- matrix(lp, ncol = 1, dimnames = list(NULL, "lp"))
X <- cbind(X, lp = lp)

X <- cbind(X, m1alpha1 = m1alpha[1], m1alpha2 = m1alpha[2],
           b = NA, eta1 = NA, eta2 = NA, cp1 = NA, cp2 = NA,
           p1 = NA, p2 = NA, p3 = NA)
## Now, a model for person 1
X[ , "b"] <- m1.re[2]
X[ , "eta1"] <- X[ , "lp"] + X[ , "b"] - X[ , "m1alpha1"] 
X[ , "eta2"] <- X[ , "lp"] + X[ , "b"] - X[ , "m1alpha2"] 
X[ , "cp1"] <- exp(X[ , "eta1"]) / (1 + exp(X[ , "eta1"]))
X[ , "cp2"] <- exp(X[ , "eta2"]) / (1 + exp(X[ , "eta2"]))
X[ , "p1"] <- X[ , "cp1"]
X[ , "p2"] <- X[ , "cp2"] - X[ , "cp1"]
X[ , "p3"] <- 1 - X[ , "cp2"]


##2 Insert anger and gender as predictors, conceptualized
## as predictors of latent effect 
m2 <- clmm(yf ~ do_want + other_self + blame + express + anger +
               gender + (1|person), data = dat, link = "logit",
           nAGQ = 5, threshold = "flexible")
summary(m2)

##3 How to get non-porportional odds?
##

dat$y01 <- dat$y < 2
