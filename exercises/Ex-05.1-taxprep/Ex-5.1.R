library(foreign)
library(lme4)
library(plm)


dat <- read.dta("taxprep.dta12")
dat$subjectf <- as.factor(dat$subject)
dat$timef <- as.factor(dat$time)

## Keep a spare copy of original data, will use once below
dat.orig <- dat

## The variables "f1040a" and "f1040ez" are not in the variables list for this
## exercise, so I'll  show you how I would omit them
dat[ , c("f1040a", "f1040ez")] <- NULL

##4.1

## Some people think the question asks us to include time as a predictor.
## I don't know, might as well try both ways.


m1 <- lmer(lntax ~  prep + ms + hh + depend + age + lntpi + mr +
               emp + (1|subjectf), data = dat, REML = FALSE)
summary(m1)

## Should time be treated as an integer or a factor

m2.1 <- lmer(lntax ~  time + prep + ms + hh + depend + age + lntpi + mr +
               emp + (1|subjectf), data = dat, REML = FALSE)
summary(m2.1)

m2.2 <- lmer(lntax ~  timef + prep + ms + hh + depend + age + lntpi + mr +
               emp + (1|subjectf), data = dat, REML = FALSE)
summary(m2.2)

anova(m2.1, m2.2)

## Introduce time as a random effect as well, just to see what might happen
m2.3 <- lmer(lntax ~  0 + prep + ms + hh + depend + age + lntpi + mr +
               emp + (1|subjectf) + (1|timef), data = dat, REML = FALSE)
summary(m2.3)
ranef(m2.3)


##2 Lets work on the "within" == fixed effects and "between" == regression
## on means.

## A copy of the gmd function, from the guide on group mean centering
gmd <- function(dframe, x, by, FUN = mean, suffix = c("_mn", "_dev")){
    xmean <- aggregate(dframe[ , x, drop = FALSE],
                       dframe[ , by, drop = FALSE], FUN,
                       na.rm = TRUE)
    df2 <- merge(dframe, xmean, by = by, suffix = c("", suffix[1]))
    for(i in x){
        df2[ , paste0(i, suffix[2])] <- df2[ , i] - df2[ , paste0(i, suffix[1])]
    }
    df2[ , colnames(df2)[!colnames(df2) %in% colnames(dframe)]]
}


dat.centered <- gmd(dat, c("ms", "hh", "depend", "age", "tpi", "txrt", "mr", "emp", "prep", "tax",
                 "time", "lntpi", "lntax"), by = "subjectf")
dat <- cbind(dat, dat.centered)
rm(dat.centered)

## Manually fit the "within" (fixed effects) model with data in deviations form.  As we
## found in the past, this will give correct coefficients, but wrong standard errors.
## Lets leave out time
m2_w1 <- lm(lntax_dev ~ prep_dev + ms_dev + hh_dev + depend_dev + age_dev + lntpi_dev + mr_dev + emp_dev, data = dat)
summary(m2_w1)

## Will give correct coefficients and correct standard errors,
## but output is littered with dummy coefficients
m2_w2 <- lm(lntax ~ 0 + prep + ms + hh + depend + age  +
                lntpi + mr + emp + subjectf, data = dat)
summary(m2_w2)


## Is it sensible to include time in a "within" model?
## Maybe, might as well try it
m3_w1 <- lm(lntax ~ 0 + time +  prep + ms + hh + depend + age  +
                lntpi + mr + emp + subjectf, data = dat)
summary(m3_w1)

## As a factor
m3_w2 <- lm(lntax ~ 0 + timef +  prep + ms + hh + depend + age  +
                lntpi + mr + emp + subjectf, data = dat)
summary(m3_w2)


## Allow intercept to be estimated
m3_w3 <- lm(lntax ~  timef +  prep + ms + hh + depend + age  +
                lntpi + mr + emp + subjectf, data = dat)
summary(m3_w3)


## Need group means for between regression model
dat.gr.means <- aggregate(dat.orig, by = list(dat$subject), mean, na.rm = TRUE)
colnames(dat.gr.means) <- paste0(colnames(dat.gr.means), "_m")

m2_m <- lm(lntax_m ~ time_m + prep_m + ms_m + hh_m + depend_m + age_m +
               lntpi_m + mr_m + emp_m , data = dat.gr.means)
summary(m2_m)


## Lets confirm the estimates with the functions in plm
library(plm)


## plm is similar to Stata's xt regression family. You can declare a new
## type of data frame and the regression functions take note of the
## information
dat.plm <- plm.data(dat.orig, indexes = c("subjectf", "time"))
summary(dat.plm)

## Note how time gets omitted from the output because the "mean" of time is constant
plm_b <- plm(lntax ~ prep + ms + hh + depend + age + lntpi + mr + emp,
             data = dat, effect = "individual", model = "between", index = "subjectf")
summary(plm_b)


## Within models.

## Should time be a predictor? Lets say  no, just to compare 
plm_w1 <- plm(lntax ~ prep + ms + hh + depend + age + lntpi + mr + emp, data = dat.plm,
              effect = "individual", model = "within", index = "subject")
summary(plm_w1)

## To compare the lm fit, we have to wade through all the coefficients
coef(m2_w2)[-grep("subject", names(coef(m2_w2))) ]
coef(plm_w1)

coef(summary(m2_w2))[ -grep("subject", names(coef(m2_w2))),]

## Note even if time is an integer, plm turns it into a factor for us.
## plm scans the input data for variable named "time", treats it differently.
## If you did want to treat time as numeric, you need to rename that variable.

## Treat time as a factor with fixed coefficients:
plm_w2 <- plm(lntax ~   prep + ms + hh + depend + age + lntpi + mr + emp, data = dat.plm,
             effect = "individual", model = "within", index = "subject")
summary(plm_w2)

## Now, compare that to the m3_w3 fit, where intercept was included
## and time was a factor
coef(summary(m3_w3))[ -grep("subject", names(coef(m3_w3))),]
## Previous should be identical to the plm output plm_w2

## Treat time as a part of "two-way" effect. This is a Panel Linear Modeling Jargon.
## Similar to fixed effect jargon, it fits indiviudal and the time effect. 
## From output
plm_w3 <- plm(lntax ~  prep  + ms + hh + depend + age + lntpi + mr +
                  emp, data = dat.plm,
              effect = "twoways", model = "within", index = c("subject", "time"))
summary(plm_w3)
## 


## Random effects with econometric corrections. Note these differe
## from lmer output These vary in the caluclation of the variance
## components

plm_r1 <- plm(lntax ~  prep + ms + hh + depend + age + lntpi + mr + emp, data = dat.plm,
             effect = "individual", model = "random", index = "subject")
summary(plm_r1)

## Compare that to lmer output:
lmer_r1 <- lmer(lntax ~  prep + ms + hh + depend + age + lntpi + mr + emp + (1|subjectf), data = dat,
                REML=FALSE)
summary(lmer_r1)

## Might as well try some alternative "random.method" estimators
plm_r2 <- plm(lntax ~  prep + ms + hh + depend + age + lntpi + mr + emp, data = dat.plm,
             effect = "individual", model = "random", index = "subject", random.method = "nerlove")
summary(plm_r2)



## Hausman test: pht(within, random_effects)
phtest(plm_r1, plm_w2)
## Previous same as:
phtest(lntax ~  time + prep + ms + hh + depend + age + lntpi + mr + emp, data = dat.plm)


#3
l1.resid <- residuals(m1, type = "response")
l2.resid <- ranef(m1)[["subjectf"]][ , "(Intercept)"]

l1 <- seq(min(l1.resid), max(l1.resid), length.out = 200)
l1.d <- dnorm(l1, mean = mean(l1.resid), sd = sd(l1.resid))
l2 <- seq(min(l2.resid), max(l2.resid), length.out = 200)
l2.d <- dnorm(l2, mean = mean(l2.resid), sd = sd(l2.resid))

dev.new(width=8, height=6)
par(mfrow=c(1,2))
hist(l1.resid, main = "Level 1 Residual", freq = FALSE, xlim = c(min(l1.resid), max(l1.resid)))
lines(density(l1.resid), col = "red")
lines(l1, l1.d, col = "blue")
legend("topleft", c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .7, bty = 'n')

hist(l2.resid, main = "Level 2 Residual", freq = FALSE, xlim = c(min(l2.resid),max(l2.resid)))
lines(density(l2.resid), col = "red")
lines(l2, l2.d, col = "blue")
legend("topleft", c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .7, bty = 'n')
def.off()
