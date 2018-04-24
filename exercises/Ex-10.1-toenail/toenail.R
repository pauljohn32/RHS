## Paul Johnson
## 20160326

library(foreign)
if(!file.exists("toenail.rds")){
    download.file("http://www.stata-press.com/data/mlmus3/toenail.dta", destfile = "toenail.dta")
    dat <- read.dta("toenail.dta")
    saveRDS(dat, file = "toenail.rds")
} else {
    dat <- readRDS("toenail.rds")
}


library(rockchalk)
summarize(dat)

dat$treatmentn <- dat$treatment
dat$treatment <- factor(dat$treatment, levels = c(0, 1),
                        labels = c("itraconazole", "terbinafine"))

dat$outcomen <- dat$outcome
dat$outcome <- factor(dat$outcome, levels = c(0, 1),
                      labels = c("none/mild", "mod/severe"))
dat$visit <- as.integer(dat$visit)

dat$patient <- as.integer(dat$patient)

summarize(dat)

str(dat)

## Does R have something like xtdescribe?

## What's not missing, compar to RHS p. 516
table(by(dat$outcomen, list(dat$patient),
         function(x) paste(as.numeric(!is.na(x)), collapse = "")))


library(lattice)

t1 <- table(dat$outcome, dat$visit, dat$treatment)
t1p <- prop.table(t1)
t1pdf <- as.data.frame(t1p)
colnames(t1pdf)[1:3] <- c("outcome", "visit", "treatment")
t1pdfbad <- t1pdf[t1pdf$outcome == "mod/severe", ]
barchart(Freq ~ visit | treatment,  t1pdfbad)


m1 <- glm(outcome ~ treatment * month, data = dat,
          family = binomial(link = "logit"))
summary(m1)

## The odd's ratios

exp(coef(m1))

library(sandwich)

rse <- sqrt(diag(vcovHC(m1, type = "HC")))
rse



library(lme4)

dat$trt_month <- dat$treatmentn * dat$month


## See RHS 527
m2 <- glmer(outcome ~ treatment * month + (1 | patient),
            data = dat, family = binomial(link = "logit"),
            nAGQ = 30)
summary(m2)
#1
m3 <- glmer(outcome ~ treatment * month + (1|patient),
            data = dat, family = binomial(link = "probit"),
            nAGQ=30)
summary(m3)

#2
# See RHS p 532
#ICC for latent response
4.484/(4.484 + 1)

#3
re2 <- ranef(m2, condVar = T)
re3 <- ranef(m3, condVar = T)

re2df <- as.data.frame(re2)
re3df <- as.data.frame(re3)

c <- re2df$condval/re3df$condval
mean(c, na.rm = TRUE)
## 1.8

## regress the condval from one on other, find out
## if there is proportional transformation
m4 <- lm(re2df$condval ~ 0 + re3df$condval)
summary(m4)

## Don't suppress intercept, then what?
m4a <- lm(re2df$condval ~ re3df$condval)
summary(m4a)

## Instructor manual reverses the comparison, it puts
## probit in numerator, so
c <- re3df$condval/re2df$condval
mean(c, na.rm = TRUE)

m5 <- lm(re3df$condval ~ 0 + re2df$condval)
summary(m5)


## Note the intsructor manual has to restrict
## the sample to prevent use of more than one
## respondent (re's return fill 1908 rows of the
## data frame, but R is different.


#4

# Normal(0,1) is assumed in Probit

# If logistic model assumes Logistic(0, 1), 1 is scale parameter,
# then the variance of the logistic will be pi^2 / 3.

# ratio of standard deviations is
1 / sqrt(pi^2 / 3)
