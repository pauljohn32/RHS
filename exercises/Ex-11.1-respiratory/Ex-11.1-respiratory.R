## Paul Johnson
## 20170501

library(foreign)
library(lme4)
library(ordinal)


dat <- foreign::read.dta("respiratory.dta12")

##1


dat1 <- reshape(dat, direction = "long", varying = c("v1", "v2", "v3", "v4"),
                   idvar = "patient", sep = "") 
dat1 <- dat1[order(dat1$patient), ]

## Make some factor variables
dat1$vf <- ordered(dat1$v, levels = c("0", "1", "2", "3", "4"))
dat1$drugf <- factor(dat1$drug, levels = c(0, 1), labels = c("no", "yes"))
dat1$patientf <- factor(dat1$patient)
dat1$genderf <- factor(dat1$male, levels = c(0,1), labels = c("female", "male"))
dat1$timef <- factor(dat1$time)

##2 ordinal::clmm uses syntax like lme4

m1 <-   clmm(vf ~ drugf + genderf + age + bl + (1|patientf),
             data = dat1, link = "logit", nAGQ = 12, threshold = "flexible")

summary(m1)#estimates match STATA

library(rockchalk)
outreg(m1, type = "html")
ordinal:::formatVC(ordinal:::varcov(m1))

##3
## Time as numeric
m2 <- clmm(vf ~ time*drugf + male + age + bl + (1|patient),
           data = dat1, link = "logit", nAGQ = 12, threshold = "flexible")
summary(m2)

m3 <- clmm(vf ~ timef*drugf + genderf + age + bl + (1|patientf),
           data = dat1, link = "logit", nAGQ = 12, threshold = "flexible")
summary(m3)

## Compare time versus dummy coding for time

anova(m1, m2)
anova(m1, m3)
anova(m2, m3)

## Interesting. RHS question does not ask me for #3, but I
## think that may be the correct model. 

## 4 Is a badly posed problem, because the "best" model is m1,
## which does not include time. Since time is not in model, the
## graphs are all flat. I will proceed as if the best model
## is m2, linear time.

## Bad news: we have to do this manually
## predict(m1)

## Error in UseMethod("predict") : 
##  no applicable method for 'predict' applied to an object of class "clmm"


m3beta <- m3$beta
m3alpha <- m3$alpha
m3re <- ranef(m3)[["patientf"]][, "(Intercept)"]

## Need to fit fixed effect model to make easy to get nd object
m4 <-  lm(v ~ timef*drugf + genderf + age + bl + patient,
           data = dat1)

##sampl <- sort(sample(1:111, 9, replace = FALSE))
sampl <- unique(dat1$patient)

nd <- rockchalk::newdata(m4, predVals=list(timef = paste(1:4), patient = sampl))
nd$re <- m3re[nd$patient]
## Now get v and vf out of original data
nd <- merge(nd, dat1[ , c("v", "vf", "patient", "timef")], by = c("patient", "timef"), sort = FALSE, all = TRUE) 
mdmm <- model.matrix(m4, data = nd)
mdmm <- cbind(mdmm, time = ifelse(mdmm[, "timef2"] == 1, 2,
                           ifelse(mdmm[ , "timef3"] == 1, 3,
                           ifelse(mdmm[ , "timef4"] == 1, 4, 1)))
              )

## put xb into mdmm matrix
mdmm <- cbind(mdmm, xb = NA)
mdmm[ , "xb"]  <- mdmm[ , names(m3beta)] %*% m3beta

mdmm <- as.data.frame(mdmm)

## nd needs patientf
mdmm$patientf <- as.factor(mdmm$patient)


## Insert random effects
mdmm$re <- m3re[mdmm[ , "patient"]]
mdmm$eta <-mdmm[ , "xb"] + mdmm[ , "re"]

names(m3alpha) <- paste0("tau", names(m3alpha))
mdmm <- cbind(mdmm, as.data.frame(t(m3alpha)))
## lp: linear predictor

lp <- mdmm$eta + mdmm[ , names(m3alpha)]
colnames(lp) <- paste0("lp", colnames(lp))

## Cumulative probabilities
cp <- exp(lp)/(1 + exp(lp))
colnames(cp) <- paste0("cp", 0:3)

mdmm <- cbind(mdmm, lp)
mdmm <- cbind(mdmm, cp)

mdmm$pr0 <- mdmm$cp0
mdmm$pr1 <- mdmm$cp1 - mdmm$cp0
mdmm$pr2 <- mdmm$cp2 - mdmm$cp1
mdmm$pr3 <- mdmm$cp3 - mdmm$cp2
mdmm$pr4 <- 1 - mdmm$cp3

par(mfcol = c(3,3))
mylty <- c(1,2,3,4)
par(ask = TRUE)
for(i in unique(mdmm$patient)){
    dframe <- mdmm[mdmm$patient == i, ]
    plot(x = c(1,4), y = c(0,1), type = "n", ylab = "Cum. Prob.", xlab = "Time")
    for(j in 0:3) {
        lines(formula(paste0("cp", j, "~ time")) , data = dframe, type = "l", lty = j + 1)
    }
}
dev.off()

library(lattice)

xyplot(cp0 ~ time | patient, data = mdmm[mdmm$patient < 10, ], type = "l")
