## Paul Johnson
## 20180426

## This estimates an ordinal mixed model with "ordinal" and "mixor".
## It explores in good depth the problem of calculating the predicted values
## that we need from these models.

## It includes a new function I've created "predprobs" which does the work
## that could be done in those packages.


library(foreign)
library(lme4)
library(ordinal)


fn <- "respiratory"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

resp <- read.dta(paste0(fn, ".dta12"))

##1
resp2 <- reshape(resp, direction = "long",
                varying = c("v1", "v2", "v3", "v4"),
                idvar = "patient", sep = "")
## Mixor (used below) will require sorted data
resp2 <- resp2[order(resp2$patient, resp2$time), ]
head(resp2)

## Explore variants on reshape
## resp2 <- reshape(resp, direction = "long",
##                  varying = c("v1", "v2", "v3", "v4"),
##                  v.names = c("vstacked"), 
##                 idvar = "patient", sep = "") 
## resp2 <- resp2[order(resp2$patient, resp2$time), ]
## head(resp2)

## Suppose resp had v's named v_1, v_2?
## colnames(resp) <- gsub("^v", "v_", colnames(resp))
## head(resp)
## resp2 <- reshape(resp, direction = "long",
##                 varying = c("v_1", "v_2", "v_3", "v_4"),
##                 idvar = "patient", sep = "_")


## Make some factor variables
resp2$vf <- ordered(resp2$v, levels = c("0", "1", "2", "3", "4"))
resp2$drugf <- factor(resp2$drug, levels = c(0, 1), labels = c("no", "yes"))
resp2$patientf <- factor(resp2$patient)
resp2$genderf <- factor(resp2$male, levels = c(0,1), labels = c("female", "male"))
resp2$timef <- factor(resp2$time)

##2 ordinal::clmm uses syntax like lme4

m1 <- clmm(vf ~ drugf + genderf + age + bl + (1|patientf),
           data = resp2, link = "logit", nAGQ = 12,
           threshold = "flexible")

summary(m1)#estimates match STATA

## Note that the predict method fails:
predict(m1)

## Hence, I launch off on a big struggle to calculate
## probablility of outcomes for each case.

## 
##' Predicted probabilities for ordinal regression
##'
##' From the vignette distributed with ordinal, I find convenient
##' function to generate probabilities.
##' @param eta The linear predictor, X %*% betahat, plus random effect b_j.
##' @param theta vector of thresholds (NOT intercepts)
##' @param inv.link "plogis" is default, supply alternative function if
##'        using other distribution
##' @param cumulative 
##' @param ylab Text string to use in column name, default is "y"
##' @return vector of probabilities
##' @author Rune H, Paul Johnson
predprobs <- function(eta, theta, 
                      inv.link = plogis,
                      cumulative = FALSE,
                      ylab = "y"
                      ){
    cat <- 1:(length(theta)+1)
    Theta <- c(-1e3, theta, 1e3)
    ## Nested function. For one eta, calculate probabilities
    predforone <- function(oneeta){
        xupper <- inv.link(Theta - oneeta)
        if(cumulative){
            return(xupper[-1])
        } else {
            return(diff(xupper))
        }
        messg <- "predprobs: error point unexpected"
        stop(messg)
    }
    ## Apply predforone one to many values of eta  
    y <- t(vapply(eta, predforone, FUN.VALUE = numeric(length(cat))))
    if(!is.null(names(eta))) rownames(y) <- names(eta)
    colnames(y) <- paste0(ylab, ".", cat)
    y
}

## Test that, suppose eta is 0 and thresholds are -1, 0, 1
predprobs(c("row1" = 0, "row2" = 1, "row3" = 2, "row4" = NA), theta = c(-1, 0, 1))
predprobs(1, c(-1, 0, 1))
predprobs(2, c(-1, 0, 1))
predprobs(3, c(-1, 0, 1), ylab = "v")
predprobs(3, c(-1, 0, 1), cumulative = TRUE)


## Use that to make predictions, but we need eta and thresholds

## Extract Thresholds from fitted model
(m1thold <- m1$alpha)

## To make predprobs, we need eta. Where to get?
## Extract slope coefficients
(m1beta <- m1$beta)
## Extract random effects
m1re <- ranef(m1)[["patientf"]]["(Intercept)"]
## The row numbers are values of patientf

## To see where I'm going, inspect
m1beta

## If you make a small, fake data frame, using inputs
## for first case in data set:

m1newdata <- data.frame(drugfyes = c(1, 0),
                        genderfmale = c(0, 0),
                        age = c(32,32),
                        bl = c(1, 1))

## These are estimates for drug yes and no, for a male, age 32, 
(eta0 <- as.matrix(m1newdata) %*% m1beta)

## Ignoring the random effect (temporarily), we can make a "respresentative
## for a randomly drawn 'average' 'representative' person
## drugf = yes:
predprobs(eta0[1], m1thold)

## drugf = no:
predprobs(eta0[2], m1thold)

## Make a barplot or such.


## That proved the specific case, but we need to incorporate
## the group-specific random effects.
## 1. Make a prediction for a small example newdata set that has
## especially interesting predictors for interpretation.
## 2. Make a prediction for each case observed in the data set, or

## 1. Make a prediction for a small example newdata set, using
## patient 1 as example
m1newdata <- data.frame(drugfyes = c(1, 0),
                        genderfmale = c(0, 0),
                        age = c(32,32),
                        bl = c(1, 1), patientf = c(1, 1))

m1re.patient1 <- m1re[m1newdata$patientf, ]
(eta1 <- as.matrix(m1newdata[ , names(m1beta)] ) %*% m1beta +
          m1re.patient1)
(pp11 <- predprobs(eta1[1], m1thold))
(pp12 <- predprobs(eta1[2], m1thold))
barplot(rbind(pp11, pp12), beside = TRUE)
barplot(pp11)
barplot(pp12)

## This is a little "precious" because I made a small test
## data set by hand. We might want to be more comprehensive in
## creation of this new data frame. Lets get data from the real
## data set.

## Here's my plan.
## 1. fit a linear model using the variables
## 2. Use the newdata function in rockchalk to make the example
## new data frame
m1lm <- lm(v ~ drugf + genderf + age + bl, data = resp2)
m1lm.newdata <- rockchalk::newdata(m1lm,
                                   predVals = list("drugf" = "table",
                                                   "genderf" = "table",
                                                   "bl" = "table",
                                                   "age" = c(20,30,40)))
## convert that to a model matrix
m1.newmm <- model.matrix( ~  drugf + genderf + age + bl,
                data = m1lm.newdata)
m1.newmm <- as.matrix(m1.newmm)
## lp := linear predictor
m1.newmm.lp <- m1.newmm[ , names(m1beta)] %*% m1beta
m1.fit0 <- data.frame(m1.newmm, lp = m1.newmm.lp)
head(m1.fit0)

## Now all we need to do is designate which patient's random
## effect is going to apply.
m1.fit0$ranef <- m1re["3", "(Intercept)"]
m1.fit0$eta <- m1.fit0$lp + m1.fit0$ranef

m1.predprobs <- predprobs(m1.fit0$eta, m1thold)
## Look at that, think what it might mean

## I mean, ontologically, what does this mean? If patient 3's random
## intercept is applied for a list of 36 different set of predictors,
## we get predicted probabilities. If #3 were a man. If it were a woman
## If it got the drug. If it is old or young.

m1.patient3 <- data.frame(m1.fit0, m1.predprobs)



## 2. Make a prediction for each case observed in the data set

## I need to make a model.matrix object
## currently, the built-in method is underfined
## > model.matrix(m1)
## Error in eval(predvars, data, env) : object 'vf' not found

## In rockchalk, I did a lot of coding, this way will get us there.
## Run a regression, where the output is the integer version v
## and the integer version of patient is a predictor as well
m1lm <- lm(v ~ drugf + genderf + age + bl + patient,
             data = resp2)
m1.mf <- model.frame(m1lm)
m1.newmm <- model.matrix( ~ drugf + genderf + age + bl + patient,
                         data = m1.mf)
## Calculate X %*% beta
m1.newmm.lp <- m1.newmm[ , names(m1beta)] %*% m1beta
## This is the final result
m1.fit0 <- data.frame(m1.newmm, lp = m1.newmm.lp)
head(m1.fit0)

## Place the random effect into that. Clean up the m1re structure
## to make the data handling a little easier
m1re$patient <- rownames(m1re)
colnames(m1re) <- gsub("\\(Intercept\\)", "m1.re", colnames(m1re))
m1.fit0 <- merge(m1.fit0, m1re, by = "patient")
head(m1.fit0)

m1.fit0$eta <- m1.fit0[ , "lp"] + m1.fit0[ , "m1.re"]
head(m1.fit0)

m1.predprobs <- predprobs(m1.fit0$eta, m1thold)

m1.allpatients <- data.frame(m1.fit0, m1.predprobs)





library(mixor)

## mixor wants output to be an integer
resp2$vi <- as.integer(resp2$v)
mx1 <- mixor(vi ~ drugf + genderf + age + bl,
             id = patientf, data = resp2, link = "logit")

summary(mx1)


## The predict method for mixor will provide probabilties, but it
## is impossible to merge it back onto the input data because the
## rownames were lost

## The predict method from mixor
mx1.pred <- predict(mx1)
## Danger Will Robinson. The row names in mx1.pred are not correct for
## the patient id values of the data.  The row names in mx1.pred are
## consecutive case numbers, they do not link back to the original
## data frame patient numbers.  The cases have been renumbered
## consecutively.
mx1.predicted.probs <- mx1.pred[["predicted"]]

## Also danger: the row numbers are invalid
## View(mx1.predicted.probs)
##Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
##                    duplicate row.names: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111
                    
## Also, it fails when we try to use a newdata object
predict(mx1, newdata = m1.mf)
## Error in `[.data.frame`(newdata, , as.character(mf[["id"]])) : 
## undefined columns selected
##

## mixor uses name ALPHA for beta slopes
mx1.beta <- mx1$ALPHA

## EBmean is the random effects. Not modes, but means, should be similar
mx1.re <- mx1$EBmean
colnames(mx1.re) <- "mx1.re"


## Recover thresholds from mx1.  This was a bit confusing.
## I peeked into the predict.mixor code to see how
## the mixor authors view their object from inside
mx1.thold <- -c(mx1$MU[1, 1], mx1$MU[1,1] - mx1$GAM)
mx1.thold
## Double-check that by using the model coef output
## thtest <- -coef(mx1)["(Intercept)"]
## thtest[2:4] <- coef(mx1)[grep("Threshold", names(coef(mx1)), value = TRUE)] - coef(mx1)["(Intercept)"]
## thtest
## Appear same. Compare above from m1thold, very  close 


## Lets use my method above method
m1.newmm <- model.matrix( ~ drugf + genderf + age + bl + patient,
                         data = m1.mf)
## Calculate X %*% beta
m1.newmm.lp <- m1.newmm[ , names(m1beta)] %*% mx1.beta
m1.fit0 <- data.frame(m1.newmm, lp = m1.newmm.lp)
## Will need categorical patient below
m1.fit0$patientf <- as.character(m1.fit0$patient)
head(m1.fit0)

mx1.fit0 <- merge(m1.fit0, mx1.re, by.x = "patient",
                 by.y = "row.names", all.x = TRUE,
                 sort = FALSE)
## Unhappy this re-sorted the data, even though I said FALSE
mx1.fit0$eta <- mx1.fit0$lp + mx1.fit0$mx1.re
head(mx1.fit0)

mx1.predprobs <- predprobs(mx1.fit0$eta, mx1.thold)
mx1.fit0 <- cbind(mx1.fit0, mx1.predprobs)



## After I did that, I thought I might have an error.  I derived
## same thing a different way, using a structure I found inside
## mx1. 
mx1.mm <- mx1$W
lp <- mx1.mm %*% mx1.beta
colnames(lp) <- "lp"
mx1.mm <- cbind(mx1.mm, lp = lp)
mx1.mm <- cbind(mx1.mm, patient = as.integer(rownames(mx1.mm), 0))
mx1.mm <- cbind(mx1.mm, mx1.re = mx1.re[mx1.mm[ , "patient"]])
mx1.mm <- as.data.frame(mx1.mm)
mx1.mm$eta <- mx1.mm$lp + mx1.mm$mx1.re
mx1.mm <- cbind(mx1.mm, predprobs(mx1.mm$eta, mx1.thold))
## Those results for predicted probabilities match the ones in
## mx1.fit0, so I think I'm OK.


## Now, about making regression tables.
## Output is not handled perfectly well by outreg,

library(rockchalk)
outreg(m1, type = "html")
ordinal:::formatVC(ordinal:::varcov(m1))

## This fails because mx1 does not have nobs method:
outreg(mx1)
## Supply that method,
nobs.mixor <- function(object) NROW(object$W)
outreg still fails because mixor
## does not supply most of the standard regression methods
## we need, or there are errors as from
## coef(summary(mx1))
## Error: $ operator is invalid for atomic vectors
## Latex output



##3
## Time as numeric
m2 <- clmm(vf ~ time*drugf + male + age + bl + (1|patient),
           data = resp2, link = "logit", nAGQ = 12, threshold = "flexible")
summary(m2)

m3 <- clmm(vf ~ timef*drugf + genderf + age + bl + (1|patientf),
           data = resp2, link = "logit", nAGQ = 12, threshold = "flexible")
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

## Anyway, we need to work this out for model 3 in order to
## practice making good-looking graphs.

## So I'll use the method I developed above.
m3.beta <- m3$beta
m3.thold <- m3$alpha
m3.re <- ranef(m3)[["patientf"]]["(Intercept)"]

m3lm <- lm(v ~ drugf*timef + genderf + age + bl, data = resp2)
m3lm.newdata <- rockchalk::newdata(m3lm,
                                   predVals = list("drugf" = "table",
                                                   "genderf" = "table",
                                                   "bl" = "table",
                                                   "timef" = "table",
                                                   "age" = c(20,30,40)))
## convert that to a model matrix
m3.newmm <- model.matrix( ~  timef*drugf + genderf + age + bl,
                data = m3lm.newdata)
m3.newmm <- as.matrix(m3.newmm)
## lp := linear predictor
m3.newmm.lp <- m3.newmm[ , names(m3.beta)] %*% m3.beta
m3.fit0 <- data.frame(m3.newmm, lp = m3.newmm.lp)
head(m3.fit0)

## Now all we need to do is designate which patient's random
## effect is going to apply.
m3.fit0$ranef <- m3.re["3", "(Intercept)"]
m3.fit0$eta <- m3.fit0$lp + m3.fit0$ranef

m3.predprobs <- predprobs(m3.fit0$eta, m3.thold)
## Look at that, think what it might mean

## I mean, ontologically, what does this mean? If patient 3's random
## intercept is applied for a list of 36 different set of predictors,
## we get predicted probabilities. If #3 were a man. If it were a woman
## If it got the drug. If it is old or young.

m3.patient3 <- data.frame(m3.fit0, m3.predprobs)
head(m3.patient3)








amodl <- m3

amodlbeta <- amodl$beta
amodlthold <- amodl$alpha
amodlre <- ranef(amodl)[["patientf"]][, "(Intercept)"]

## Need to fit fixed effect model to make easy to get nd object
m4 <-  lm(v ~ timef*drugf + genderf + age + bl + patient,
           data = resp2)

##sampl <- sort(sample(1:111, 9, replace = FALSE))
sampl <- unique(resp2$patient)

nd <- rockchalk::newdata(m4, predVals=list(timef = paste(1:4), patient = sampl))
nd$re <- amodlre[nd$patient]
## Now get v and vf out of original data
nd <- merge(nd, resp2[ , c("v", "vf", "patient", "timef")], by = c("patient", "timef"), sort = FALSE, all = TRUE) 
mdmm <- model.matrix(m4, data = nd)
mdmm <- cbind(mdmm, time = ifelse(mdmm[, "timef2"] == 1, 2,
                           ifelse(mdmm[ , "timef3"] == 1, 3,
                           ifelse(mdmm[ , "timef4"] == 1, 4, 1)))
              )

## put xb into mdmm matrix
mdmm <- cbind(mdmm, xb = NA)
mdmm[ , "xb"]  <- mdmm[ , names(amodlbeta)] %*% amodlbeta

mdmm <- as.data.frame(mdmm)

## nd needs patientf
mdmm$patientf <- as.factor(mdmm$patient)


## Insert random effects
mdmm$re <- amodlre[mdmm[ , "patient"]]
mdmm$eta <-mdmm[ , "xb"] + mdmm[ , "re"]

names(amodlthold) <- paste0("tau", names(amodlthold))
mdmm <- cbind(mdmm, as.data.frame(t(amodlthold)))
## lp: linear predictor

lp <- mdmm$eta - mdmm[ , names(amodlthold)]
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
    plot(x = c(1,4), y = c(0,1), type = "n",
         ylab = "Cum. Prob.", xlab = "Time")
    for(j in 0:3) {
        lines(formula(paste0("cp", j, "~ time")) , data = dframe, type = "l", lty = j + 1)
    }
}
dev.off()

library(lattice)

xyplot(cp0 ~ time | patient, data = mdmm[mdmm$patient < 10, ], type = "l")
