## Paul Johnson
## 20180429

## I worked out a plan for ordinal model predictions in
## Ex-11.1-respiratory. Here I copy in the predprobs
## function and I'm forcing same "solution" into this case.

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
##' @return vector of probabilities, one for each value of eta
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


library(foreign)
library(ordinal)

fn <- "tvsfpors"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}
assign(fn, read.dta(paste0(fn, ".dta12")))

## Mixor requires sorted data, so do this NOW to make results comparable
tvsfpors <- tvsfpors[order(tvsfpors$school, tvsfpors$class), ]
rownames(tvsfpors) <- 1:NROW(tvsfpors)

## Create factor for thkf
tvsfpors$thkf <- as.factor(tvsfpors$thk) 

## Lets fit an ordinal model with no random effects, for later comparison
m0 <- clm(factor(thk) ~ cc + tv + cc*tv + prethk, data = tvsfpors, link = "logit")
m0.beta <- m0$beta


#1
m1 <- clmm(thkf ~ cc + tv + cc*tv + prethk  + (1|school),
           data = tvsfpors, link = "logit", nAGQ = 30)
summary(m1)

m1.beta <- m1$beta
m1.thold <- m1$alpha

## lr test for random effects
anova(m0, m1)


#a.
exp(coef(m1))

#b.
.07351/(.07351 + (pi^2/3))
# School effect is negligible

#2



m2 <- clmm(thkf ~ cc + tv + cc*tv + prethk  + (1|class),
           data = tvsfpors, link = "logit", nAGQ = 30)
summary(m2)
m2.beta <- m2$beta
m2.thold <- m2$alpha


anova(m0, m1)
anova(m1, m2)

#a.
exp(coef(m2))

#b.
.1886/(.1886 + (pi^2/3))

#c. 
#

## Create predicted values/probabilities


## Note the following fails because of row mismatches due to missing values
## tvsfpors$m1.fit <- fitted(m1)
## Error in `$<-.data.frame`(`*tmp*`, m1.fit, value = c(0.300737722066932,  : 
##  replacement has 1600 rows, data has 1601

## I have no patience for the predict methods of ordinal or mixor anymore
## and I fixes a plan in Ex-11.1-respiratory, so use that here.

## We need to recalculate a linear predictor and eta

## So use my my method to manufacture the "all cases" new data matrix
tvsf.mf <- model.frame(thkf ~ cc + tv + cc*tv + prethk + class + school,
                       data = tvsfpors)
## Note that tvsf.mf deleted the case that had a missing value


## Because this example does not use any R factor variables,
## it is not necessary to do the next step which converts the
## predictors to numerics.  But I still do it, to make
## this example comparable to other ones.
tvsf.mm <- model.matrix( ~ cc + tv + cc*tv + prethk + class + school,
                         data = tvsf.mf)
## fitted value: linear predictor + random effect

## 
m0.lp <- tvsf.mm[ , names(m0.beta)] %*% m0.beta
m1.lp <- tvsf.mm[ , names(m1.beta)] %*% m1.beta
m2.lp <- tvsf.mm[ , names(m2.beta)] %*% m2.beta

tvsf.fits <- data.frame(tvsf.mm, m0.lp = m0.lp,
                        m1.lp = m1.lp, m2.lp = m2.lp,
                        check.names = FALSE)
## check.names = FALSE to prevent "cc:tv" from being
## turned into "cc.tv".  

## lets put the random effects into that as well
m2.ranef <- ranef(m2)[["class"]]
tvsf.fits$m2.ranef <- m2.ranef[as.character(tvsf.fits$class), "(Intercept)"]

m1.ranef <- ranef(m1)[["school"]]
tvsf.fits$m1.ranef <- m1.ranef[as.character(tvsf.fits$school), "(Intercept)"]

tvsf.fits$m1.eta <- tvsf.fits$m1.lp + tvsf.fits$m1.ranef
tvsf.fits$m2.eta <- tvsf.fits$m2.lp + tvsf.fits$m2.ranef



## predict(m2, newdata = tvsf.mm)
## Error in UseMethod("predict") : 
##  no applicable method for 'predict' applied to an object of class "clmm"

## Inspect the distribution of the random effects
m2.ranef <- ranef(m2)[["class"]][["(Intercept)"]]
hist(m2.ranef)

m1.ranef <- ranef(m1)[["school"]][["(Intercept)"]]
hist(m1.ranef)


## Extractor for SD of random effect
m2$ST[["class"]][, "(Intercept)"]


## Selects random effects that are at the 5% and 95%tiles

qnorm(0.95) * c(1, -1) * m2$ST[["class"]][, "(Intercept)"]


predprobs(.7, m2$Theta)

m1.predprobs <- predprobs(tvsf.fits$m1.eta, m1.thold, ylab = "m1.y")
m2.predprobs <- predprobs(tvsf.fits$m2.eta, m2.thold, ylab = "m2.y")
m1.predcumul <- predprobs(tvsf.fits$m1.eta, m1.thold, cumulative = TRUE, ylab = "m1.yc")
m2.predcumul <- predprobs(tvsf.fits$m2.eta, m2.thold, cumulative = TRUE,  ylab = "m2.yc")



tvsf.fits <- data.frame(tvsf.fits, m1.predprobs, m2.predprobs,
                        m1.predcumul, m2.predcumul, check.names = FALSE)
plot(tvsf.fits$m1.y.3, tvsf.fits$m2.y.3)
plot(tvsf.fits$m1.yc.3, tvsf.fits$m2.yc.3)



## Compare mixor
library(mixor)

mx1 <- mixor(thk ~ cc + tv + cc*tv + prethk, id = school, data = tvsfpors,
             link =  "logit", NQ1 = 30)
summary(mx1)

## Observe a predict error now:
mx1.predict <- predict(mx1, na.action = na.pass)
## Error in neww %*% zeta : non-conformable arguments
## predict(mx1, newdata = tvsf.fits)
## The predict( .. , newdata = ??) does not work with mixor,
## I lost my patience with that, we'll use my method


mx1.pred <- mx1.predict[["predicted"]]
## mx1.pred return is not useful because rows are not named
## in a way to merge with original data. They have school
## name as rowname, not individual name.
head(mx1.pred, 15)

mx1.class <- mx1.predict[["class"]]



## Because the predict method does not return something we can merge
## with original data, we use "my method" from Ex-11.1 to create
## linear predictor and eta

(mx1.beta <- mx1$ALPHA)
## Extractor for slopes. Note this stays as a matrix, not a vector
## so we make it into a named vector
mx1.beta <- mx1.beta[ ,1]

mx2.beta <- mx2$ALPHA[ , 1]
## Note similarity to clmm
m1.beta
m2.beta

## Calculate linear predictors
mx1.lp <- as.matrix(tvsf.fits[ , names(mx1.beta)]) %*% mx1.beta
mx2.lp <- as.matrix(tvsf.fits[ , names(mx1.beta)]) %*% mx2.beta
tvsf.fits <- data.frame(tvsf.fits, mx1.lp = mx1.lp, mx2.lp = mx2.lp)


## Get the random effects
mx1.ranef <- mx1$EBmean ## random effect "predictions", compare to
## conditional modes from other random effect models in R, 
head(mx1.ranef)
## Name that column
colnames(mx1.ranef) <- "mx1.re"


mx1.thold <- -c(mx1$MU[1, 1], mx1$MU[1,1] - mx1$GAM)
mx1.thold
## Compare to m1.thold
m1.thold

## Nervous about merge, so carefully inspect result.
## after merge, can't assume row ordering is same/safe
## anymore, so be extra cautious in other work.
tvsf.fits2 <- merge(tvsf.fits, mx1.ranef, all.x = TRUE,
                    by.x = "school", by.y = "row.names",
                    sort = FALSE)


mx2 <- mixor(thk ~ cc + tv + cc*tv + prethk, id = class, data = tvsfpors,
             link =  "logit", NQ1 = 30)
summary(mx2)

mx2.thold <- -c(mx2$MU[1, 1], mx2$MU[1,1] - mx2$GAM)
mx2.thold

## Compare to m2.thold
m2.thold

mx2.ranef <- mx2$EBmean ## random effect "predictions", compare to
## conditional modes from other random effect models in R, 
head(mx2.ranef)
## Name that column
colnames(mx2.ranef) <- "mx2.re"

tvsf.fits2 <- merge(tvsf.fits2, mx2.ranef, all.x = TRUE,
                    by.x = "class", by.y = "row.names",
                    sort = FALSE)

tvsf.fits2$mx1.eta <- tvsf.fits2$mx1.lp + tvsf.fits2$mx1.re
tvsf.fits2$mx2.eta <- tvsf.fits2$mx2.lp + tvsf.fits2$mx2.re


mx1.preds <- predprobs(tvsf.fits2$mx1.eta, mx1.thold, ylab= "mx1.y")
mx2.preds <- predprobs(tvsf.fits2$mx2.eta, mx2.thold, ylab = "mx2.y")

## Now we have predictions for all the methods to compare


