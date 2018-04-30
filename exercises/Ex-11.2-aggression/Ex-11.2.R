## Paul Johnson
## 20180502

## The Ex-11.1 respiratory example has a clear, mapped-out plan. This
## was done before that, so it was not as clear.
## I'm going to copy over my predprobs function, so I don't have to
## reinvent that, at least.


library(foreign)
library(lme4)
library(ordinal)
library(rockchalk)

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



fn <- "aggression"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

aggression <- read.dta(paste0(fn, ".dta12"))



table(aggression$y)
aggression$yf <- ordered(aggression$y, levels = 0L:2L, labels = c("no", "perhaps", "yes"))
summarize(aggression)

#1
m1 <- clmm(yf ~ do_want + other_self + blame + express + (1|person),
           data = aggression, link = "logit", nAGQ = 5, threshold = "flexible")
summary(m1)
m1.re <- ranef(m1)
## previous is a list
m1.re <- ranef(m1)[["person"]]["(Intercept)"]
head(m1.re)

## This is the way I did "Method 1" before I worked out the
## easier way in "Ex 11.1-respiratory"
## all possible predictor values
m1.newdata <- as.matrix(expand.grid(do_want = c(0, 1),
                 other_self = c(0, 1),
                 blame = c(-1, 0.5),
                 express = c(-1, 0.5)))

(m1.beta <- m1$beta)
(m1.thold <- m1$alpha)

## a linear predictor
lp <- m1.newdata[ , names(m1.beta), drop=F] %*% m1.beta
m1.newdata <- data.frame(as.data.frame(m1.newdata), lp = lp)

## We need a random effect to make eta, which is needed for
## prediction. Suppose first b = 0

predprobs(m1.newdata$lp, m1.thold)

## Grab low and hi values of random effects.
## Should we take from the predicted re's or the estimated distribution?
## I'm not sure, here we just pick values from predicted re's
reLowHigh <- quantile(m1.re[["(Intercept)"]], probs = c(0.05, 0.95))

predprobs(m1.newdata$lp + reLowHigh[1], m1.thold)
predprobs(m1.newdata$lp + reLowHigh[2], m1.thold)



##2 Insert anger and gender as predictors, conceptualized
## as predictors of latent effect 
m2 <- clmm(yf ~ do_want + other_self + blame + express + anger +
               gender + (1|person), data = aggression, link = "logit",
           nAGQ = 5, threshold = "flexible")
summary(m2)

##3 How to get non-porportional odds?
##

aggression$y01 <- aggression$y < 2
