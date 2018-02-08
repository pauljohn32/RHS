## ----import--------------------------------------------------------------
library(foreign)
dat <- read.dta("anorexia.dta12")
rockchalk::summarize(dat)

## ------------------------------------------------------------------------
library(dplyr)
t0.dplyr <- dat %>%
    group_by(treat) %>%
    summarize(N = n(), Preweight = mean(weight1, na.rm = TRUE),
              Postweight = mean(weight2, na.rm = TRUE))
t0.dplyr

## ------------------------------------------------------------------------
t1 <- aggregate(dat[, c("weight1", "weight2")],
                by = list(treat = dat$treat), mean, na.rm = TRUE)
t1[ , "N"] <- table(dat$treat)
t1

## ------------------------------------------------------------------------
t2 <- lapply(split(dat, f = dat$treat),
       function(x){
           data.frame(treat = unique(x$treat),
             N = NROW(x),
             weight1 = mean(x$weight1, na.rm = TRUE),
             weight2 = mean(x$weight2, na.rm = TRUE))
       }
)
do.call("rbind", t2)

## ----histo---------------------------------------------------------------
library(lattice)
histogram( ~ weight2 | treat, data = dat)

## ----boxplot-------------------------------------------------------------
boxplot(weight2 ~ treat, data = dat)

## ------------------------------------------------------------------------
dat$treat <- relevel(dat$treat, "contrl")
contrasts(dat$treat)

## ------------------------------------------------------------------------
m1 <- lm(weight2 ~ weight1 + treat, data = dat)
summary(m1)

## ------------------------------------------------------------------------
library(rockchalk)
predictOMatic(m1, predVals = c("weight1", "treat"))

## ----ps10----------------------------------------------------------------
plotSlopes(m1, plotx = "weight1", modx = "treat")

## ----ps20----------------------------------------------------------------
plotSlopes(m1, plotx = "weight1", modx = "treat", interval = "confidence")

## ----fancyt--------------------------------------------------------------
##' T-test for the difference in 2 regression parameters
##'
##' This is the one the students call the "fancy t test".
##'
##' I did this because I have trouble understanding terminology
##' in canned functions in other R packages
##' @param parm1 A parameter name, in quotes!
##' @param parm2 Another parameter name, in quotes!
##' @param model A fitted regression model
##' @return A list with the difference, std. err., t-stat,
##' and p value
##' @author Paul Johnson
fancyt <- function(parm1, parm2, model, model.cov = NULL){
    V <- function(mat, parm1, parm2 = NULL) {
        if(is.null(parm2)) return (mat[parm1, parm1])
        else return(mat[parm1, parm2])
    }
    if(is.null(model.cov)) model.cov <- vcov(model)
    
    model.coef <- coef(model)[c(parm1, parm2)]
    se <- sqrt(V(model.cov, parm1) + V(model.cov, parm2) - 2*V(model.cov, parm1, parm2))
    diff <- model.coef %*% c(1, -1)
    t <-  diff/se
    pval <- pt(abs(t), lower.tail = FALSE, df = model$df) * 2
    c(diff = diff, Std.Err. = se, t = t, p = pval)
}

## ----fancyt20------------------------------------------------------------
fancyt("treatcbt", "treatft", m1)

## ------------------------------------------------------------------------
plot(m1)

## ------------------------------------------------------------------------
## Exercise asks about homoskedasticity, do robust standard errors
library(sandwich)
## To match stata, we need type HC1
m1.robust <- vcovHC(m1, type = "HC1")
## StdError
m1.robust.se <- sqrt(diag(m1.robust))
m1.robust.se

## Can compare to HC2 and HC3 in Stata, which are same:
sqrt(diag(vcovHC(m1, type = "HC2")))
sqrt(diag(vcovHC(m1, type = "HC3")))

## ------------------------------------------------------------------------
fancyt("treatcbt", "treatft", m1, m1.robust)

## ---- results="hide"-----------------------------------------------------
o1 <- outreg(list("m1 homo" = m1, "m1 robust" = m1 ),
       SElist = list("m1 robust" = m1.robust.se),
       type = "html", browse = FALSE)


## ----o1, results="asis"--------------------------------------------------
##markdown can't render HTML correctly
o1 <- gsub("[ ]{2,}", " ", o1)
cat(o1)

## ------------------------------------------------------------------------
m1.resid <- resid(m1)
hist(m1.resid)

## ----histplotter---------------------------------------------------------
##' Histogram with added Kernel Density Smooth and Normal Probability
##'
##' See the working example at
##' http://pj.freefaculty.org/guides/Rcourse/WorkingExamples/
##' plot-histogramWithLinesAndLegend.html.. content for \details{} ..
##' @param x A variable to be plotted
##' @param main Title for graph
##' @return none
##' @author Paul Johnson
drawHist <- function(x, main = "Histogram"){
    xlab <- deparse(substitute(x))
    par(mai=c(1,1,2.5,1)) ## margins
    par(xpd=TRUE) ## write outside plot region
    
    myhist <- hist(x, prob = TRUE, main=main, xlab = xlab)
    lines(density(x))
    xseq1 <- seq(min(x), max(x), length.out=100)
    m1 <- mean(x)
    sd1 <- sd(x)
    obsNormal <- dnorm(xseq1, mean=m1, sd=sd1)
    lines(xseq1, obsNormal, lty=2, col="red")
    usr <- par("usr")
    legend(0.5 * usr[1] + 0.5 * usr[2], 1.3*usr[4],
           legend=c("observed density", "normal with observed mean & sd"),
           lty=c(1,2), col=c("black", "red"))
}

## ----residplot10, fig.height=6-------------------------------------------
drawHist(resid(m1), main = "Regression residuals")

## ----m3------------------------------------------------------------------
m3 <- lm(weight2 ~ weight1 * treat, data = dat)
summary(m3)

## ----m3ps10--------------------------------------------------------------
m3.ps <- plotSlopes(m3, plotx = "weight1", modx = "treat", interval = "confidence")

## ----m3ts10--------------------------------------------------------------
m3.ps.ts <- testSlopes(m3.ps)

## ----m3robust------------------------------------------------------------
m3.robust <- vcovHC(m3)
m3.robust.se <- sqrt(diag(sandwich(m3)))

## ----m3outreg10, results="hide"------------------------------------------
o3 <- outreg(list("m3 homo" = m3, "m3 robust" = m3 ),
       SElist = list("m3 robust" = m3.robust.se),
       type = "html")

## ----m3outreg20, results="asis"------------------------------------------
## markdown error correction:
o3 <- gsub("[ ]{2,}", " ", o3)
cat(o3)

## ------------------------------------------------------------------------
fancyt("treatcbt", "treatft", m3, m3.robust)
fancyt("weight1:treatcbt", "weight1:treatft", m3)
fancyt("weight1:treatcbt", "weight1:treatft", m3, m3.robust)

## ----m3rr, fig.height=6--------------------------------------------------
drawHist(resid(m3), main = "Regression residuals")

## ------------------------------------------------------------------------
plot(m3)

## ------------------------------------------------------------------------
dat$treat2 <- combineLevels(dat$treat, c("cbt", "ft"), newLabel = "cbtorft")

m4 <- lm(weight2 ~ weight1 * treat2, data = dat)
summary(m4)

## ------------------------------------------------------------------------
anova(m3, m4)

