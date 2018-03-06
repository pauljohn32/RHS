library(foreign)
library(lme4)
##library(lmerTest)
##library(plyr)


if (!file.exists("neighborhood.dta12")){
    download.file("http://www.stata-press.com/data/mlmus3/neighborhood.dta",
                  destfile = "neighborhood.dta12")
}
dat <- read.dta("neighborhood.dta12")

#1
m1 <- lmer(attain ~ (1 | neighid), data = dat, REML = FALSE)
summary(m1)

##' Calculate the ICC (Intragroup Correlation Coefficient)
##'
##' Only tricky part is retrieving parts from the fitted lmer object
##'
##' Could redesign this so that the function retrieves the name of
##' the grouping variable.
##' 
##' @param obj Fitted lme4 object
##' @param var A character string with the name of the grouping variable.
##' @return The value of the ICC, psi^2/(theta^2 + psi^2)
##' @author Paul Johnson <pauljohn@@ku.edu>
ICC <- function(obj, varname){
    ## Retrieve variance component psi
    psi_hat <- unname(attr(VarCorr(obj)[[varname]], "stddev"))
    ## Std of residual error is theta
    theta_hat<- attr(VarCorr(obj), "sc")
    psi_hat^2/(theta_hat^2 + psi_hat^2)
}

ICC(m1, "neighid")

#2
m2 <- lmer(attain ~ deprive + (1 | neighid), data = dat, REML = FALSE)
summary(m2)
#SD of random intercept has decreased, compared to M1
# level-1 resid is more or less unchanged. 
# one unit increase in deprivation associated with -.5 decrease in attain
ICC(m2, "neighid")


#3
m3 <- lmer(attain ~ p7vrq + p7read + dadunemp + daded + dadocc + momed +
               male + deprive + (1 | neighid), data = dat, REML = FALSE)
summary(m3)

#4

##' Calculates one variant of an R square for MLM
##'
##' This code was created by a student.
##'
##' Appears to try to calculate the Snidjers and Bosker proposal, a comparison of
##' the null model and the final fitted model estimates.
##'
##' The formula for this is discussed in RHS p. 135. It answers the
##' question "how much less unexplained variance is there in the fitted model (obj1)
##' than in the empty model?"
##' @param obj0 fitted lme4 object, no predictors
##' @param obj1 fitted lme4 object, including predictors
##' @param varname character string for name of grouping variable
##' @return estimate of rsquare
##' @author Paul Johnson <pauljohn@@ku.edu>
Rsq <- function(obj0, obj1, varname){
    if (! varname %in% attr(VarCorr(obj0), "names")){
        MESSG <- paste0("Grouping variable: \"", varname, "\" omitted from model 0")
        stop (MESSG)
    }
    psi_hat0 <- unname(attr(VarCorr(obj0)[[varname]], "stddev"))
    theta_hat0<- attr(VarCorr(obj0), "sc")
    psi_hat1 <- unname(attr(VarCorr(obj1)[[varname]], "stddev"))
    theta_hat1<- attr(VarCorr(obj1), "sc")
    rsq <- ((psi_hat0^2 + theta_hat0^2) - (psi_hat1^2 + theta_hat1^2))/(psi_hat0^2 + theta_hat0^2)
    rsq
}

Rsq(m1, m3, "neighid")
