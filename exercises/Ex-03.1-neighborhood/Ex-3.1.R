library(foreign)
##library(reshape2)
library(lme4)
library(lmerTest)
library(plyr)

##dat <- read.dta("http://www.stata-press.com/data/mlmus3/neighborhood.dta")

dat <- read.dta("neighborhood.dta12")

#1
m1 <- lmer(attain ~ (1 | neighid), data = dat, REML = FALSE)
summary(m1)

ICC <- function(x){
  psi_hat <- unname(attr(VarCorr(x)$neighid, "stddev"))
  theta_hat<- attr(VarCorr(x), "sc")
  print(psi_hat^2/(theta_hat^2 + psi_hat^2))
}

ICC(m1)

#2
m2 <- lmer(attain ~ deprive + (1 | neighid), data = dat, REML = FALSE)
summary(m2)
#SD of random intercept has decreased, compared to M1
# level-1 resid is more or less unchanged. 
# one unit increase in deprivation associated with -.5 decrease in attain

#3
m3 <- lmer(attain ~ p7vrq + p7read + dadunemp + daded + dadocc + momed + male + deprive + (1 | neighid), data = dat, REML = FALSE)
summary(m3)

#4
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param z 
##' @return 
##' @author Paul Johnson
Rsq <- function(x, z){
  psi_hat0 <- unname(attr(VarCorr(x)$neighid, "stddev"))
  theta_hat0<- attr(VarCorr(x), "sc")
  psi_hat1 <- unname(attr(VarCorr(z)$neighid, "stddev"))
  theta_hat1<- attr(VarCorr(z), "sc")
  rsq <- (psi_hat0 + theta_hat0) - (psi_hat1 + theta_hat1)/(psi_hat0 + theta_hat0)
  print(rsq)
}

Rsq(m1, m3)
