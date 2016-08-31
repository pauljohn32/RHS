library(foreign)
library(reshape2)
library(lme4)
library(lmerTest)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/birthwt.dta")
dat$mother <- factor(dat$mother)

m1 <- lmer(birthwt ~ (1|mother), data = dat, REML = FALSE)
summary(m1)
confint(m1)

#2 -- not sure how to do a likelihood ratio test in R against linear model, using LME4

#3
ICC <- function(x){
  psi_hat <- unname(attr(VarCorr(m1)$mother, "stddev"))
  theta_hat<- attr(VarCorr(x), "sc")
  print(psi_hat^2/(theta_hat^2 + psi_hat^2))
}

ICC(m1)#42% of variance is shared within siblings. 

#4 
m1.r <- ranef(m1)
hist(unname(m1.r[[1]]))