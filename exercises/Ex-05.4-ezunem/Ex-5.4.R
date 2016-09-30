library(lme4)
library(plm)
library(foreign)
library(nlme)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/ezunem.dta") 

#1 NA

#2
#a
dat$uclms_l <- log(dat$uclms)

m1w <- plm(luclms ~ d81 + d82 + d83 + d84 + d85 + d86 +d87 + d88 + ez, data = dat, index = c("city", "t"), model = "within") 
summary(m1w)
mean(fixef(m1w))

#b
dat.1 <- pdata.frame(dat[,c("city", "t", "luclms", "d81", "d82", "d83", "d84", "d85", "d86", "d87", "d88", "ez")], index = c("city", "t"),
             drop.index = TRUE, row.names = TRUE)

mfd <- plm(luclms ~ d81 + d82 + d83 + d84 + d85 + d86 +d87 + d88 + ez, model = "fd", data = dat.1))
coef(mfd)#matches stata
summary(mfd)
#Cannot get summary() to run. 
#I gathered from Stackexchange that the problem
#comes from the calculation of R2 and collinearity. 
#Is there another summary function from another 
#package?

#b.i
# EZ, which I think is the intervention, has a much a greater
# mean difference in the f.d. model than the fe model. 
#Although, I cannot see the S.E. for coefs, so I'm not sure
#on the significance of the est.

#b.ii
luclms_ij - luclmsi_1,j = b2 * (x1_ij - x1_1,j) + e_ij - e_1,j
#Solution has tau as constant, not sure why. Does this reprsent the intercept?

#b.iii
#Not sure about this one.

#3
#Not sure how to fit this model in R. Can do it with a random intercept.
#PLM seems like it would be the most likely package
# but, there does not seem to an option to have an AR coefficient.

#4
#m.ar1 <- lme(luclms ~ d81 + d82 + d83 + d84 + d85 + d86 +d87 + d88 + ez, 
#  random = ~ 1 | city,
 # correlation = corAR1(),
#  method = "ML",
#  data = dat
 # )
#summary(m.ar1)
#This example needs work, I don't know how to fit this in R. 
