library(foreign)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/homework.dta") 

#1
#E(Zeta_school1j|Xj) = 0
#E(Zeta_homework2j|Xj) = 0
#E(etaij | Xj, Zeta_school1j, Zeta_homework2j) = 0
# Psi = [var(zeta1_schlj|Xj)            cov(zeta1_shcl, zeta2_hwj|Xj)  
#       cov(zeta2_hwj, zeta1_shcl|Xj)   var(zeta2_hwj|Xj)]
#      
#, all covariances are symmetirical.

#2   
m1 <- lmer(math ~ homework + white + ratio + (homework | schid), data = dat, REML = FALSE)
summary(m1) 

#3
vc <- as.data.frame(VarCorr(m1))

var.y <- vc[1,4] + 2*(vc[3,4]*dat$homework) + vc[2,4]*dat$homework^2 + plot(var.y ~ dat$homework)
# variance is heteroskedastic. The J-shaped curve looks identical to the one on RHS 
summary(var.y)

#4
## This approach is from chap 4 course notes on BB. 
## Much easier for me to implement. Although, not quite what the 
# question asks for. 
mathij = Beta1 + Beta_star1*HWij + Beta_star2*White1ij + Beta_star3*ratioij + Beta2*HWij + Beta3*(HWij * meansesj) + zeta1j + zeta2j*HWij + rji

#5
m2 <- lmer(math ~ homework + white + ratio + meanses + homework * meanses + (homework | schid), data = dat, REML = FALSE)
summary(m2)