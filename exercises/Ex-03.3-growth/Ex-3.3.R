library(reshape)
library(foreign)
library(lme4)

grow <- read.dta("http://www.stata-press.com/data/mlmus3/growth.dta")
grow$sex <- factor(grow$sex)

#1
plot(measure ~ age, data = grow[grow$sex == "1", ], type = 'l', col = "blue", main = "Grow Trajectories by Gender")
lines(measure ~ age, data = grow[grow$sex == "2", ], col = "green")
legend(8, 30, c("boys", "girls"), lty =c(1,1), col = c("blue", "green"), cex = .60, bty = 'n')

#2 
m1 <- lmer(measure ~ age + sex + (1 | idnr), data = grow, REML = FALSE)
summary(m1)


#3
m2 <- lmer(measure ~ age + sex + age:sex + (1 | idnr), data = grow, REML = FALSE)
summary(m2)

#4
#I know this isn't the most efficient way to do this.###
# Could not think of a better way at the moment.###### 
# Quite ugly plot, as well. ############
m1_b <- lmer(measure ~ age + (1 | idnr), data = grow[grow$sex =="1", ], REML = FALSE)
m1_g <- lmer(measure ~ age + (1 | idnr), data = grow[grow$sex =="2", ], REML = FALSE)

ab_m1 <- unname(coef(summary(m1_b))[ , "Estimate"][1])
bb1_m1 <- unname(coef(summary(m1_b))[ , "Estimate"][2])
ag_m1 <- unname(coef(summary(m1_g))[ , "Estimate"][1])
bg1_m1 <- unname(coef(summary(m1_g))[ , "Estimate"][2])

plot(measure ~ age, data = grow[grow$sex == "1", ], type = 'l', col = "grey", main = "Grow Trajectories by Gender", ylim = c(15, 35))
lines(measure ~ age, data = grow[grow$sex == "2", ], col = "pink")
legend(8, 32, c("boys", "girls","boys", "girls"), lty =c(1,1,3,4), lwd=c(1,1,3,3), col = c("grey", "pink", "black", "black"), cex = .60, bty = 'n')
abline(a = ab_m1, b = bb1_m1, lty = 3, lwd = 3)
abline(a = ag_m1, b = bg1_m1, lty = 4, lwd = 3)
