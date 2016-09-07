library(reshape)
library(foreign)
library(lme4)
library(plyr)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/pups.dta")
dat[,2:4] <- lapply(dat[,2:4], factor)

#1
size <- count(dat$dam)[,2]
dat$size <- rep(size, size)

#2
mnw <- aggregate(dat[c("dam", "w")], list(dat$dam), FUN = "mean")[, 3]
dat$mnw <- rep(mnw, size)

#3
plot(mnw ~ size, data = dat[dat$dose == "0", ], main = "Mean Weight by Size for Each Dose Group", pch = 2, ylab = "mean weight")
points(mnw ~ size, data = dat[dat$dose == "1", ], pch = 3, col = "blue")
points(mnw ~ size, data = dat[dat$dose == "2", ], pch = 6, col = "red")
legend(15,7.25, c("control", "low", "high"), pch =c(2,3,6), col = c("black","blue","red"), cex = .65, bty = 'n')

#4
m1 <- lmer(w ~ sex + dose + size + (1 | dam), data = dat, REML = FALSE)
summary(m1)

#5
l1.resid <- residuals(m1, type = "response")
l2.resid <- unlist(unname(ranef(m1)[[1]][1]))

l1 <- seq(min(l1.resid), max(l1.resid), length.out = 200)
l1.d <- dnorm(l1, mean = mean(l1.resid), sd = sd(l1.resid))
l2 <- seq(min(l2.resid), max(l2.resid), length.out = 200)
l2.d <- dnorm(l2, mean = mean(l2.resid), sd = sd(l2.resid))

dev.new(width=8, height=6)
par(mfrow=c(1,2))
hist(l1.resid, main = "Level 1 Residual", freq = FALSE, ylim = c(0, 1.5))
lines(density(l1.resid), col = "red")
lines(l1, l1.d, col = "blue")
legend(-3.5, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')

hist(l2.resid, main = "Level 2 Residual", freq = FALSE, ylim = c(0, 1.5), xlim =c(-1,1))
lines(density(l2.resid), col = "red")
lines(l2, l2.d, col = "blue")
legend(-1, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')