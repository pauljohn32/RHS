library(foreign)
library(reshape2)
library(lme4)
library(lmerTest)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/pefr.dta")

# EX in chapter 2
plot(wp1 ~ id, data = dat, ylim = c(200,700))
points(wp2 ~ id, data = dat, col = "red")
abline(h = (mean(dat$wp1)+mean(dat$wp2))/2)

dat$mean_wm <- rowMeans(dat[, 4:5])
datm <- melt(dat, id.vars = "id")
datm$occasion <- ifelse(datm$variable == "wp1" | datm$variable == "wm1", 1, 2)

m1 <- lmer(value ~ (1|id), data = datm[grep("wm", datm$variable), ], REML = FALSE)#ex 1 (p. 87)
summary(m1)
confint(m1) 
ran_m1 <- ranef(m1, condVar = T)#empirical bayes est. of random intercept, p. 109-113
se <- sqrt(attr(ran_m1[[1]], which = "postVar"))#Comparative standard error, p. 113-114


#ex. 2.1
# 1
m2 <- lmer(value ~ (1|id), data = datm[grep("wp", datm$variable), ], REML = FALSE)#1
summary(m1); summary(m2)#2

m1_r <- ranef(m1, condVar = TRUE)
m2_r <- ranef(m2, condVar = TRUE)

dfplot <- data.frame(
id = c(1:17),
ran1 = m1_r[[1]][,1],
var1 = rep(attr(m1_r[[1]], "postVar")[1], 17),
ran2 = m2_r[[1]][,1],
var2 = rep(attr(m2_r[[1]], "postVar")[1], 17)
)

#3 & #4
plot(ran1 ~ id, data = dfplot, col = "black", ylim = c(-400, 300), main = "Empicial Bayes Preds. for both models", xlab = "ID", ylab = "Random Effect - Intercept", xaxt = 'n')
axis(side=1, at=1:17, labels=dfplot$id,cex.axis=0.50)
legend(1, -240, c("Wright", "Mini Wright"), pch = 1, col = c("black", "red"), bty = 'n')
points(ran2 ~ id, data = dfplot, col = "red")
arrows(dfplot[, 1], dfplot[, 2] - dfplot[, 3], dfplot[, 1], dfplot[, 2] + dfplot[, 3], length=0.05, angle=90, code=3, col = "black")
arrows(dfplot[, 1], dfplot[, 4] - dfplot[, 5], dfplot[, 1], dfplot[, 4] + dfplot[, 5], length=0.05, angle=90, code=3, col = "red")

