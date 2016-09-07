library(reshape)
library(foreign)
#gpa <- read.dta("gpa.dta12")
gpa <- read.dta("http://www.stata-press.com/data/mlmus3/gpa.dta")

##reshape(gpa, paste0("job", 1:6), "job", direction = "long")

gpa2 <- reshape(gpa, varying= list(paste0("job", 1:6), paste0("gpa", 1:6)),
                c("job", "gpa"),  direction = "long")

View(gpa2)

lm1 <- lm(gpa ~ job + time + highgpa, data = gpa2)

library(rockchalk)
plotSlopes(lm1, plotx = "time", modx = "job")

##
#2

library(lme4)
m2 <- lmer(gpa ~ job + time + highgpa + (1|student), data = gpa2)
summary(m2)

resid.m2 <- residuals(m2, type = "response")

gpa_new <- gpa2[, c("job", "time", "highgpa")]

gpa_new$pred <- predict(m2, re.form=NA, newdata=gpa_new, type = "response")

#3 - fairly certain that linearity holds here. Not sure if I did this quite right. 
par(mfrow=c(2,2))
plot(resid.m2 ~ gpa2$job, xlab = "Job",ylab = "Residual", cex.axis = .75) 
plot(resid.m2 ~ gpa2$time, xlab = "Time", ylab = "Residual", cex.axis = .75)
plot(resid.m2 ~ gpa2$highgpa, xlab = "HS GPA",ylab = "Residual", cex.axis = .75)
plot(gpa_new$pred ~ resid.m2, xlab = "Residual", ylab = "Pred. Val", cex.axis = .75)
## library(rockchalk)
#plotSlopes(m2, plotx = "time", modx = "job")

#4. 
m3 <- lmer(gpa ~ job + time + highgpa + job:time + highgpa:time + (1|student), data = gpa2)
summary(m3)
#No sig. interactions. 

#5
re <- ranef(m3)
raneffs <- unlist(unname(re[[1]]))
xx <- seq(min(raneffs), max(raneffs), length.out = 200)
yy <- dnorm(xx, mean = mean(raneffs), sd = sd(raneffs))

hist(raneffs, col = "grey", prob = TRUE, xlim = c(min(raneffs), max(raneffs)), main = "Histogram for Random Effects")
lines(density(raneffs), col = "red")
lines(xx, yy, col = "blue")
legend(.30, 1.5, c("Density Line for Model", "Density Line for N(mu, sigma)"), lty=c(1,1), col = c("red", "blue"), cex = .50, bty = 'n') 
#Fairly normal distribution of EB estimates for the random intercepts. 