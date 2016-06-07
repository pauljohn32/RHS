
library(foreign)
gpa <- read.dta("gpa.dta12")



##reshape(gpa, paste0("job", 1:6), "job", direction = "long")

gpa2 <- reshape(gpa, varying= list(paste0("job", 1:6), paste0("gpa", 1:6)),
                c("job", "gpa"),  direction = "long")

View(gpa2)

lm1 <- lm(gpa ~ job + time + highgpa, data = gpa2)

library(rockchalk)
plotSlopes(lm1, plotx = "time", modx = "job")



##


library(lme4)
m2 <- lmer(gpa ~ job + time + highgpa + (1|student), data = gpa2)
summary(m2)

## library(rockchalk)
## plotSlopes(m2, plotx = "time", modx = "job")

