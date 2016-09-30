library(foreign)
library(reshape)
library(plm)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/taxprep.dta") 
dat <- dat[,-c(5,6)]

#1
m1 <- lmer(lntax ~ time + prep + ms + hh + depend + age + lntpi + mr + emp + (1|subject), data = dat, REML = FALSE)
summary(m1)

#2 
mn_wi<- aggregate(dat[,-11], by = list(subject = dat$subject),mean, na.rm = TRUE) 
rownames(mn_wi) <- mn_wi$grp
colnames(mn_wi) <- c("subject", "ms_m", "hh_m", "depend_m", "age_m", "tpi_m", "txrt_m", "mr_m", "emp_m", "prep_m", "tax_m", "time_m", "lntpi_m", "lntax_m")
dat[,15:27] <- mn_wi[dat$subject, c("ms_m", "hh_m", "depend_m", "age_m", "tpi_m", "txrt_m", "mr_m", "emp_m", "prep_m", "tax_m", "time_m", "lntpi_m", "lntax_m")]

#could not think of a clever way to 
#within-subj 
dat$ms_dev <- dat$ms - dat$ms_m
dat$hh_dev <- dat$hh - dat$hh_m
dat$depend_dev <- dat$depend - dat$depend_m
dat$age_dev <- dat$age - dat$age_m
dat$tpi_dev <- dat$tpi - dat$tpi_m
dat$txrt_dev <- dat$txrt - dat$txrt_m
dat$mr_dev <- dat$mr - dat$mr_m
dat$emp_dev <- dat$emp - dat$emp_m
dat$prep_dev <- dat$prep - dat$prep_m
dat$tax_dev <- dat$tax - dat$tax_m
dat$time_dev <- dat$time - dat$time_m
dat$lntpi_dev <- dat$lntpi - dat$lntpi_m
dat$lntax_dev <- dat$lntax - dat$lntax_m

m2_w <- lm(lntax_dev ~ time_dev + prep_dev + ms_dev + hh_dev + depend_dev + age_dev + lntpi_dev + mr_dev + emp_dev, data = dat)
summary(m2_w)

m2_m <- lm(lntax_m ~ time_m + prep_m + ms_m + hh_m + depend_m + age_m + lntpi_m + mr_m + emp_m , data = dat)
summary(m2_m)

#plm_w <- plm(lntax ~ time + prep + ms + hh + depend + age + lntpi + mr + emp, data = dat, effect = "time", model = "between", index = "subject")
#summary(plm_w)

#plm_m <- plm(lntax ~ prep + ms + hh + depend + age + lntpi + mr + emp, data = dat, effect = "time", model = "between", index = "subject")
#summary(plm_m)

#There is a Hausman test option in PLM
#but, I'm getting results for my models that are pretty
#discrepant from Stata and lme4, so I'm not sure
#I am understanding the syntax. 

#3
l1.resid <- residuals(m1, type = "response")
l2.resid <- unlist(unname(ranef(m1)[[1]][1]))

l1 <- seq(min(l1.resid), max(l1.resid), length.out = 200)
l1.d <- dnorm(l1, mean = mean(l1.resid), sd = sd(l1.resid))
l2 <- seq(min(l2.resid), max(l2.resid), length.out = 200)
l2.d <- dnorm(l2, mean = mean(l2.resid), sd = sd(l2.resid))

dev.new(width=8, height=6)
par(mfrow=c(1,2))
hist(l1.resid, main = "Level 1 Residual", freq = FALSE, xlim = c(min(l1.resid), max(l1.resid)))
lines(density(l1.resid), col = "red")
lines(l1, l1.d, col = "blue")
legend(-3.5, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')

hist(l2.resid, main = "Level 2 Residual", freq = FALSE, xlim = c(min(l2.resid),max(l2.resid)))
lines(density(l2.resid), col = "red")
lines(l2, l2.d, col = "blue")
legend(-1, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')