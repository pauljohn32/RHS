

library(foreign)
library(lme4)
library(plm)


dat <- read.dta("papke_did.dta12")

str(dat)
rockchalk::summarize(dat)


## 1. Difference of means among cities in 1984
## To match stata, turn of Welch's correction'
with(dat[dat$year==1984, ], t.test(luclms ~ ez, var.equal = TRUE))

##
m1 <- lm(luclms ~ ez, data = dat[dat$year==1984, ])
summary(m1)


## 2. Difference between 1983 and 1984 among treatment cities

treated <- tapply(dat$ez, list(dat$city), function(x) any(x > 0))
table(treated)

dat[ , "treated"] <- treated[dat$city]
head(dat, 30)
## Observe the treated subset
dat[dat$treated, ]

with(dat[dat$treated, ], t.test(luclms ~ ez, var.equal = TRUE))

## Don't know where I'm wrong on that one, compared to MLMUS answer
## I get same mean, but R's t.test output is so sparse I can't see what else
## is wrong.
## Went on a little goose chase, stopped.
## ttest2 <- with(dat[dat$treated, ], t.test(luclms ~ as.factor(year), var.equal = TRUE))

m2 <- lm(luclms ~ ez + as.factor(city), data = dat[dat$treated, ])
summary(m2)



## 3
