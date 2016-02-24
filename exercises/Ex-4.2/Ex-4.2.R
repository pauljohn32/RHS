library(foreign)

dat <- read.dta("hsb.dta12")
dat$schoolid <- as.character(dat$schoolid)

## Ways to create group level mean for ses
## I'll insert na.rm = TRUE to drop missings, if there are any
ses <- sapply(split(dat, dat$schoolid), function(x) mean(x$ses, na.rm = TRUE))
dat$sesmean <- ses[dat$schoolid]
dat$sesdev <- dat$ses - dat$sesmean

## Another equivalent:
ses2 <- tapply(dat$ses, list(dat$schoolid), mean, na.rm = TRUE)
dat$sesmean2 <- ses2[dat$schoolid]
dat$sesdev2 <- dat$ses - dat$sesmean2

## Aggregate creates a differently structured 2 column data frame
ses3 <- aggregate(dat$ses, by = list(schoolid = dat$schoolid), mean, na.rm = TRUE)
## Inspect ses3: How to join that back to dat? Various ideas, none smooth
rownames(ses3) <- ses3$schoolid
dat$sesmean3 <- ses3[dat$schoolid, "x"]
dat$sesdev3 <- dat$ses - dat$sesmean3
identical(dat$sesdev3, dat$sesdev)

## If this were truly huge data, I'd be inclined to use data.table
library(data.table)
DT <- as.data.table(dat)
DT[ , sesmean4:=mean(ses, na.rm = TRUE), by = schoolid]
DT[ , sesdev4:= ses - sesmean4]
## Sometimes unexpected things happen with DT's, so change back to
## data frame
dat <- as.data.frame(DT)


## Hadley Wickham's plyr package provides a number of idioms that
## have been very popular.  I always avoided using them because they
## were not "core R" idioms and there are special features there that
## do not travel to other parts of R.  However, there's no denying
## the popularity of it. 
## Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
##     Analysis. Journal of Statistical Software, 40(1), 1-29.  <
##     http://www.jstatsoft.org/v40/i01/>.
## Lots of people love it, but I just can't make it work on a daily
## basis.  I just can't think that way.


library(lme4)

m0 <- lmer(mathach ~ sesmean + sector + sesdev + sesmean*sesdev +
         sector*sesdev + (1|schoolid), data = dat)


m1 <- lmer(mathach ~ sesmean + sector + sesdev + sesmean*sesdev +
         sector*sesdev + (sesdev|schoolid), data = dat)

anova(m1, m0)

summary(m1)

library(lattice)
dotplot(ranef(m1))

dotplot(ranef(m1, condVar=TRUE))
