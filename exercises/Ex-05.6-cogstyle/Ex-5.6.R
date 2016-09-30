library(lme4)
library(nlme)
library(reshape)
library(foreign)
library(ggplot2)
library(lmerTest)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/cogstyle.dta") 

#1 & 2
dat1 <- reshape(dat, 
  varying = c("rfn", "rfc", "rfi", "rcn", "rcc", "rci"),
  v.names = "time",
  timevar = "words",
  times = c("rfn", "rfc", "rfi", "rcn", "rcc", "rci"),
  direction = "long"
  )
  
dat1$words <- factor(dat1$words)
xx <- dat1$words
dat1$cue <- ifelse(grepl('^..i', xx)==TRUE, "i", ifelse(grepl('^..n', xx)==TRUE, "n", "c"))
dat1$form <- ifelse(grepl('^.f', xx)==TRUE, "f", "c")

dat1$t1 <- log(dat1$time-134)

#3
d <- ggplot(dat1, aes(x=factor(form), y=t1, group = interaction(form, cue), colour = factor(cue))) + geom_boxplot() 
d + facet_wrap(~ dependent)

#4
dat1[c("words", "cue", "dependent")] <- lapply(dat1[c("words", "cue", "dependent")], factor)

m1 <- lmer(t1 ~ words + cue+ dependent + words*cue + words*dependent + cue*dependent + cue*words*dependent+(1|subj), data = dat1, REML = FALSE)
summary(m1)#difficulty estimating, lots of rank deficiency. 

#n1 <- lme(t1 ~ words + cue+ dependent + words*cue + words*dependent + cue*dependent + cue*words*dependent, random = ~ 1|subj,method = "ML", data = dat1)
#summary(n1)
#This model does not run. 
#Can't do the testparm using multcomp, get error. 

m2 <- lmer(t1 ~ words + cue+ dependent + words*cue + words*dependent + cue*dependent +(1|subj), data = dat1, REML = FALSE)
summary(m2)#difficulty estimating, lots of rank deficiency. 

#Not quite sure how to proceed with this example
#given that so few of the parameters were estimated. 