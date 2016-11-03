library(foreign)
library(lme4)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/police.dta")

dat_crime <- subset(dat, crime ==1)
dat_crime$white <- ifelse(dat_crime$eth=="3", 1,0)
dat_crime$black <- ifelse(dat_crime$eth=="1", 1,0)
dat_crime$hispanic <- ifelse(dat_crime$eth=="2", 1,0)

#1
m1 <- glm(stops ~ black + hispanic + offset(log(pop)), data = dat_crime, family =poisson(link="log"))
summary(m1)

exp(coef(m1))
exp(confint(m1))

clus1 <- cluster.bs.glm(m1, dat_crime, ~precinct, report = TRUE)
exp(clus1[[1]])

m2 <- glm(stops ~ black + hispanic + offset(log(arrests)), data = dat_crime, family =poisson(link="log"))
summary(m2)

exp(coef(m2))
exp(confint(m2))

clus2 <- cluster.bs.glm(m2, dat_crime, ~precinct, report = TRUE)
exp(clus2[[2]])

#2
m3 <- glm(stops ~ black + hispanic + prblack + prhisp + offset(log(arrests)), data = dat_crime, family =poisson(link="log"))
summary(m3)

exp(coef(m3))
exp(confint(m3))

clus3 <- cluster.bs.glm(m3, dat_crime, ~precinct, report = TRUE)
exp(clus3[[3]])

#3
m4 <- glm(stops ~ black + hispanic + factor(precinct) + offset(log(arrests)), data = dat_crime, family =poisson(link="log"))
summary(m4)

exp(coef(m4))
exp(confint(m4))

#coefs for black and hisp match stata
#ignore coefs for precincts.

#clus4 <- cluster.bs.glm(m4, dat_crime, ~precinct, report = TRUE)
#exp(clus4[[2]])

#4
m5 <- glmer(stops ~ black + hispanic + offset(log(arrests)) +prblack + prhisp + (1|precinct), data = dat_crime, family =poisson(link="log"))
summary(m5)

exp(fixef(m5))
exp(confint(m5))

#5
exp(sqrt(2*0.4254^2)*qnorm(0.75))

#6
#I think this model is specified correctly.
# Except, I am not getting a covariance between black and hispanic for the
#random effects. The loglik and parameter etimates are near identical to Stata.

#The model does not converge. I'm trying to see if trying different optimizers
#leads to different parameter estimates. 
m6_b <- glmer(stops ~ black + hispanic + offset(log(arrests)) +prblack + prhisp + (black|precinct) + (hispanic|precinct), data = dat_crime, family =poisson(link="log"),
control=glmerControl(optimizer="bobyqa"))
summary(m6_b)
exp(fixef(m6_b))
exp(confint(m6_b))

m6_n <- glmer(stops ~ black + hispanic + offset(log(arrests)) +prblack + prhisp + (black|precinct) + (hispanic|precinct), data = dat_crime, family =poisson(link="log"),
control=glmerControl(optimizer="Nelder_Mead"))
summary(m6_n)
exp(fixef(m6_n))
exp(confint(m6_n))

#Results are fairly similar, the differ most for the random intercepts. 

re6 <- ranef(m6_b, simplify = TRUE)

par(mfrow=c(1,2))
plot(re6[[1]][,2] ~ re6[[1]][,1], main = "black|precinct")
plot(re6[[1]][,4] ~ re6[[1]][,2], main = "hispanic|precinct")