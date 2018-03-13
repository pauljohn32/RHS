##PJ>
##2018-03-13


library(foreign)
library(lme4)
library(multcomp)

fn <- "gcse"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

gcse <- read.dta("gcse.dta12")
gcse$genderf <- factor(gcse$girl, levels = c(0, 1), labels = c("M", "F"))
gcse$schgendf <- factor(gcse$schgend, levels = c(1, 2,3), labels = c("coed", "allM", "allF"))
gcse$boyschool <- ifelse(gcse$schgendf == "allM", TRUE, FALSE)
gcse$girlschool <- ifelse(gcse$schgendf == "allF", TRUE, FALSE)
gcse$coedschool <- ifelse(gcse$schgendf == "coed", TRUE, FALSE)
# Overview

rockchalk::summarize(gcse)
str(gcse)

if(interactive())kutils::peek(gcse)

# Set the problem?

table(gcse$genderf, gcse$schgendf)
##    coed allM allF
##  M 1110  513    0
##  F 1059    0 1377



## empty model for fun
m1 <- lmer(gcse ~ lrt + (lrt | school), data = gcse, REML = FALSE)
summary(m1)

## 4.1.1 Fit model from p 212
m2 <- lmer(gcse ~ lrt*schgendf + (lrt | school), data = gcse, REML = FALSE)
summary(m2)

#a. 
## H0 = Beta(girl_school) - (Beta(girlsi) * (Beta(mixed_schl)) = 0

#b.
## H0 = Beta(boy_schl) - (Beta(boysi) * (Beta(mixed_schl)) = 0

#matrix is rank deficient
m3 <- lmer(gcse ~ schgendf*lrt + genderf*schgendf + (lrt | school), data = gcse, REML = FALSE)
summary(m3)

## Difficult to fit without commiting some sins
gcse$boyincoed <- factor(ifelse(gcse$genderf=="M" & gcse$schgendf == "coed", TRUE, FALSE))
gcse$girlincoed <- factor(ifelse(gcse$genderf=="F" & gcse$schgendf == "coed", TRUE, FALSE))

gcse$genderspecial <- ifelse(gcse$schgendf == "allM", "b_allM",
                      ifelse(gcse$schgendf == "allF", "g_allF",
                      ifelse(gcse$schgendf == "coed" & gcse$genderf == "M", "b_coed",
                             "g_coed")))
table(gcse$genderspecial)
gcse$genderspecial <- factor(gcse$genderspecial, levels = c("g_coed", "b_coed", "b_allM", "g_allF"))


m4 <- lmer(gcse ~ 0 +  genderspecial + lrt*schgendf + (lrt|school), data = gcse, REML = FALSE)
summary(m4)


library(multcomp)
summary(glht(m4, linfct=c("genderspecialb_allM - genderspecialb_coed = 0")))

summary(glht(m4, linfct=c("genderspecialg_allF - genderspecialg_coed = 0")))
