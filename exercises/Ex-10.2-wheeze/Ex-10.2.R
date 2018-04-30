## Paul Johnson
## 20180424

library(foreign)
library(lme4)
library(data.table)

fn <- "wheeze"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

wheeze <- read.dta(paste0(fn, ".dta12"))

setDT(wheeze)
setkey(wheeze, id, age)
wheeze[ , ylag:=shift(y), by = list(id)]

wheeze <- as.data.frame(wheeze)

rockchalk::summarize(wheeze)

#1

m1 <- glm(y ~ age + smoking + ylag, family = binomial(link = "logit"),
          data = wheeze, subset= age > -2)
summary(m1)
exp(coef(m1))
exp(confint(m1))

nd <- rockchalk::newdata(m1, predVals = c("age", "smoking", "ylag"))
nd$m1.fit.lp <- predict(m1, newdata = nd)
nd$m1.fit.prob <- predict(m1, newdata = nd, type = "response")

## Approximate CIs
rockchalk:::predictCI(m1, newdata = nd)


#2
m2 <- glmer(y ~ age + smoking + (1|id), family = binomial(link =        
      "logit"), nAGQ = 30, data = wheeze)
summary(m2)
exp(fixef(m2))
exp(confint(m2))

exp(confint(m2, method = "Wald"))

#3
library(gee)

m3 <- gee(y ~ age + smoking, 
  id,
  wheeze,
  family = binomial(link = "logit")
  )
summary(m3)

m4 <- gee(y ~ age + smoking, 
  id,
  wheeze,
  family = binomial(link = "logit"),
  Mv = 1,
  corstr = "unstructured"
  )
summary(m4)

m5 <- gee(y ~ age + smoking, 
  id,
  wheeze,
  family = binomial(link = "logit"),
  Mv = 1,
  corstr = "AR-M"
  )
summary(m5)

exp(sapply(list(m3,m4,m5), coef))
