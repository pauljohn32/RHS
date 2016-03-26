## Paul Johnson
## 20160326

library(foreign)
if(!file.exists("womenlf.rds")){
    download.file("http://www.stata-press.com/data/mlmus3/womenlf.dta", destfile = "womenlf.dta")
    dat <- read.dta("womenlf.dta")
    saveRDS(dat, file = "womenlf.rds")
} else {
    dat <- readRDS("womenlf.rds")
}


library(rockchalk)
table(dat$workstat)

dat$workstat.old <- dat$workstat
dat$workstat <- combineLevels(dat$workstat, levs = c("parttime", "fulltime"), newLabel = "employed")
table(dat$workstat, dat$workstat.old)

m1 <- glm(workstat ~ husbinc + chilpres, data = dat, family = binomial(link = "logit"))

summary(m1)

outreg(list("Logistic Model 1" = m1))

plotCurves(m1, plotx = "husbinc", modx = "chilpres")
