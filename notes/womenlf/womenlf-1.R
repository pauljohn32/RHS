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


dat$workstatn <- ifelse(levels(dat$workstat)[dat$workstat] %in% "employed", 1, 0) 
plot(workstatn ~ husbinc, data = dat,  col = chilpres, ylim = c(0, 1.2))
pom <- predictOMatic(m1, predVals = list(husbinc = seq(0,50), chilpres = levels(dat$chilpres)))
lines(fit ~ husbinc, data = subset(pom, subset = chilpres == "absent"))
lines(fit ~ husbinc, data = subset(pom, subset = chilpres == "present"), lty = 2, col = 4)
legend("topright", c("absent", "present"), lty = c(1, 2), col = c(1, 4), title = "chilpres", horiz = TRUE)
