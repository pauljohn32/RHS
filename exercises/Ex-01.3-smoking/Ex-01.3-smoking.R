## ----import--------------------------------------------------------------
if (!file.exists("smoking.dta12")){
    dsname <- "http://www.stata-press.com/data/mlmus3/smoking.dta"
    download.file(dsname,
                  destfile = "smoking.dta12")
}
library(foreign)
dat <- read.dta("smoking.dta12")
rockchalk::summarize(dat)
dim(dat)
dat <- dat[dat$idx==1, ]
dim(dat)

## ----recode10------------------------------------------------------------
dat$education.num <- ifelse(dat$hsgrad == 1, 1,
                        ifelse(dat$somecoll == 1, 2,
                        ifelse(dat$collgrad == 1, 3, 0)))
dat$education <- factor(dat$education.num, levels = 0:3,
                        labels = c("none", "hs", "somecoll", "collgrad"))
table(dat$education, dat$education.num)

## ------------------------------------------------------------------------
m1 <- lm(birwt ~ education + male + black, data = dat)
summary(m1)
dat2 <- rockchalk::newdata(m1, list("education" = unique(dat$education), "male" = unique(dat$male), "black" = unique(dat$black)))

## ------------------------------------------------------------------------
splits <- split(dat, f = list(dat$male, dat$black, dat$smoke, dat$education), drop = TRUE)
reslts <- lapply(splits, function(ds){
    data.frame("male" = unique(ds$male), "black" = unique(ds$black),
               "smoke" = unique(ds$smoke), "education" = unique(ds$education),
               "N" = NROW(ds), 
               "mean" = mean(ds$birwt, na.rm=TRUE), "sd" = sd(ds$birwt, na.rm=TRUE))})
sumrytable <- do.call("rbind", reslts)
rownames(sumrytable) <- NULL
sumrytable

## ----boxplot-------------------------------------------------------------
library(lattice)
bwplot(birwt ~  education + black | smoke + male, data = dat)

## ----reg10---------------------------------------------------------------
m1 <- lm(birwt ~ smoke, data = dat)
summary(m1)

## ----reg20---------------------------------------------------------------
m2 <- lm(birwt ~ smoke + mage + male + black + education, data = dat)
summary(m2)

## ----reg30---------------------------------------------------------------
rockchalk::predictOMatic(m2, "margins", interval = "confidence")

## ----reg40---------------------------------------------------------------
rockchalk::predictOMatic(m2, "auto", interval = "confidence")

