## ----import--------------------------------------------------------------
if (!file.exists("pefr.dta12")){
    download.file("http://www.stata-press.com/data/mlmus3/pefr.dta",
                  destfile = "pefr.dta12")
}
library(foreign)
pefr <- read.dta("pefr.dta12")

## ----recode--------------------------------------------------------------
rockchalk::summarize(pefr)
pefr$id.orig <- pefr$id
pefr$id <- as.factor(pefr$id)

## ------------------------------------------------------------------------
plot(wp1 ~ id.orig, data = pefr, main = "Wright Peak Flow for all Subjects")
points(wp2 ~ id, data = pefr, col = "red")
abline(h = (mean(pefr$wp1) + mean(pefr$wp2)) / 2)
legend("bottom", legend = c("pre", "post"), col = c("black", "red"),
       pch = c(1,1), horiz = TRUE)

## ------------------------------------------------------------------------
head(pefr)
pefrlong <- reshape(pefr, direction="long",
                    varying = list(c("wp1", "wp2"), c("wm1", "wm2")),
                    v.names = c("wp", "wm"),
                    timevar = "occasion")
head(pefrlong, 10)

## ----lmer10--------------------------------------------------------------
library(lme4)
m1 <- lmer(wp ~ (1|id), data = pefrlong, REML = FALSE)
summary(m1)

## ----confint-------------------------------------------------------------
confint(m1)

## ----mw10----------------------------------------------------------------
m2 <- lmer(wm ~ (1|id), data = pefrlong, REML = FALSE)
summary(m2)

## ----outreg10, results = "hide"------------------------------------------
library(rockchalk)
or <- outreg(list("Wright Peak flow" = m1, "Mini Wright meter" = m2))

## ----outreg20, results = "asis"------------------------------------------
or <- gsub("[ ]{2,}", " ", or)
cat(or)

## ----blups10-------------------------------------------------------------
m1.ranef <- ranef(m1, condVar = TRUE)
m1.ranef

## ----dotplot10-----------------------------------------------------------
library(lattice)
dotplot(m1.ranef)

## ----blups11a------------------------------------------------------------
attributes(m1.ranef[["id"]])

## ----blups11b------------------------------------------------------------
m1.ranef.se <- sqrt( attr(m1.ranef[["id"]], "postVar")[1,1, ])
## Not easy. Get the 3-d array from attr(m1.ranef[["id"]], "postVar")
## Then snip off the part we need with [1, 1, ]
## Then take square root

## ----mw15----------------------------------------------------------------
m2 <- lmer(wm ~ (1|id), data = pefrlong, REML = FALSE)
summary(m2)
m2.ranef <- ranef(m2, condVar = TRUE)
m2.ranef.se <- sqrt(attr(m2.ranef[["id"]], "postVar")[1,1, ])

## ----mw20----------------------------------------------------------------
## Not too easy to get the random effects as vectors, by themselves
y <- m1.ranef[["id"]]$`(Intercept)`
x <- m2.ranef[["id"]]$`(Intercept)`
plot(y ~ x,
     ylim = c(-300, 200), xlab = "Mini Wright meter predictions",
     ylab = "Wright peak flow predictions")
myx <- lm(y ~ x)
abline(myx, lty = 2)
abline(a = 0, b = 1, lty = 3)
legend("bottom", legend = c("least squares line", "y=x line"),
       lty = c(2, 3), horiz=TRUE)

## ------------------------------------------------------------------------
m2.ranef.se

## ------------------------------------------------------------------------
m1.ranef.se

