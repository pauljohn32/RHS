## Eddie Liebmann
## Paul Johnson
## 2018-03-06
library(foreign)
library(lme4)


if (!file.exists("growth.dta12")){
    download.file("http://www.stata-press.com/data/mlmus3/growth.dta",
                  destfile = "growth.dta12")
}
grow <- read.dta("growth.dta12")
grow$sex <- factor(grow$sex, levels = 1:2, labels = c("M", "F"))

#1
## See what's wrong in this plot?
plot(measure ~ age, data = grow[grow$sex == "M", ],
     type = 'l', col = "blue", main = "Grow Trajectories by Gender")
lines(measure ~ age, data = grow[grow$sex == "F", ], col = "green")
legend(8, 30, c("boys", "girls"), lty =c(1,1), col = c("blue", "green"),
       cex = .60, bty = 'n')

## Note: line wraps from right to left. How to correct that?
## Note the illusion that age has values 9, 11, 13. 
sexcol <- c("M" = "blue", "F" = "green")
plot(measure ~ age, data = grow, type = "n", 
     main = "Grow Trajectories by Gender", axes = FALSE)
box(bty = "L")
axis(2)
axis(1, at = sort(unique(grow$age)))
lineplot <- lapply(split(grow, f=grow$idnr), function(x) {
    lines(measure ~ age, data = x,  col = unique(sexcol[x$sex]))
})
legend("topleft", c("boys", "girls"), lty =c(1,1), col = sexcol,
       cex = .90, bty = 'n')

## Previous lapply same as
## plot(measure ~ age, data = grow, type = "n", 
##      main = "Grow Trajectories by Gender")
## growsplit <- split(grow, f=grow$idnr)
## for(i in seq_along(growsplit)){
##     x <- growsplit[[i]]
##     lines(measure ~ age, data = x,  col = unique(sexcol[x$sex]))
## }


## Instead, you want a trellis plot?
## The groups=idnr argument stops the line from wrapping
library(lattice)

xyplot(measure ~ age | sex, data = grow, groups = idnr,
       type = "l")

## Can we get rid of the tuity-frooty colors?
grow$sexcolor <- sexcol[grow$sex]
##
xyplot(measure ~ age | sex, data = grow, groups = idnr,
       type = "l", par.settings = list(col = "green"))


## Fiddled several ways, all are fails
sexcol <- c("red", "blue")
grow$sexcol <- sexcol[grow$sex]
xyplot(measure ~ age | sex, data = grow, groups = idnr, type = "l",
       panel = function(x, y, groups){
           panel.xyplot(x, y, type = "l")
       })
## Figure how to add lines one for each case within there




## Get free line breaks form ggplot2, but controlling
## color and display is an awful hassle
library(ggplot2)
theme_set(theme_bw())

ggplot(grow, aes(x = age, y = measure, group = idnr)) +
    geom_line(aes(group = idnr)) +
    facet_wrap( ~ sex)


## Can't get colors right
sexcol <- c("M" = "red", "F" = "blue")
grow$sexcol2 <- as.integer(grow$sex)
ggplot(grow, aes(x = age, y = measure, group = idnr, colour=sexcol)) +
    geom_line(aes(group = idnr, colour=sexcol2)) +
    facet_wrap( ~ sex)
## Note the colors are displayed badly.

#2 
m1 <- lmer(measure ~ age + sex + (1 | idnr), data = grow, REML = FALSE)
summary(m1)


#3
m2 <- lmer(measure ~ age*sex + (1 | idnr), data = grow, REML = FALSE)
summary(m2)

#4

## need new data for predicting
## Gives predicted values that include random effects
nd <- rockchalk::newdata(m2, predVals = "auto")
nd$measure <- predict(m2, newdata = nd, re.form = NULL)
nd

## Excludes random effects, for plotting group means
nd <- expand.grid(age = c(8, 10, 12, 14), sex = c("M", "F"))
nd$measure <- predict(m2, newdata = nd, re.form = ~0)
nd$idnr <- 0
nd

rockchalk::predictOMatic(m2)
rockchalk::predictOMatic(m2, predVals = "auto")

plot(measure ~ age, data = grow, type = "n", 
     main = "Grow Trajectories by Gender", axes = FALSE)
box(bty = "L")
sexcol3 = c("M" = "gray60", "F" = "pink")
axis(2)
axis(1, at = sort(unique(grow$age)))
lapply(split(grow, f=grow$idnr), function(x) {
    lines(measure ~ age, data = x, lwd=2, col = sexcol3[as.integer(x$sex)],
          lty = as.integer(x$sex))
})
sexcol4 = c("M" = "black", "F" = "black")
sexlty <- c("M" = 1, "F" = 2)
for(i in c("M", "F")){
    lines(measure ~ age, nd, subset = sex == i, col = sexcol4[i],
          lwd = 3, lty = sexlty[i])
}
legend("topleft", legend = c("Male", "Female"), title = "Predicted values",
       lty = sexlty, col =  sexcol4)

##
ggplot(grow, aes(x = age, y = measure, group = idnr)) +
    geom_line(aes(group = idnr, colour = "gray70")) +
    facet_wrap( ~ sex) +
    geom_line(data = nd, color = "black")



## This stanza fits separate models for boys and girls
## and plots them. This is obviously wrong, but one person
## suggested doing it this way. So we have the code for it.

m1_b <- lmer(measure ~ age + (1 | idnr), data = grow,
             subset = sex == "M",  REML = FALSE)
m1_g <- lmer(measure ~ age + (1 | idnr), data = grow,
             subset = sex == "F",  REML = FALSE)

m1_b_coef <- coef(summary(m1_b))[ , "Estimate"]
m1_g_coef <- coef(summary(m1_g))[ , "Estimate"]

plot(measure ~ age, data = grow, type = "n", 
     main = "Grow Trajectories by Gender")
sexcol = c("M" = "gray60", "F" = "pink")
lapply(split(grow, f=grow$idnr), function(x) {
    lines(measure ~ age, data = x, lwd=2, col = sexcol[x$sex],
          lty = as.integer(x$sex))
})
abline(m1_b_coef, lty = 1, lwd = 4, col = "black")
abline(m1_g_coef, lty = 2, lwd = 4, col = "black")
legend("topleft", c("boys", "girls", "pred(boys)", "pred(girls)"),
       lty = c(1,2,1,2), lwd = c(2,2,4,4),
       col = c(sexcol, "black", "black"), cex = .80, bty = 'n')

