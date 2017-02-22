## Paul Johnson
## 20170222
## Eddie Liebman
library(foreign)
library(lme4)
library(plm)


if (file.exists("pups.dta12")){
    dat <- read.dta("pups.dta12")
} else {
    url <- "http://www.stata-press.com/data/mlmus3/pups.dta"
    dat <- read.dta(url)
    write.dta(dat, file = "pups.dta12")
    ## Possibly also
    ## saveRDS(pups, "pups.rds")
}

## Make factors out of variables 1 through 4. Should I rename
## them with f suffix?
for (i in colnames(dat)[1:4]){
    dat[ , paste0(i, "f")] <- factor(dat[ , i])
}

#1
##size <- plyr::count(dat$dam)[, "freq"]

size <- table(dat$damf)
dat$size <- as.vector(size[dat$damf])
## Need as.vector because otherwise this is a table added into data frame.

        
#2 
##' Generate group summaries and individual deviations within groups
##'
##' Similar to Stata egen, except more versatile and fun! Will
##' create a new data frame with 2 columns. Rows match
##' the rows of the original data frame. 
##'
##' Now I return only the new columns
##' @param dframe a data frame
##' @param x Variable names or a vector of variable names
##' @param grp A grouping variable name or a vector of grouping names
##' @param FUN Defaults to the mean, have not tested alternatives
##' @param suffix The suffixes to be added to column 1 and column 2
##' @return new data frame with "x_mn" and "x_dev" added as variables
##' @author Paul Johnson
##' @examples
##' Suppose you get the MLMUS hsb data frame, somehow
##' xx1 <- egen_level_2(hsb, "ses", "schoolidf")
##' xx2 <- egen_level_2(hsb, c("ses", "female"), "schoolidf")
##' xx3 <- egen_level_2(hsb, c("ses", "female"), c("schoolidf", "sector"))
##' xx4 <- egen_level_2(hsb, c("ses", "female"),
##'                    c("schoolidf"), FUN = median)
egen_level_2 <- function(dframe, x, grp, FUN = mean, suffix = c("_mn", "_dev")){
    xmean <- aggregate(dframe[ , x, drop = FALSE],
                       dframe[ , grp, drop = FALSE], FUN,
                       na.rm = TRUE)
    df2 <- merge(dframe, xmean, by = grp, suffix = c("", suffix[1]))
    for(i in x){
        df2[ , paste0(i, suffix[2])] <- df2[ , i] - df2[ , paste0(i, suffix[1])]
    }
    df2[ , colnames(df2)[!colnames(df2) %in% colnames(dframe)]]
}

dat <- cbind(dat, egen_level_2(dat, "w", "dam"))
head(dat)


#3
plot(w_mn ~ size, data = dat[dat$dosef == "0", ],
     main = "Mean Weight by Size for Each Dose Group",
     pch = 2, ylab = "mean weight")
points(w_mn ~ size, data = dat[dat$dosef == "1", ], pch = 3, col = "blue")
points(w_mn ~ size, data = dat[dat$dosef == "2", ], pch = 6, col = "red")
legend(15,7.25, c("control", "low", "high"), pch =c(2,3,6), col = c("black","blue","red"), cex = .65, bty = 'n')

## I'd do that in one step, usually
plot(w_mn ~ size, data = dat,
     main = "Mean Weight by Size for Each Dose Group",
     pch = c(2, 3, 6)[dat$dosef],
     col = c("black", "blue", "red")[dat$dosef],
     ylab = "mean weight")



#4
m1 <- lmer(w ~ sex + dosef + size + (1 | dam), data = dat, REML = FALSE)
summary(m1)

#5
l1.resid <- residuals(m1, type = "response")
l2.resid <- unlist(unname(ranef(m1)[[1]][1]))

##l1 <- seq(min(l1.resid), max(l1.resid), length.out = 200)
library(rockchalk)
l1 <- plotSeq(l1.resid, length.out = 200)
l1.d <- dnorm(l1, mean = mean(l1.resid), sd = sd(l1.resid))
## l2 <- seq(min(l2.resid), max(l2.resid), length.out = 200)
l2 <- plotSeq(l2.resid, length.out = 200)
l2.d <- dnorm(l2, mean = mean(l2.resid), sd = sd(l2.resid))

dev.new(width=8, height=6)
par(mfrow=c(1,2))
hist(l1.resid, main = "Level 1 Residual", freq = FALSE, ylim = c(0, 1.5))
lines(density(l1.resid), col = "red")
lines(l1, l1.d, col = "blue")
legend(-3.5, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')

hist(l2.resid, main = "Level 2 Residual", freq = FALSE, ylim = c(0, 1.5), xlim =c(-1,1))
lines(density(l2.resid), col = "red")
lines(l2, l2.d, col = "blue")
legend(-1, 1, c("density of resid", "density of N(mu, sigma)"), lty=c(1,1), col=c("red", "blue"), cex = .33, bty = 'n')



## MLE versus REML

m1.reml <- lmer(w ~ sex + dosef + size + (1 | dam), data = dat)
summary(m1.reml)

outreg(list("mle" = m1, "reml" = m1.reml), type = "html")



## Fixed effects

m1.fe1 <- lm(w ~ sexf + dosef + size + damf, data = dat)
summary(m1.fe1)


m1.fe2 <- lm(w ~ 0 + sexf + dosef + size + damf, data = dat)
summary(m1.fe2)

library(plm)

m1.feplm <- plm(w ~ sexf + dosef + size, data = dat,
                index = c("damf"), model = "within")

