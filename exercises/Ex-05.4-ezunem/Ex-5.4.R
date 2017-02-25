library(lme4)
library(plm)
library(foreign)
library(nlme)


if (file.exists("ezunem.dta12")){
    dat <- read.dta("ezunem.dta12")
} else {
    url <- "http://www.stata-press.com/data/mlmus3/ezunem.dta"
    dat <- read.dta(url)
    write.dta(dat, file = "ezunem.dta12")
    ## Possibly also
    ## saveRDS(ezunem, "ezunem.rds")
}

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

dat$tf <- as.factor(dat$t)
dat$cityf <- as.factor(dat$city)

#1 NA

#2
#a
dat$uclms_l <- log(dat$uclms)

m1w <- plm(luclms ~ tf + ez, data = dat, index = c("city"), model = "within")
summary(m1w)

(m1w.fe <- fixef(m1w))
mean(m1w.fe)
sd(m1w.fe)
## Note that the mean of fixed effects equals the constant
## that Stata xtreg reports

dat <- cbind(dat, egen_level_2(dat, c("luclms", "ez"), "city"))

## Reproduces stata parameter estimates, but not standard
## errors because the number of parameters is not understood to lm
m1.pj1 <- lm(luclms_dev ~ tf + ez_dev, data = dat)
summary(m1.pj1)

m1.pj2 <- lm(luclms_dev ~ 0 + tf + ez_dev, data = dat)
summary(m1.pj2)

m1.pj3 <- lm(luclms ~ 0 + ez + cityf + tf , data = dat)
summary(m1.pj3)

(m1.pj3.coef <- coef(m1.pj3))
fixef <-m1.pj3.coef[grep("city", names(m1.pj3.coef), value = TRUE)]
mean(fixef)
sd(fixef)





## #b
## dat.1 <- pdata.frame(dat[,c("city", "t", "luclms", "d81", "d82", "d83", "d84", "d85", "d86", "d87", "d88", "ez")], index = c("city", "t"),
##              drop.index = TRUE, row.names = TRUE)

## mfd <- plm(luclms ~ d81 + d82 + d83 + d84 + d85 + d86 +d87 + d88 + ez, model = "fd", data = dat.1))
## coef(mfd)#matches stata
## summary(mfd)
## #Cannot get summary() to run. 
## #I gathered from Stackexchange that the problem
## #comes from the calculation of R2 and collinearity. 
## #Is there another summary function from another 
## #package?

## #b.i
## # EZ, which I think is the intervention, has a much a greater
## # mean difference in the f.d. model than the fe model. 
## #Although, I cannot see the S.E. for coefs, so I'm not sure
## #on the significance of the est.

## #b.ii
## luclms_ij - luclmsi_1,j = b2 * (x1_ij - x1_1,j) + e_ij - e_1,j
## #Solution has tau as constant, not sure why. Does this reprsent the intercept?

## #b.iii
## #Not sure about this one.

## #3
## #Not sure how to fit this model in R. Can do it with a random intercept.
## #PLM seems like it would be the most likely package
## # but, there does not seem to an option to have an AR coefficient.

## #4
## #m.ar1 <- lme(luclms ~ d81 + d82 + d83 + d84 + d85 + d86 +d87 + d88 + ez, 
## #  random = ~ 1 | city,
##  # correlation = corAR1(),
## #  method = "ML",
## #  data = dat
##  # )
## #summary(m.ar1)
## #This example needs work, I don't know how to fit this in R. 
