## Paul Johnson

library(foreign)
library(lme4)

fn <- "hsb"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

hsb <- read.dta("hsb.dta12")
hsb$schoolidf <- as.factor(hsb$schoolid)

## I'm not interested in debating anymore how to create
## group-mean centered data

## ------------------------------------------------------------------------
##' Generate group summaries and individual deviations within groups
##'
##' Similar to Stata egen, except more versatile and fun! Will
##' create a new data frame with 2 columns. Rows match
##' the rows of the original data frame. 
##'
##' Now I return only the new columns
##' @param dframe a data frame
##' @param x Variable names or a vector of variable names
##' @param by A grouping variable name or a vector of grouping names
##' @param FUN Defaults to the mean, have not tested alternatives
##' @param suffix The suffixes to be added to column 1 and column 2
##' @return new data frame with "x_mn" and "x_dev" added as variables
##' @author Paul Johnson
##' @examples
##' Suppose you get the MLMUS hsb data frame, somehow
##' xx2 <- gmd(hsb, c("ses", "female"), "schoolidf")
##' xx3 <- gmd(hsb, c("ses", "female"), c("schoolidf", "sector"))
##' xx4 <- gmd(hsb, c("ses", "female"),
##'                    c("schoolidf"), FUN = median)
gmd <- function(dframe, x, by, FUN = mean, suffix = c("_mn", "_dev")){
    meanby <- aggregate(dframe[ , x, drop = FALSE],
                       dframe[ , by, drop = FALSE], FUN,
                       na.rm = TRUE)
    df2 <- merge(dframe, meanby, by = by, suffix = c("", suffix[1]), sort = FALSE)
    if(!all.equal(rownames(df2), rownames(dframe))){
        MESSG <- "rows were shuffled"
        stop(MESSG)
    }
    for(i in x){
        df2[ , paste0(i, suffix[2])] <- df2[ , i] - df2[ , paste0(i, suffix[1])]
    }
    ##gives back just the means and deviations part
    ##df2[ , colnames(df2)[!colnames(df2) %in% colnames(dframe)]]
    ##gives back whole data frame
    attr(df2, "meanby") <- meanby
    df2
}

hsb2 <- gmd(hsb, c("ses", "female"), by = "schoolidf")

head(hsb2)

## 3
m0 <- lmer(mathach ~ sector*ses_dev + ses_mn*ses_dev + (1|schoolid), data = hsb2,
           REML=FALSE)
summary(m0)

m1 <- lmer(mathach ~ sector*ses_dev + ses_mn*ses_dev + (ses_dev|schoolid), data = hsb2,
           REML=FALSE)
summary(m1)

## Recall sector had 0 = public 1 = Catholic.

## Interpret estimate of -1.64 for "sector:ses_dev".
## Among public schools, impact of within-school class variations is 2.9.
## Within catholic schools, the SES driven differences in score
## are much lower, they will be 2.9 - 1.64. In other words,
## wealth differences have a more dramatic impact if sector = 0.

## The coefficient estimated "ses_mn:ses_dev" appears to be nonsense
## to me. All interactions are difficult, but this is the most
## horrible because we are looking at the interaction with 2 variables
## constructed from the same variable. Literally, it says the effect
## of within class deviations in SES is greater when the mean school
## wealth is higher.  However, ses_mn * (ses - ses_mn) = ses_mn*ses
## + ses_mn^2. 


## 4 random effect
m1 <- lmer(mathach ~ sesmean + sector + sesdev + sesmean*sesdev +
         sector*sesdev + (sesdev|schoolid), data = hsb)

anova(m1, m0)
summary(m1)

library(rockchalk)
outreg(list("Random Int\n Only" = m0, "Random Slope" = m1), type = "html")

library(lattice)
dotplot(ranef(m1))
dotplot(ranef(m1, condVar=TRUE))

#3

m2 <- lmer(mathach ~ sesmean + sector + disclim + minority + sesdev + sesmean*sesdev +
         sector*sesdev +disclim*sesdev + sesmean*minority + sector*minority + disclim*minority + (1|schoolid), data = hsb)
summary(m2)
