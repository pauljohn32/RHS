library(foreign)
library(lme4)
library(lmerTest)
library(plyr)

hsb <- read.dta("hsb.dta12")
hsb$schoolidf <- factor(hsb$schoolid)

## Variable "mmses" is the mean of ses, within schools, which we verify here
## Create a vector of variable names for which we need school level means
vars <- c("ses", "female")
ses_mn <- aggregate(hsb[ , vars],
                          by = list("schoolidf" = hsb$schoolidf),  mean, na.rm = TRUE)

hsb2 <- merge(hsb, ses_mn, by = "schoolidf", suffix = c("", "_mn"))
for(i in vars){
    hsb2[ , paste0(i, "_dev")] <- hsb2[ , i] - hsb2[ , paste0(i, "_mn")]
}


## Another way to do same calculation
library(data.table)
hsbdt <- as.data.table(hsb)
setkey(hsbdt, schoolidf)
hsbdt[ , ses_mn2 := mean(ses), by = schoolidf]
hsbdt[ , ses_dev2 := ses - ses_mn2]



##' Generate group mean and individual deviations within groups
##'
##' Similar to Stata egen, except more versatile and fun! Will
##' create a new data frame that includes the new columns.
##'
##' I'm replacing the whole data frame here, I agree that's
##' wasteful, but it is also convenient.  Will reconsider
##' Just returning the new columns
##' @param dframe a data frame
##' @param x Variable names or a vector of variable names
##' @param grp A grouping variable name or a vector of grouping names
##' @param FUN Defaults to the mean, have not tested alternatives
##' @return new data frame with "x_mn" and "x_dev" added as variables
##' @author Paul Johnson
##' @examples
##' Suppose you get the MLMUS hsb data frame, somehow
##' xx1 <- egen_level_2(hsb, "ses", "schoolidf")
##' xx2 <- egen_level_2(hsb, c("ses", "female"), "schoolidf")
##' xx3 <- egen_level_2(hsb, c("ses", "female"), c("schoolidf", "sector"))
##' xx4 <- egen_level_2(hsb, c("ses", "female"),
##'                    c("schoolidf"), FUN = median)
egen_level_2 <- function(dframe, x, grp, FUN = mean){
    xmean <- aggregate(dframe[ , x, drop = FALSE],
                       dframe[ , grp, drop = FALSE], FUN,
                       na.rm = TRUE)
    df2 <- merge(dframe, xmean, by = grp, suffix = c("", "_mn"))
    for(i in x){
        df2[ , paste0(i, "_dev")] <- df2[ , i] - df2[ , paste0(i, "_mn")]
    }
    df2
}

xx1 <- egen_level_2(hsb, "ses", "schoolidf")
xx2 <- egen_level_2(hsb, c("ses", "female"), "schoolidf")
xx3 <- egen_level_2(hsb, c("ses", "female"), c("schoolidf", "sector"))
xx4 <- egen_level_2(hsb, c("ses", "female"),



                    c("schoolidf"), FUN = median)



for (i in c("ses", "female")){
    hsb[ , i] <- magicfunction_here_like_stata_egen( )
}




#1
m1_hs <- lmer(mathach ~ ses + (1 | schoolid), data = dat1, REML = FALSE)
summary(m1_hs)

#############################
#2 - overall mean of birthwt
overall_mean <- mean(dat1$ses)

#B/w cluster SD
means <- ddply(dat1, .(schoolid), summarize, mean=mean(ses))
sqrt(1/(length(levels(dat1$schoolid)) - 1) * sum((means[, 2] - overall_mean)^2))

#W/i cluster SD
clust.subj <- list()
for (i in levels(dat1$schoolid)){
  clust.subj[[i]] <- subset(dat1[, c("schoolid", "ses")], schoolid == i)
}
means.l <- lapply(means[, 2], list)
within.ss <- mapply('-', clust.subj, means.l, SIMPLIFY=FALSE)

ss <- c()
for (h in levels(dat1$schoolid)){
  within.ss[[h]][2] <- within.ss[[h]][2]^2
  ss[h] <- colSums(within.ss[[h]][2])
}

sqrt(1/(nrow(dat1)-1) * sum(ss))
##############################
#3
mean_ses <- means[, 2]
counts <- count(dat1$schoolid)[, 2]
dat1$mn_ses <- rep(mean_ses, counts)
dat1$dev_ses <- dat1$ses - dat1$mn_ses

###NEED PACKAGE: multcomp, for this#####
#4
library(multcomp)

m2_hs <- lmer(mathach ~ mn_ses + dev_ses + (1 | schoolid), data = dat1, REML = FALSE)
summary(m2_hs)
summary(glht(m2_hs, linfct = c("mn_ses - dev_ses = 0")))#Identical to Stata. 
#Two estimates are not the same.

#5
#A one unit increase in mean school SES is equal to a 5.9 increase in math ach.
#Two students in the same school will differ by 1 ses point will differ on average by 2.19 math ach points. 

#6
# Sig diff on Hausman test in Stata. Model mis-specified, RE not needed. 
# Not sure how to do this in R. 
