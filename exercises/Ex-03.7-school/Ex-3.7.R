library(foreign)
library(lme4)
library(lmerTest)
library(plyr)

hsb <- read.dta("hsb.dta12")
hsb$schoolidf <- factor(dat1$schoolid)



## Variable "mmses" is the mean of ses, within schools, which we verify here
ses.agg.mean <- aggregate(hsb$ses,
                          by = list("schoolidf" = hsb$schoolidf),  mean, na.rm = TRUE)

crap2 <- tapply(hsb$ses, hsb$schoolidf, mean, na.rm = TRUE)
guess1 <- merge(hsb, ses.agg.mean, by = "schoolidf")


## Map that back onto the data frame for comparison (2 steps)
rownames(ses.agg.mean) <- ses.agg.mean$schoolidf
hsb$mmses.pj <- ses.agg.mean$x[hsb$schoolidf]
hsb$ses.group.diff <- hsb$ses - hsb$mmses.pj


library(data.table)
hsbdt <- as.data.table(hsb)
setkey(hsbdt, schoolidf)
hsbdt[ , sesmean2:=mean(ses), by = schoolidf]


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
