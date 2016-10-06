library(foreign)
library(lme4)
library(car)
library(nlme)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/wagepan.dta")

#1
m1 <- glmer(union ~ educ + black + hisp + exper + married 
            + rur + nrtheast + nrthcen + south + (1 | nr), 
            family = binomial(link = "logit"), nAGQ = 30, data = dat)
summary(m1)# failed to converge

#2
cc <- confint(m1, parm = "beta_")#error in calculating CIs
tab <- cbind(estimate = fixef(m1), cc)
etab <- exp(tab)

exp(fixef(m1))

9.107/(9.107 + (pi^2/3))

#3
library(gee)

m2 <- gee(union ~ educ + black + hisp + exper + married +
  rur + nrtheast + nrthcen + south,
  nr,
  dat,
  corstr = "exchangeable",
  family = binomial(link = "logit")
  )
summary(m2)

exp(coef(m2))

#4
ov_all <- as.data.frame(sapply(dat[,c("union", "educ", "black", "hisp", "exper", "married", "rur", "nrtheast", "nrthcen", "south")], mean))
colnames(ov_all)[1] <- "mean"
clus_mn <- aggregate(dat[,c("union", "educ", "black", "hisp", "exper", "married", "rur", "nrtheast", "nrthcen", "south")], list(nr = dat$nr),FUN ={mean(x, na.rm = T)})
clus_mn1 <- clus_mn[, -1]

#Beginning of an xtsum like function
#Got stuck on the w/i effects--complicated
# because of NAs in the data. 
bw_sd <- function(x, y){
  zz <- 1/(nrow(x)-1)
  bw_list <- list()
  for (i in names(x[,-1])){
    bw_list[[i]] <- sqrt(zz * sum((x[,i] - y[i, ])^2))
  }
  df <- data.frame(matrix(unlist(unique(bw_list)), nrow = nrow(y), byrow = T))
  colnames(df)[1] <- "BW SD"
  rownames(df) <- rownames(y)
  df$MN <- y[,1]
  return(df)
}

bw_sd(clus_mn, ov_all)

d.use <- dat[,c("nr", "union", "educ", "black", "hisp", "exper", "married", "rur", "nrtheast", "nrthcen", "south")]

#Found this function online. Works well. 
#http://www.willamette.edu/~rwalker/astat450/R/pan-summary-function.R

pan.btw.summary <- function(mydata, idim) {
# Summarize the grand mean and unit means
# Example invocation: pan.btw.summary(dframe, dframe$i)
mydata <- mydata
idim <- as.vector(idim)
identifier <- table(idim)
pan.cen.nls <- matrix(nrow=length(identifier), ncol=dim(mydata)[[2]])
for (i in 1:length(identifier)) {
  friend.nls <- NULL
  friend.big.nls <- NULL
  id.val <- as.numeric(names(identifier)[[i]])
  for (j in 1:dim(mydata)[[2]]) {
  friend.nls <- mean(mydata[idim==id.val,j], na.rm=TRUE)
  friend.big.nls <- c(friend.big.nls,friend.nls)
}
pan.cen.nls[i,] <- friend.big.nls
}
ret.sum <- apply(pan.cen.nls, 2, sd, na.rm=TRUE)
mean.mat <- pan.cen.nls
pan.sum <- list(summary.btw=ret.sum, mean.mat.btw=mean.mat)
return(pan.sum)
}

btw <- pan.btw.summary(d.use, d.use$nr)
btw[1]

pan.with.summary <- function(mydata, idim) {
# Within deviations
# Example invocation: pan.with.summary(dframe, dframe$i)
mydata <- mydata
idim <- as.vector(idim)
pan.mean <- matrix(nrow=dim(mydata)[[1]], ncol=dim(mydata)[[2]])
for (i in 1:dim(mydata)[[1]]) {
  friend.nls <- NULL
  friend.big.nls <- NULL
  id.val <- idim[[i]]
  for (j in 1:dim(mydata)[[2]]) {
  friend.nls <- mean(mydata[idim==id.val,j], na.rm=TRUE)
  friend.big.nls <- c(friend.big.nls,friend.nls)
}
pan.mean[i,] <- friend.big.nls
}
res.mat <- mydata - pan.mean
ret.sum <- apply(res.mat, 2, sd, na.rm=TRUE)
mean.mat <- res.mat
pan.sum <- list(summary.with=ret.sum, mean.mat.with=mean.mat)
return(pan.sum)
}
#http://www.willamette.edu/~rwalker/astat450/R/pan-summary-function.R

pan.with.summary(d.use, d.use$nr)[1]

#Fixed effect models are concerned within-cluster variance.
# A bunch of these covariates have no within cluster variation.
# These same variables have the largest SE in the GEE results. 

#6.
library(survival)
mc <- clogit(union ~ educ + black + hisp + exper + married +
  rur + nrtheast + nrthcen + south + strata(nr),
  data = dat)
summary(mc)
exp(coef(mc))
# estimates match
# stata
#clogit union educ black hisp exper married rur nrtheast nrthcen south, group(nr) or

#7. 
#The estimates are not especially different from
# the model in #1. 3 Covariates are omitted from 
# the conditional model because they are level-1 covs,
#i.e., they don't vary within clusters. 

#8.
# Still don't have a solution for the Hausman test in R

#9.
m1_prob <- glmer(union ~ educ + black + hisp + exper + married 
            + rur + nrtheast + nrthcen + south + (1 | nr), 
            family = binomial(link = "probit"), nAGQ = 30, data = dat)
summary(m1_prob)
#The latent response metric is not intuitive to me.
# OR are much nicer to deal with. 
#Especially given that predicted probabilities
#do not really differ unless who have a huge sample