library(foreign)
library(reshape2)
library(lme4)
library(lmerTest)

dat <- read.dta("http://www.stata-press.com/data/mlmus3/twin.dta")

df.exp <- dat[rep(row.names(dat), dat$num2), c(1:4, 3)]#1
df.exp$id <- 1:nrow(df.exp)#2

dat.m <- melt(df.exp, id.vars = c("num2", "id", "dzmz"))

#3/4
mz_1 <- lmer(value ~ (1 |id), data = dat.m[dat.m$dzmz == "mz", ], REML = FALSE)
options(scipen = 999)
summary(mz_1)

dz_1 <- lmer(value ~ (1 |id), data = dat.m[dat.m$dzmz == "dz", ], REML = FALSE)
summary(dz_1)

#5
ICC <- function(x){
  psi_hat <- unname(attr(VarCorr(x)$id, "stddev"))
  theta_hat<- attr(VarCorr(x), "sc")
  print(psi_hat^2/(theta_hat^2 + psi_hat^2))
}

ICC(mz_1)
ICC(dz_1)# NO WAY!

#6.
##OBSERVATIONS ARE NOT INDEPENDENT
## Assumption of exchangeability is OK in this example, not for Pearson's r