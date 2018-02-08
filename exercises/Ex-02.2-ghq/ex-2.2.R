library(lme4)
library(lmerTest)

ghq <- data.frame(
  Student = seq(1, 12, by =1),
  GHQ = c(12, 8, 22, 10, 10, 6, 8, 4, 14, 6, 2, 22, 12, 7, 24, 14, 8, 4, 5, 6, 14, 5, 5, 16),
  occasion = rep(c(1,2), each = 12)
)
#1
mg_1 <- lmer(GHQ ~  (1 | Student), data = ghq)
summary(mg_1) 
eb <- unname(ranef(mg_1)[[1]][1])# EB estimates

psi_hat <- unname(attr(VarCorr(mg_1)$Student, "stddev"))
theta_hat<- attr(VarCorr(mg_1), "sc")
shrinkage <- psi_hat/(psi_hat + (theta_hat/12))

plot.df <- data.frame(
  eb = eb,
  shrink = rep(shrinkage, 12)
)
plot.df$ML <- plot.df$eb/plot.df$shrink# p. 111 for formula
plot.df$more.shrink <- plot.df$shrink * 0.5 

#2 - morr shrinkage, the more shrunken EB vals are -- less reliability means lower
# corr b/w eb & ML
plot(eb ~ ML, data = plot.df, main = "Demonstration of EB Values Shrinking to Zero", ylab = "EB Value", xlab = "ML Value")
legend(-7, 8, c(paste0("Shrinkage =", round(plot.df[1, 2], 2)), paste0("Shrinkage =", round(plot.df[1, 4], 2))), col = c("black", "blue"), pch =1,  bty = 'n')
points((plot.df$ML * plot.df$more.shrink) ~ plot.df$ML, data = plot.df, col = "blue")
abline(lm(eb ~ ML, data = plot.df), col = "red")
abline(lm((plot.df$ML * plot.df$more.shrink) ~ plot.df$ML), col = "green")

#3 -- No evidence. 
mg_2 <- lmer(GHQ ~ factor(occasion)  + (1|Student), data = ghq)
summary(mg_2)  