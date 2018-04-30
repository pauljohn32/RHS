## Paul Johnson
## 20180424

library(foreign)
library(lme4)



fn <- "dairy"
if (!file.exists(paste0(fn, ".dta12"))) {
    download.file(paste0("http://www.stata-press.com/data/mlmus3/", fn, ".dta"),
                  destfile = paste0(fn, ".dta12"))
}

dairy <- read.dta(paste0(fn, ".dta12"))


#1

m1 <- glmer(fscr ~ lncfs + ai + heifer + (1 | cow), 
            family = binomial(link = "logit"), nAGQ = 30,
            data = dairy)

## Convergence warning is solved by changing optimizer
m1a <- glmer(fscr ~ lncfs + ai + heifer + (1 | cow), 
            family = binomial(link = "logit"), nAGQ = 30,
            data = dairy, control = glmerControl(optimizer = "bobyqa"))
summary(m1)


#2
cc <- confint(m1, parm = "beta_", )
tab <- cbind(estimate = fixef(m1), cc)
etab <- exp(tab)

#3
0.3419/(0.3419 + (pi^2/3))
## 9% variance attributable to the random intercept,
## i.e., differences between cows

#4 Where in the world does this calculation come from?
exp(sqrt(2 * 0.3419)*qnorm(.75))

