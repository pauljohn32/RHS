* Paul E. Johnson
* 20160402

* Doodles on the do file from RHS book

capture log close
set more off, permanently
log using toenail-1.log, replace text

use toenail.dta, clear

gen trt_month = treatment*month
gllamm outcome treatment month trt_month, i(patient) ///
  family(binom) link(probit) adapt
estimates store gllamm1

meglm outcome treatment month trt_month || patient: , ///
  family(binom) link(probit) intp(12)
estimates store meglam1

matrix a=e(b)
gllamm outcome treatment month trt_month, i(patient) ///
  family(binom) link(probit) nip(16) from(a) adapt

matrix a=e(b)
gllamm outcome treatment month trt_month, i(patient) ///
  family(binom) link(probit) nip(24) from(a) adapt

matrix a=e(b)
gllamm outcome treatment month trt_month, i(patient) ///
  family(binom) link(probit) nip(32) from(a) adapt
estimates store probit

* What is this matrix thing?
matrix list e(b)

* compares the estimates on the linear predictor scale

estimates restore probit
gllapred probit, u
gllamm outcome treatment month trt_month, i(patient) family(binom) ///
  link(logit) nip(32) adapt
gllapred logit, u

twoway (scatter probitm1 logitm1)

regress probitm1 logitm1 if visit==1, noconstant

* Usually people view it from the other direction
regress logitm1 probitm1 if visit==1, noconstant

* The point: logit coefficients are about 1.8 times
* the probit coefficients because the variance of the logit is that much
* greater. I can't figure how to make Stata show this, but in R, run:
* sd(rlogis(10000))



meglm outcome treatment month || patient: , ///
  family(binom) link(probit) intp(12)
estimates store meglam2


predict mu, mu
predict ranef, reffects
