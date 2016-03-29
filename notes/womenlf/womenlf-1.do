* Paul E. Johnson
* 20160308

capture log close
set more off, permanently
log using womenlf-1.log, replace text

* Download the womenlf data
use womenlf.dta, clear

* subj
* dv for field dependent
* rfn-rci word attributes

summarize

recode workstat 2=1

logit workstat husbinc chilpres


predict preta, xb

predict prprob, pr


logit workstat husbinc chilpres, or


glm workstat husbinc chilpres, link(logit) family(binomial)


predict glmpreta, xb

predict glmprprob, mu



probit workstat husbinc chilpres



predict probitpreta, xb

predict probobitprprob, pr

twoway (scatter glmprprob probobitprprob)

twoway(scatter prprob probobitprprob)



