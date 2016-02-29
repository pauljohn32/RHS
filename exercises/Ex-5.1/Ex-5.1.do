* Paul E. Johnson
* 20160229

capture log close
set more off, permanently
log using Ex-5.1.log, replace text

* Tax preparer data
* use http://www.stata-press.com/data/mlmus3/taxprep.dta, clear
* saveold taxprep.dta12, version(12) replace
use taxprep.dta12

summarize

xtset subject 
xtsum
xtset subject time
xtsum

help summarize

mixed lntax time prep ms hh depend age lntpi mr emp || subject: , ///
    mle covariance(unstructured) stddev
estimates store re1

xtreg lntax time prep ms hh depend age lntpi mr emp, be
* Note time disappears. Figure out why (below)
estimates store be1

xtreg lntax time prep ms hh depend age lntpi mr emp, fe
estimates store fe1


* What's the between regression? Manually create an aggregated data set
* preserve/restore not necessary, but convenient.
egen pickone = tag(subject)
* set to find out
preserve
keep if pickone==1
sum
xtsum
regress lntax time prep ms hh depend age lntpi mr emp
estimates store be2
restore
* Now I see why time dropped out of be model


* Whats the fe regression?  Lets fit a dummy, one for each subject

regress lntax time prep ms hh depend age lntpi mr emp i.subject, noconstant
estimates store fe2
* Appears there is an error that Stata does not estimate Subject 1
regress lntax time prep ms hh depend age lntpi mr emp i.subject
estimates store fe3


hausman fe1 re1
* Fails, don't know why yet. Method of fitting mixed effect?

xtreg lntax time prep ms hh depend age lntpi mr emp, re 
estimates store re2

hausman fe1 re2
* Beginning to conclude hausman requires xtreg??

xtmixed lntax time prep ms hh depend age lntpi mr emp || subject: , ///
     reml covariance(unstructured) stddev
estimates store re3
hausman fe1 re3


*5.1.4
estimates restore re1

predict double re1pred, reffects 
hist(re1pred)

predict double reresid, residuals
hist(reresid)



* See RHS p. 152, showing how the between and within slopes can
* be recovered by recoding thus:
foreach var of varlist lntax time prep ms hh depend age lntpi mr emp {
    egen `var'_mn = mean(`var'), by(subject)
}

foreach var of varlist lntax time prep ms hh depend age lntpi mr emp {
    gen `var'_dev = `var' - `var'_mn
}


xtset subject time
xtreg lntax prep ms hh depend age lntpi mr emp prep_dev ms_dev hh_dev ///
    depend_dev age_dev lntpi_dev mr_dev emp_dev, mle



