* Paul E. Johnson
* 20160229

capture log close
set more off, permanently
log using Ex-5.1.log, replace text

* Tax preparer data
* Run download.do, which is just:
* use http://www.stata-press.com/data/mlmus3/taxprep.dta, clear
* saveold taxprep.dta12, version(12) replace
use taxprep.dta12

summarize

xtset subject 
xtsum

help summarize

mixed lntax prep ms hh depend age lntpi mr emp || subject: , ///
    mle covariance(unstructured) stddev
estimates store re1

xtreg lntax prep ms hh depend age lntpi mr emp, be
* Note time disappears. Figure out why (below)
estimates store be1

xtreg lntax prep ms hh depend age lntpi mr emp, fe
estimates store fe1
predict fe1_u, u
* When we have the group level data summary below, look at std.dev.(fe1_u)

regress lntax prep ms hh depend age lntpi mr emp i.subject



* What's the between regression? 
* Manually create an aggregated data set
* preserve/restore not necessary, but convenient.

* See RHS p. 152-4, showing how the between and within slopes can
* be recovered by recoding. First, create group mean variables
foreach var of varlist lntax time prep ms hh depend age lntpi mr emp {
    egen `var'_mn = mean(`var'), by(subject)
}

* Next create "individual deviations about mean" variables:
foreach var of varlist lntax time prep ms hh depend age lntpi mr emp {
    gen `var'_dev = `var' - `var'_mn
}


egen pickone = tag(subject)
* When you do this the first time, DO NOT use preserve. Study
* how the data set changes
preserve
keep if pickone==1
sum
xtsum
regress lntax_mn time_mn prep_mn ms_mn hh_mn depend_mn age_mn lntpi_mn mr_mn emp_mn
estimates store be2
restore
* Now I see why time dropped out of be model BE model?
* Unfortunately, if you did not run this as a big block with
* preserve at start and restore at end, you have annihialated your data
* and you can't recover it. So you have to go reload and start from scratch.
* Sorry, this is an educational process... 


* Whats the fe regression?  Lets fit a dummy, one for each subject

regress lntax time prep ms hh depend age lntpi mr emp ibn.subject, noconstant
estimates store fe2
* ibn causes stata to estimate intercepts for all groups

* Look above in the summary for the pickone data. Note the standard
* deviation of fixed effect estimates is same as number reported in
* fe output for sd(u)



hausman fe1 re1
* Fails, because I did not use same stata function to fit both

xtreg lntax time prep ms hh depend age lntpi mr emp, re 
estimates store re2

hausman fe1 re2
* Beginning to conclude hausman requires xtreg?   Not exactly.

* It requires results from same fitting functions with similar name
* structures. 

* And here's a bad thing. Unlike ANOVA tests, the order does matter.
hausman re2 fe1
* THe fixed effect should be first, it is the one that's consistent (I think)


* Here is a Fixed fit equivalent to the "within" model
mixed lntax time prep ms hh depend age lntpi  ///
    mr_dev emp_dev i.subject
estimates store fe4

* Random effects also with the mixed function. Use reml b/c consistent.
* In book, it says you need xtmixed with the GLS estimator, so I'm guessing there
mixed lntax time prep ms hh depend age lntpi mr emp || subject: , ///
    reml
estimates store re3

hausman fe4 re3
* Woo hoo, estimates! But output says nonsense. 
* If only I believed this was not gibberish: But I think it is, but 
* maybe I need to get help understanding hausman from somebody who knows.

hausman re3 fe4




*5.1.4
estimates restore re1

predict double re1pred, reffects 
hist(re1pred)

predict double reresid, residuals
hist(reresid)


xtset subject time
xtsum

xtreg lntax prep ms hh depend age lntpi mr emp prep_dev ms_dev hh_dev ///
    depend_dev age_dev lntpi_dev mr_dev emp_dev, mle



