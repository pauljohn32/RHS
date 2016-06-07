* Paul E. Johnson
* 20160229

capture log close
set more off, permanently
log using Ex-5.2.log, replace text

* Antisocial behavior data
* use http://www.stata-press.com/data/mlmus3/antisocial.dta, clear
* saveold antisocial.dta12, version(12) replace
use antisocial.dta12


summarize

xtset id occ
xtsum




mixed anti pov momage female childage hispanic black momwork married || id: , covariance(unstructured) mle stddev
estimates store ri1


xtset id
xtmixed anti pov momage female childage hispanic black momwork married, mle 
estimates store ri2

lrtest ri1 ri2
* ERROR: test involves different estimators: mixed vs. xtmixed

xtmixed anti pov momage female childage hispanic black momwork married || id: , covariance(unstructured) mle stddev
estimates store ri3

lrtest ri2 ri3


mixed anti pov momage female childage hispanic black momwork married, mle stddev
estimates store ri4

lrtest ri4 ri1

psi
----
    psi + theta


generate icc = 1.1469 / (1.1469 + 1.0127)
* flabbergasted Stata makes it so difficult to do this programatically



* 5.2.4 guessing x2 is pov ?


* See RHS p. 152, showing how the between and within slopes can
* be recovered by recoding thus:
foreach var of varlist anti pov momage female childage hispanic black momwork married {
    egen `var'_mn = mean(`var'), by(id)
}

foreach var of varlist anti pov momage female childage hispanic black momwork married {
    gen `var'_dev = `var' - `var'_mn
}




mixed anti momage female childage hispanic black ///
                                               momwork married pov_mn pov_dev || id: , ///
                                                                          covariance(unstructured) mle stddev
estimates store ri5


* 5.2.5 comparing the between and within estimates

lincom pov_mn - pov_dev
