* Paul E. Johnson
* 20170202

capture log close
set more off
log using "Ex-3.7.log", replace text

* HSB: High School and Beyond
use hsb.dta12, replace





quietly xtset schoolid
xtreg mathach ses, mle


* quietly xtset schoolid
xtsum ses


* To calculate the school-level mean and deviations
* for a lot of variables, Stata offers. Here
* we don't need to do a log of variables, but 
* we can practice by doing ses and female

foreach var of varlist ses female {
    egen `var'_mn = mean(`var'), by(schoolid)
}

foreach var of varlist ses  female {
    gen `var'_dev = `var' - `var'_mn
}


* 3.7.4 asks for xtreg

xtreg mathach ses_dev ses_mn, mle
lincom ses_mn - ses_dev

* Compare xtmixed
xtmixed mathach ses_dev ses_mn || schoolid: , mle
lincom ses_mn - ses_dev

* Or just mixed

mixed mathach ses_dev ses_mn || schoolid: , mle
lincom ses_mn - ses_dev


mixed mathach ses_dev ses_mn || schoolid: , reml
lincom ses_mn - ses_dev




