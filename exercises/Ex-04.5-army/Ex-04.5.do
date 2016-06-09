* Paul E. Johnson
* 20160308

capture log close
set more off, permanently
log using Ex-4.5.log, replace text

* Army member satisfaction (Bliese, 1996)
* use http://www.stata-press.com/data/mlmus3/army.dta, clear
* saveold army.dta12, version(12) replace
use army.dta12, clear

xtset grp

xtsum

twoway scatter wbeing cohes if grp < 13, by(grp, compact)


foreach var of varlist cohes lead hrs wbeing{
    egen `var'_mn = mean(`var'), by(grp)
}

foreach var of varlist cohes lead hrs wbeing{
    gen `var'_dev = `var' - `var'_mn
}

* I wondered what tsum means by "SD within", here's the answer:
* It is SD of group-mean centered data
sum cohes_dev


regress wbeing hrs
regress wbeing hrs cohes lead


* Between "be" lets get the between regression estimates, the easy and hard ways
xtreg wbeing hrs, be

egen pickone = tag(grp)
preserve
keep if pickone==1
sum
xtsum
regress wbeing_mn hrs_mn
estimates store be2
restore

* Fixed Effects

xtreg wbeing hrs, fe

reg wbeing hrs_dev



mixed wbeing cohes lead hrs || grp: , covariance(unstructured)

xtmixed wbeing cohes lead hrs  cohes_mn lead_mn hrs_mn  || grp: , covariance(unstructured)

xtmixed wbeing cohes_dev lead_dev hrs_dev  cohes_mn lead_mn hrs_mn  || grp: , covariance(unstructured)

