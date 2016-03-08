* Paul E. Johnson
* 20160229

capture log close
set more off, permanently
log using Ex-5.4.log, replace text

* Papke unemployment data
* use http://www.stata-press.com/data/mlmus3/ezunem.dta, clear
* saveold ezunem.dta12, version(12) replace
use ezunem.dta12, clear


summarize

xtset city t
xtsum

* There are 9 time points

gen uclmslog = log(uclms)

reg uclmslog i.t i.city ez



xtreg uclmslog i.t ez, fe 

foreach var of varlist uclmslog ez t {
    egen `var'_mn = mean(`var'), by(city)
}

foreach var of varlist uclmslog ez t{
    gen `var'_dev = `var' - `var'_mn
}

reg uclmslog_dev ez_dev i.t


xtreg luclms d81-d88 ez, fe


5.4.2.b

regress D.uclmslog D.(d81-d88) D.ez


5.4.3 lagged response model

regress uclmslog d81-d88 ez L.uclmslog

* 5.4.4 Lagged response with office intercept
* * 5.4.4.a random intercept city
xtmixed uclmslog d81-d88 ez L.uclmslog || city:, mle

xtmixed uclmslog i.t ez L.uclmslog || city:, mle


It seems unreasonable to assume (as implicitly in the above model) that the random
intercept only affects the response in 1981-1988 but not the response at the first occasion
in 1980. If the random intercept also affects the response in 1980, the estimate of the
intervention effect given above will be inconsistent due to this initial-conditions problem.

* 5.4.4.b: Anderson Hsiao approach with second lag as instrument
ivregress 2sls D.uclmslog D.(ez d82-d87) (LD.uclmslog = L2.uclmslog)

* 5.4.4.c: A-H with second lag of difference as instrument. 
xtivreg luclms d82-d88 ez (L.luclms = L2.luclms), fd

