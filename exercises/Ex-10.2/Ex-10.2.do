* Paul Johnson
*

* Exercise 10.2 Ohio wheeze data

capture log close
set more off, permanently
log using Ex-10.2.log, replace text

* use http://www.stata-press.com/data/mlmus3/wheeze, clear
* saveold wheeze.dta12, version(12) replace
use wheeze.dta12, clear

by id (age), sort: generate lag = y[_n-1]
logistic y age smoking lag if age>-2


* 2.
gllamm y age smoking, i(id) link(logit) family(binom) adapt eform

* xtset id
* xtlogit y age smoking, or
*
* xtmelogit y age smoking || id:, or

* 3.
xtset id age
xtgee y age smoking, corr(unstructured) vce(robust) eform

estat wcorrelation

xtgee y age smoking, corr(exchangeable) vce(robust) eform

estat wcorrelation
