* Paul E. Johnson
* 20160401

capture log close
set more off, permanently
log using Ex-5.3.log, replace text

* Unemployment claims data
* Run download.do to get the data

use papke_did.dta12, clear

* This is a two-year data set, only 1983 and 1984

summarize

xtset city
xtsum



xtset city year
xtsum


help summarize

* 1 Posttest uses data only from 1984

* I did some notes about R, showing regression is equivalent to a t-test,
* as long as we assume equal within group variance. This is familiar to me.

* A t test
ttest luclms if year == 1984, by(ez)


regress luclms ez if year == 1984



* 2 Do a pre-post design, keeping only the ez = 1 cities

egen treat = max(ez), by(city)

preserve
reshape wide luclms ez, i(city) j(year)
ttest luclms1984=luclms1983 if treat==1
restore


* Note MLMUS do it by reshaping (so they can see result) and then
* reshaping back.

* reshape long luclms ez, i(city) j(year)

* I don't need to do that because I have preserve / restore

* Do same with a fixed effects regression model

xtset city
xtreg luclms ez if treat==1, fe


* I wondered if random effect would differ hugely (sample very small)

xtset city
xtreg luclms ez if treat==1, re


* 3 Compare posttest-only and one-group pre-post

* posttest-only does not control for time-constant confounders (unit characteristics)
* pre-post does not control for time-correlated confounders


* 4 The diff in diff model.

* I'll just take the regression model here

xtset city
xtreg luclms i.year ez, fe


** Recall fe same as fitting all dummies, might as well confirm that

reg luclms i.year ez ibn.city, noconstant



* 5. The "diff in diff" design theoretically accounts for confounding
* variables that are fixed characteristics of the cities and also
* for time effects that are equivalent across all cities.
