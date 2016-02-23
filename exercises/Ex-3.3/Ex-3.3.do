* Paul E. Johnson
* 20160202

capture log close
set more off
log using Ex-3.3.log, replace text

* JPotthoff & Roy jaw growth data
* use http://www.stata-press.com/data/mlmus3/growth, clear
* saveold growth.dta12, version(12)

use growth.dta12, replace


sum

* 3.3.1
twoway (scatter measure age, sort)

sort idnr age
twoway (line measure age, connect(ascending))

twoway (line measure age, connect(ascending)), by(sex)
* want to label "1" and "2" in plot

* Go back and label the data
label define sex 1 "boy" 2 "girl"
label values sex sex


twoway (line measure age, connect(ascending)), by(sex)

* 3.3.2

xtmixed measure age i2.sex || idnr: , mle


* 3.3.3

xtmixed measure c.age##i2.sex || idnr: , mle


xtmixed measure c.age##i2.sex || idnr:, reml

* 3.3.4

predict m1, xb
* Want better color control, not getting on  lines
twoway (line measure age, connect(ascending) lcolor("gs10")) (line m1 age, connect(ascending))
* Apparently, quotes optional
twoway (line measure age, connect(ascending) lcolor(gs10)) (line m1 age, connect(ascending)), by(sex)

predict m1re, reffects


gen m1gp = m1re + m1
twoway (line m1gp age, connect(ascending) lcolor("pink"))

twoway (line measure age, connect(ascending) lcolor("gs10")) ///
    (line m1 age, connect(ascending)) ///
    (line m1gp age, connect(ascending) lcolor("pink"))
