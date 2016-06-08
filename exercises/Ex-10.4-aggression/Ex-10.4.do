* Paul Johnson
* 20160411

capture log close
set more off, permanently
log using Ex-10.4.log, replace text



* Verbal-aggression data
* use http://www.stata-press.com/data/mlmus3/aggression, clear
* saveold aggression.dta12, version(12) replace
use aggression.dta12, clear

* 1.
tab y
* 2 becomes 1
recode y 2=1
tab y

* 2.
xtset person
xtlogit y do_want other_self blame express

* 3.
quietly xtset person
xtlogit y do_want other_self blame express anger gender

* 4.
quietly xtset person
xtlogit y i1-i24, nocons
estimates store xtlogit1
* 5.

* Learn how to convert string to factor
meglm y i.description || person: , family(binom) link(logit)



* 6.
estimates restore xtlogit1
matrix a = e(b)
matrix a1 = a[1,1..24]

clogit y i2-i24, group(person)
matrix a=e(b)
matrix a2 = 0,a[1,1..23]

matrix diff = a1-a2
matrix list diff

* 7.
use aggression.dta12, clear
recode y 2=1
set more off
gllamm y i1-i24, nocons i(person) link(logit) family(binom) adapt
estimates store gllamm
gllapred eap, u

xtmelogit y i1-i24, nocons || person:, 
estimates store xtmel
predict map, reffects
predict mapse, rese

predict fixed, xb

statsby mlest=_b[_cons] mlestse=_se[_cons], by(person) saving(ml, replace): ///
    logit y, offset(fixed)
merge m:1 person using ml
	
egen score = total(y), by(person)

tabstat eapm1 map mlest, by(score)

tabstat eaps1 mapse mlestse, by(score)


twoway (pcarrow mlest mlest eapm1 mlest if item==1) ///
   (function y=x, range(mlest)), xtitle(ML estimate) ///
   ytitle(EAP and ML estimates) legend(order(1 "ML to EAP"))

twoway (pcarrow mlest mlest map mlest if item==1) ///
   (function y=x, range(mlest)), xtitle(ML estimate) ///
   ytitle(MAP and ML estimates) legend(order(1 "ML to MAP"))

twoway (line eaps1 score, sort) (line mapse score, sort) ///
   (line mlestse score, sort), xtitle(Total score) ///
   ytitle(Standard error of prediction) ///
   legend(order(3 "ML" 1 "EAP" 2 "MAP") row(1) pos(12) ring(0))

