* Paul E. Johnson
* 20160208

capture log close
set more off
log using Ex-3.4.log, replace text
 
* Dempster Rat-pups data
* use http://www.stata-press.com/data/mlmus3/pups, clear
* saveold pups.dta12, version(12)
* or
use pups.dta12, replace


sum

* 3.4.1. 

egen size = count(w), by(dam)

* 3.4.2
egen wmn = mean(w), by(dam)

* 3.4.3

twoway (scatter wmn size), ytitle("Mean of Litter")
* Need different symbols

* Do they want wmn horiz or vert?

* See: http://www.ats.ucla.edu/stat/stata/faq/graph_sep.htm
twoway (scatter wmn size if dose == 0, msymbol(0h)) ///
    (scatter wmn size if dose == 1, msymbol(S)) ///
    (scatter wmn size if dose == 2, msymbol(0)), ///
    ytitle("Mean of Litter") ///
    legend(label(1 "0") label(2 "1") label(3 "2"))
    
    
*3.4.4

xtmixed w i.sex i2.dose size|| dam: , mle

label define dose 0 "control" 1 "low" 2 "high"
label values dose dose
label define sex 0 "male" 1 "female"
label values sex sex


xtmixed w i.sex i.dose size|| dam: , mle
estimates store xtm1

 
xtset dam 
xtreg w i.sex i.dose size, fe

* Pooling
reg w i.sex i.dose size

* More fixed effects comparison

reg w i.sex i.dose size ibn.dam, noconstant

reg w ibn.dam, noconstant





*3.4.5
estimates restore xtm1
predict lev2, reffects
predict compse, reses
generate diagse = sqrt(exp(2*[lns1_1_1]_cons) - compse^2)
gen lev2s=lev2/diagse
twoway (scatter lev2 lev2s)
hist(lev2s)

predict lev1, rstandard
hist(lev1)



xtmixed w i.sex i.dose size|| dam: , mle vce(robust)
estimates store xtm2

