* Paul Johnson
* 20160411

capture log close
set more off, permanently
log using Ex-10.5.log, replace text



* Cows!
* use http://www.stata-press.com/data/mlmus3/dairy, clear
* saveold dairy.dta12, version(12) replace
use dairy.dta12, clear


gllamm fscr lncfs ai heifer, i(cow) link(logit) fam(binom) adapt
estimates store cow1

meglm fscr lncfs ai heifer || cow: , family(binom) link(logit) 
estimates store cow2

xtmelogit fscr lncfs ai heifer || cow:
estimates store cow3

*xtset cow
*xtlogit fscr lncfs ai heifer

* 2.
estimates restore cow1
gllamm, eform

* 3.

display .34188062/(.34188062 + _pi^2/3)

* 4.

display exp(sqrt(2*.34188062)*invnormal(3/4))
