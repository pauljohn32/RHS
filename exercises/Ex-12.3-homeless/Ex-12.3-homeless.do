* Paul Johnson
* 20160426

capture log close
set more off, permanently
log using Ex-12.3.log, replace text

* If you don't have data yet, run these 2 lines
* use http://www.stata-press.com/data/mlmus3/homeless, clear
* saveold homeless.dta12, version(12) replace
use homeless.dta12, clear

* 1.
xtset id time
xtdescribe

* Previous same as this:
* xtdescribe, i(id) t(time)

* 2.


* 3.
* Make sure time is treated as integer, not float:
recast int time , force
recast int section8 , force
label define Housing 1 "section8" 0 "notsection8" 
label values section8 Housing

mlogit housing ib3.time##ib0.section8, base(0)

* Is better than manually creating indicators and interactions like this.
* Note following turns time = 2 into t1
tabulate time, gen(t)
generate t1_sec8 = t1*section8
generate t2_sec8 = t2*section8
generate t3_sec8 = t3*section8

mlogit housing section8 t1 t2 t3 t1_sec8 t2_sec8 t3_sec8, base(0)



* 4. Start over with clear data
clear all
use homeless.dta12, clear
recast int time , force
recast int section8 , force
label define Housing 1 "section8" 0 "notsection8" 
label values section8 Housing

tabulate time, gen(t)
generate t1_sec8 = t1*section8
generate t2_sec8 = t2*section8
generate t3_sec8 = t3*section8

* Create integer to index rows
generate obs = _n

expand 3
* Some fancy recoding and re-indexing here
by obs, sort: generate alt = _n-1
generate choice = housing==alt

* Creates dummies for values of alt:
label define altlab 0 "unknown" 1 "comm" 2 "indep" 
label values alt altlab

tabulate alt, generate(a)
* Has bad property that value 0 becomes a1, ... value 2 becomes a3
rename a2 comm
rename a3 indep

foreach var of varlist section8 t1 t2 t3 t1_sec8 t2_sec8 t3_sec8 {
    gen comm_`var' = comm*`var'
    gen indep_`var' = indep*`var'
}


gllamm alt comm_* comm indep_* indep, i(id) link(mlogit) ///
   exp(obs choice o) noconstant init


* 5.

eq co: comm
eq in: indep

gllamm alt comm_* comm indep_* indep, i(id) nrf(2) eqs(co in) ///
  link(mlogit) exp(obs choice o) nip(5) noconstant trace
