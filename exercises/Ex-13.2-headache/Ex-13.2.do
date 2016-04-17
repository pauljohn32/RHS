* Paul Johnson
* 20160411

* RHS exercise 13.2: Headaches
capture log close
set more off, permanently
log using Ex-13.2.log, replace text


use http://www.stata-press.com/data/mlmus3/headache, clear
saveold headache.dta12, version(12) replace
use headache.dta12, clear


sum
xtset id
xtsum

tab belief

tab y aspartame
tab y belief
tab belief aspartame

generate daysln = ln(days)


* 0. Ordinary poisson

poisson y i.aspartame, offset(daysln)  nolstretch
estimates store pois0
*Note same as:
poisson y i.aspartame, exposure(days)


poisson y i.aspartame##i.belief, offset(daysln) vsquish
estimates store pois1

est tab pois0 pois1, se

* Fit same with meglm to allow lrtest later
meglm y i.aspartame, offset(daysln) family(poisson) nolstretch vsquish
estimates store pois2

meglm y i.aspartame, offset(daysln) /// 
    family(nbinom) link(log) vsquish
estimates store nb0
display exp(-.4431468)
lrtest nb0 pois2





* 1. Fit an offset model

xtmepoisson y i.aspartame || id: , offset(daysln) 
estimates store ri0


meglm y i.aspartame, offset(daysln) || id:  , family(poisson) link(log) vsquish nolstretch
estimates store ri0b

est tab ri0 ri0b
display exp(-.363194)^2

est tab pois0 ri0b



lrtest ri0b     

* 2.

xtmepoisson y i.aspartame##i.belief || id: , offset(daysln)
estimates store ri1


meglm y i.aspartame##i.belief, offset(daysln) || id: , /// 
    family(poisson) link(log) vsquish nolstretch
estimates store ri1b
predict meglm1pr, xb




lrtest ri1b pois2


est tabl ri10b ri1b
estimates restore ri1
* How the hell do you extract that log sigma? Could they make this any
* more tedious and baroque?
display exp(lns1_1_1)^2

display exp(-.3506469)^2

est tab ri1b pois2


* Negative binomial

meglm y i.aspartame##i.belief, offset(daysln) || id: , /// 
    family(nbinom) link(log) vsquish nolstretch
estimates store nb1

lrtest ri1b nb1

gen rown = _n



meglm y i.aspartame##i.belief, offset(daysln) || id: , || rown:, /// 
    family(poisson) link(log) vsquish nolstretch intpoint(10)
estimates store mepois2lev


* That is reported as if it is nested. 
meglm y i.aspartame##i.belief, offset(daysln) || id:  || rown:, /// 
    family(poisson) link(log) vsquish nolstretch intpoint(10) covar(unstructured)
estimates store mepois2lev



/* gllamm command:
generate belief_aspart = belief*aspartame
gllamm y aspartame belief belief_aspart, i(id) link(log) family(poisson) offset(daysln)
*/

* 3. introduce period 1 lag of aspartme.  Assume initial aspartame is 0
*  This avoids loss of observations that usually results from lagging

by id (period), sort: generate aspart_lag = aspartame[_n-1]
replace aspart_lag = 0 if period == 1
xtmepoisson y c.aspartame##i.belief aspart_lag || id: , ///
   offset(daysln)
estimates store ri2

meglm y c.aspartame##i.belief aspart_lag, offset(daysln) || id: , family(poisson) link(log) 
estimates store ri2b

est tab ri2 ri2b

* 4. Random slope
xtmepoisson y c.aspartame##i.belief || id: aspartame, ///
    cov(unstr) offset(daysln) nolstretch
estimates store rc
lrtest rc ri1

meglm y c.aspartame##i.belief, offset(daysln) || id: aspartame, ///
    cov(unstr) family(poisson) link(log) 
estimates store rc1b

* Note difference when I accidentally excluded the covariance

meglm y c.aspartame##i.belief, offset(daysln) || id: aspartame, ///
    family(poisson) link(log) 
estimates store rc1c

estimates tab rc rc1b rc1c


/* gllamm command:
eq inter:cons
eq slope:aspartame
gllamm y aspartame, i(id) link(log) family(poisson) offset(daysln) ///
  nrf(2) eqs(inter slope) nip(15) ip(m)
*/

* 5. * gllamm does not allow factor notation, have to generate interaction
generate belief_aspart = belief*aspartame
gllamm y aspartame belief belief_aspart, i(id) link(log) ///
   family(poisson) offset(daysln)
gllapred rate, nooff mu

egen one = tag(aspartame id)
sort id aspartame
twoway line rate aspartame if one==1&id~=26, connect(ascending) ///
  xtitle(Aspartame) xlabel(0 "No" 1 "Yes") ytitle(Predicted headache rate)

  
* 6. Is there any way to do fe without xtpoisson?
xtset id
xtpoisson y i.aspartame##i.belief, fe offset(daysln) vsquish

glm y i.aspartame#i.belief ibn.id, noconstant offset(daysln) family(poisson)

xtset id

xtpoisson y i.aspartame, fe offset(daysln)

glm y i.aspartame ibn.id, noconstant offset(daysln)

