* Paul Johnson
* 20160411

* RHS exercise: epilep & brain tauma
capture log close
set more off, permanently
log using Ex-13.1.log, replace text


* use http://www.stata-press.com/data/mlmus3/epilep, clear
* saveold epilep.dta12, version(12) replace
use epilep.dta12, clear

sum
* The lbase_trt variable is not understanable, it is not
* a product of these two

* This one is also available with Stata
* use http://www.stata-press.com/data/r14/epilepsy, clear
sum
* Same problem


* what is lbas_trt?
* gen lbastrtpj = lbas * treat
* summarize lbastrtpj, mean
* gen lbastrtpjctr = lbastrtpj - r(mean)
* gen lbastrtpjstd = lbastrtpjctr/r(sd)

* 1.
gllamm y lbas treat lbas_trt lage v4, i(subj) ///
  link(log) family(poisson) adapt
estimates store gllamm1

meglm y lbas treat lbas_trt lage v4 || subj: , ///
    link(log) family(poisson) intpoints(12)
estimates store meglm1

est tab gllamm1 meglm1, se

* 2.
eq int: cons
eq slope: visit
gllamm y lbas treat lbas_trt lage visit, i(subj) ///
    link(log) family(poisson) nrf(2) eqs(int slope) ///
    ip(m) nip(15) adapt
estimates store gllamm2


meglm y lbas treat lbas_trt lage visit || subj:visit, ///
    link(log) family(poisson) intpoints(12)
estimates store meglm2a

est tab gllamm2 meglm2a, se

meglm y i.treat##c.lbas lage visit || subj:visit, ///
    link(log) family(poisson) intpoints(12) covariance(unstructured)
estimates store meglm2b

est tab gllamm2 meglm2a meglm2b


* 3.
estimates restore gllamm2
gllapred pred, mu
gllapred gllamu, u
sort treat subj

* TODO: Note the way to generate observation identifier.
* Why this way? 
by treat subj: generate f = _n==1
by treat: generate id = sum(f)

twoway line pred visit if id<13 & treat==0, by(id)

twoway line pred visit if id<13 & treat==1, by(id)

*TODO: insert colored spaghetti plot 


estimates restore meglm2b

margins treat 


* I continue to be baffled that the lbase_trt variable is
* nonsense.  Why nobody else noticing this?

* Clayton uploaded possibly the original data with a stata package "sg98" which
* has an early random effects function "rpoisson"
* findit sg98

use epilepsy, clear
sum d
sum lbase
gen lbasectr = lbase - r(mean)
gen lbasectr_trt = lbasectr * trt

glm d i.trt##c.lbase lage period, link(log) family(poisson)

meglm  d i.trt##c.lbase lage period || id:period, ///
    link(log) family(poisson) intpoints(12) covariance(unstructured)
estimates store meglm3a

meglm  d i.trt##c.lbasectr lage period || id:period, ///
    link(log) family(poisson) intpoints(12) covariance(unstructured)
estimates store meglm3b



meglm  d i.trt##c.lbasectr lage period|| id: , ///
    link(log) family(poisson) intpoints(12) covariance(unstructured)
estimates store meglm3c
