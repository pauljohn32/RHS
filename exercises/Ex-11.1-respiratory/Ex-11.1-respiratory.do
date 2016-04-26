* Paul Johnson
* 20160426

capture log close
set more off, permanently
log using Ex-11.1.log, replace text

* use http://www.stata-press.com/data/mlmus3/respiratory, clear
* saveold respiratory.dta12, version(12) replace
use respiratory.dta12, clear
 
* 1
reshape long v, i(patient) j(visit)


* 2 Use meglm, not old tools:

meglm v drug male age bl || patient:, family(ordinal) intpoints(12)
estimates store meglm1

* Close enough to gllamm
gllamm v drug male age bl, i(patient) ///
  link(ologit) adapt
estimates store gllamm1


* 3 

meglm v drug male age bl visit || patient:, family(ordinal) intpoints(12)
estimates store meglm2

meglm v drug male age bl c.visit#i.drug || patient:, family(ordinal) intpoints(12)
estimates store meglm3

*4 

*v is scored 0 through 4, but output still numbers categories 1-5
estimates restore meglm1
predict meglim1pr*, mu
gen gt3 = meglim1pr5 
gen gt2 = meglim1pr4 + meglim1pr5 
gen gt1 = meglim1pr3 + meglim1pr4 + meglim1pr5 
gen gt0 = meglim1pr2 + meglim1pr3 + meglim1pr4 + meglim1pr5 

sort patient visit
twoway (line gt3 visit) (line gt2 visit) ///
  (line gt1 visit) (line gt0 visit) ///
  if patient<13, by(patient)

translate @Graph "graph1.pdf", name("Graph")


estimates restore gllamm1
gllapred above3, mu above(3)
gllapred above2, mu above(2)
gllapred above1, mu above(1)
gllapred above0, mu above(0)

sort patient visit
twoway (line above3 visit) (line above2 visit) ///
  (line above1 visit) (line above0 visit) ///
  if patient<13, by(patient)
  
translate @Graph "graph2.pdf", name("Graph")

estimates restore meglm1
* Previous was eb means
predict meglmmode*, mu conditional(ebmodes)


* margins, at(male=(0, 1) visit=(1, 2)) plot
* margins, at(male=(0, 1) visit=(1, 2)) atmeans plot
