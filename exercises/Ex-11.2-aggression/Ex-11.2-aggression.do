* Paul Johnson
* 20160426

capture log close
set more off, permanently
log using Ex-11.2.log, replace text

* use http://www.stata-press.com/data/mlmus3/aggression, clear
* saveold aggression.dta12, version(12) replace
use aggression.dta12, clear
 
 
 *1 
 
 meglm y do_want other_self blame express || person: , family(ordinal)
 estimates store meglm1
 
 *2
 
 
 meglm y do_want other_self blame express anger gender || person: ///
    , family(ordinal) intpoints(5)
 estimates store meglm2
 
 
 *3 I don't know how to do this one in Stata without gllamm
 * See below for RHS answer
  
 
 *** From the RHS Teacher manual, these
 *** equivalents take much longer!  But #3 gets the non-parallel lines
 *** Exercise 11.2 Verbal-aggression data

* 1.

gllamm y do_want other_self blame express, i(person)  ///
  link(ologit) adapt nip(5)

* 2.
matrix a = e(b)
gllamm y do_want other_self blame express anger gender, ///
  i(person) link(ologit) from(a) adapt nip(5)
estimates store mod2

* 3.
matrix a = e(b)
eq thr: do_want
gllamm y other_self blame express anger gender, i(person) ///
  link(ologit) thresh(thr) from(a) skip adapt nip(5)
estimates store mod3
lrtest mod2 mod3
