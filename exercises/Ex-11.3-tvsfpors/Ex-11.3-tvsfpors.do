* Paul Johnson
* 20160426

capture log close
set more off, permanently
log using Ex-11.3.log, replace text

* use http://www.stata-press.com/data/mlmus3/tvsfpors, clear
* saveold tvsfpors.dta12, version(12) replace
use tvsfpors.dta12, clear
 
 *1
meglm thk prethk i.cc##i.tv || school: , family(ordinal)

meglm, or

* 2

meglm thk prethk i.cc##i.tv || class: , family(ordinal)

meglm, or


* Following test fails!
** meglm thk prethk i.cc##i.tv || class:  || school: , family(ordinal)

* Now respect the nesting, runs!
meglm thk prethk i.cc##i.tv || school:  || class: , ///
    family(ordinal) covar(unstructured)

