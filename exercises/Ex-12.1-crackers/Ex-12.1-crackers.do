* Paul Johnson
* 20160503

capture log close
set more off, permanently
log using Ex-12.1.log, replace text


* use http://www.stata-press.com/data/mlmus3/crackers, clear
* saveold crackers.dta12, version(12) replace

use crackers.dta12, clear

* 1.

egen resp = group(id occ)

tabulate brand, generate(br)
rename br1 Sunshine
rename br2 Keebler
rename br3 Nabisco
clogit choice Sunshine Keebler Nabisco display feature price, group(resp)


* 2.
gllamm brand Sunshine Keebler Nabisco display feature price, i(resp) ///
   noconstant link(mlogit) expanded(resp choice o) init
estimates store mod0

   
eq pr: price
gllamm brand Sunshine Keebler Nabisco display feature price, i(id) ///
   eqs(pr) noconstant link(mlogit) expanded(resp choice o) adapt
lrtest mod0 .
   
eq f: feature
gllamm brand Sunshine Keebler Nabisco display feature price, i(id) ///
   eqs(f) noconstant link(mlogit) expanded(resp choice o) adapt
lrtest mod0 .
   
eq d: display
gllamm brand Sunshine Keebler Nabisco display feature price, i(id) ///
   eqs(d) noconstant link(mlogit) expanded(resp choice o) adapt
lrtest mod0 .



