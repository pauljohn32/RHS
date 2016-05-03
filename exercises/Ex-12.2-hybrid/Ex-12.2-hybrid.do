* Paul Johnson
* 20160426
* Hybrid car data

capture log close
set more off, permanently
log using Ex-12.2.log, replace text

* use http://www.stata-press.com/data/mlmus3/hybrid, clear
* saveold hybrid.dta12, version(12) replace
use hybrid.dta12, clear



* 1.
clogit chosen price cost range electric hybrid highperf medhiperf, ///
    group(situation)

* 2.

* 3. Problem as stated says create 1-4 alt, but there are
* only 3 alts
by situation, sort: gen alt = _n

* "o" in expanded specifies one set of coefficients
gllamm alt price cost range electric hybrid       ///
   highperf medhiperf, i(id) link(mlogit)         ///
   expanded(situation chosen o) noconstant init
   
* 4.

eq elec: electric
eq hyb: hybrid
 
gllamm alt price cost range electric hybrid   ///
   highperf medhiperf, i(id) link(mlogit)     ///
   expanded(situation chosen o) noconstant    ///
   nrf(2) eqs(elec hyb) trace adapt ip(m) nip(11)
   
*ip: specifies shortcut integration options   
*m : spherical quadrature rules
*g : gaussian quadrature
*f : freely estimated mass points (is this the A in GQ?)
