* Paul E. Johnson
* 20170403

capture log close
set more off
log using download.log, replace text

* Cognitive style data (Broota, 1989)
use http://www.stata-press.com/data/mlmus3/cogstyle.dta, clear
saveold cogstyle.dta12, version(12) replace




