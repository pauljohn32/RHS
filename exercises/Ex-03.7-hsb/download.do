* Paul E. Johnson
* 20170125

capture log close
set more off
log using "download.log", replace text

use http://www.stata-press.com/data/mlmus3/hsb, clear
saveold hsb.dta12, version(12)

