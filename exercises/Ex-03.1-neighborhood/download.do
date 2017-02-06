* Paul E. Johnson
* 20170125

capture log close
set more off
log using "download.log", replace text

use http://www.stata-press.com/data/mlmus3/neighborhood, clear
saveold neighborhood.dta12, version(12)

