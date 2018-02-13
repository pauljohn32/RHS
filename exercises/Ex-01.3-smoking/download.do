* Paul E. Johnson
* 20170125

capture log close
set more off
log using download.log, replace text

* smoking 
use http://www.stata-press.com/data/mlmus3/smoking, clear
saveold smoking.dta12, version(12)



