* Paul E. Johnson
* 20170125

capture log close
set more off
log using download.log, replace text


* Tax preparer data
use http://www.stata-press.com/data/mlmus3/taxprep, clear
saveold taxprep.dta12, version(12) replace




