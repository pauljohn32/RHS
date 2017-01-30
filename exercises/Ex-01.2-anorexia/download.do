* Paul E. Johnson
* 20170125

capture log close
set more off
log using download.log, replace text

* Hand's anorexia data 
use http://www.stata-press.com/data/mlmus3/anorexia, clear
saveold anorexia.dta12, version(12)



