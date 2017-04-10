* Paul E. Johnson
* 20170410

capture log close
set more off
log using download.log, replace text


* Toenail fungus data
use http://www.stata-press.com/data/mlmus3/toenail, clear
saveold toenail.dta12, version(12) replace




