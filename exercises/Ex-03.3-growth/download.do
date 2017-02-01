* Paul E. Johnson
* 20160202

capture log close
set more off
log using "download.log", replace text

* JPotthoff & Roy jaw growth data
use http://www.stata-press.com/data/mlmus3/growth, clear
saveold growth.dta12, version(12)
