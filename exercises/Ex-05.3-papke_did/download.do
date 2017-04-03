* Paul E. Johnson
* 20170401

capture log close
set more off
log using download.log, replace text


* Unemployment claims data
use http://www.stata-press.com/data/mlmus3/papke_did, clear
saveold papke_did.dta12, version(12) replace




