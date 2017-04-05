* Paul E. Johnson
* 20170404

capture log close
set more off
log using download.log, replace text


* Returns to schooling data (Hausman)
use http://www.stata-press.com/data/mlmus3/returns, clear

generate exp2 = exp^2

saveold returns.dta12, version(12) replace




