* Paul E. Johnson
* 20170124

capture log close
set more off
log using "Ex-01.2-anorexia.log", replace text

use anorexia.dta12, clear


* uses HC1 by default:
regress weight2 weight1 ib2.treat, vce(robust)

* use HC2 instead
regress weight2 weight1 ib2.treat, vce(hc2)


* user HC3 instead

regress weight2 weight1 ib2.treat, vce(hc3)


* see the memory structures created by most recent reg
ereturn list
