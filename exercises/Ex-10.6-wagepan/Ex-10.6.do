* Paul Johnson
* 20160411

capture log close
set more off, permanently
log using Ex-10.7.log, replace text

* use http://www.stata-press.com/data/mlmus3/wagepan, clear
* saveold wagepan.dta12, version(12) replace
use wgepan.dta12, clear
* Union membership data data again


xtset nr
xtlogit union educ black hisp exper married rur nrtheast nrthcen south, re or
estimates store ri1

* I hate odds ratios, re not needed (is default)
xtlogit union educ black hisp exper married rur nrtheast nrthcen south
estimates store ri2

* compare fe, just for fun
xtlogit union educ black hisp exper married rur nrtheast nrthcen south, fe
estimates store fe1

* Check new function
meglm union educ black hisp exper married rur nrtheast nrthcen south || nr: , ///
   family(binomial) link(logit)
* Why diff from xtlogit?   
   
meglm union educ black hisp exper married rur nrtheast nrthcen south || nr: , ///
   family(binomial) link(logit) intp(12)
   
   
estimates store ri2

* 2.

* 3.
xtgee union educ black hisp exper married rur nrtheast nrthcen south, ///
  corr(exchangeable) link(logit) fam(binom) eform

* 4.

* 5.
xtsum union educ black hisp exper married rur nrtheast nrthcen south

* 6.
xtlogit union educ black hisp exper married rur nrtheast nrthcen south, fe or
estimates store fi

* 7. 


* 8.
hausman fi ri2

* 9.
xtprobit union educ black hisp exper married rur nrtheast nrthcen south, re 

meglm union educ black hisp exper married rur nrtheast nrthcen south || nr:, ///
    family(binomial) link(probit) intp(12)

meglm union educ black hisp exper married rur nrtheast nrthcen south || nr:, ///
    family(binomial) link(probit) intp(20)

