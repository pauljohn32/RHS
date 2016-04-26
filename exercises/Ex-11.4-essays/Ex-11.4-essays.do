* Paul Johnson
* 20160426

capture log close
set more off, permanently
log using Ex-11.4.log, replace text

* use http://www.stata-press.com/data/mlmus3/essays, clear
* saveold essays.dta12, version(12) replace
use essays.dta12, clear
 
 


* 1.

recode grade 1/2=1 3/4=2 5/6=3 7/10=4

meglm grade || essay: , family(ordinal) link(probit)
estimates store meglm1

* gllamm grade, i(essay) link(oprobit) adapt

* 2.

tabulate grader, generate(gr)

meglm grade i.grader || essay: , family (ordinal) link(probit)
estimates store meglm2a

lrtest meglm1 meglm2a


meglm grade gr2-gr5 || essay: , family(ordinal) link(probit)


* matrix a = e(b)
* gllamm grade gr2-gr5, i(essay) link(oprobit) from(a) adapt
* estimates store mod2
* lrtest mod1 mod2



* 3.


meglm grade i.grader wordlength sqrtwords commas errors prepos sentlength ///
    || essay: ///
    , family (ordinal) link(probit)
estimates store meglm3

* matrix a=e(b)
* gllamm grade gr2-gr5 wordlength sqrtwords commas errors prepos ///
*  sentlength, i(essay) link(oprobit) from(a) adapt

* 4.

meglm grade i.grader##c.sqrtwords  wordlength sqrtwords commas errors ///
   prepos sentlength || essay: ///
    , family (ordinal) link(probit)
estimates store meglm4
lrtest meglm3 meglm4


* estimates store mod3
* gen gr2_s = gr2*sqrtwords
* gen gr3_s = gr3*sqrtwords
* gen gr4_s = gr4*sqrtwords
* gen gr5_s = gr5*sqrtwords
* matrix a=e(b)
* gllamm grade gr2-gr5 gr2_s-gr5_s wordlength sqrtwords commas errors prepos ///
*  sentlength, i(essay) link(oprobit) from(a) adapt
* estimates store mod4
* lrtest mod3 mod4
