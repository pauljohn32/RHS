
* Paul Johnson
* 20160411

capture log close
set more off, permanently
log using Ex-10.4.log, replace text



* pisa international education 
use http://www.stata-press.com/data/mlmus3/pisaUSA2000, clear
saveold pisaUSA2000.dta12, version(12) replace

gllamm pass_read female isei high_school college test_lang  ///
   one_for both_for, i(id_school) link(logit) family(binom) adapt 

* 2.
egen mn_isei = mean(isei), by(id_school)
matrix a = e(b)

gllamm pass_read female isei mn_isei high_school college test_lang  ///
   one_for both_for, i(id_school) link(logit) family(binom) from(a) adapt 
estimates store gllamm1

meglm pass_read female isei high_school college test_lang  ///
   one_for both_for || id_school: , link(logit) family(binom) intp(12) 
estimates store meglm1

est tab gllamm1 meglm1, se

* 3.
* 4.

* 5.
estimates restore gllamm1
gllamm, robust
estimates store gllamm1rob
est tab gllamm1 gllamm1rob meglm1, se


* 6.
estimates restore gllamm1rob
generate one = 1
eq inter: one
eq slope: isei

matrix a=e(b)
* inserted one more 0 than RHS answer manual
matrix a = (a, 0, 0, 0)

gllamm pass_read female isei mn_isei high_school college test_lang  ///
   one_for both_for, i(id_school) link(logit) family(binom) adapt from(a) ///
   copy nrf(2) eqs(inter slope)
estimates store glamm2rc

lrtest gllamm1 gllamm2rc

* 7.

** a.
egen mnw = mean(w_fstuwt), by(id_school)

** b.
generate wt1 = w_fstuwt/mnw
rename wnrschbw wt2

** c.
matrix a=e(b)
gllamm pass_read female isei mn_isei high_school college test_lang  ///
   one_for both_for, i(id_school) link(logit) family(binom) from(a)  ///
   pweight(wt) adapt 
   
** d.

