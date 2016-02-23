* Paul E. Johnson
* 20160220

capture log close
set more off, permanently
log using Ex-4.3.log, replace text

* Kreft and de Leew (1998) homework data
* Download OR use previously saved version
* use http://www.stata-press.com/data/mlmus3/homework, clear
* saveold homework.dta12, version(12) replace
* or
use homework.dta12








* 4.3.1

* math_ji = beta_0 + beta_1 homework_ji + beta_2 white_ji + beta_3 ratio
*           + b0_j + b1_j homework + e_ji


* 4.3.2

mixed math homework white ratio || schid: , mle
estimates store m0

mixed math homework white ratio || schid: homework, mle covar(unstructured)
estimates store m1

lrtest m1 m0


* 4.3.3
* variance of math conditional on predictors. That's just the variance
* of the combined errors.
*
* If only random intercept, easy:
* combined error is {b0_j + e_ji}.
* Var(b0_j) + Var(e_ji) + 2 Cov(b0_j, e_ji)
* Assert Cov(b0_j, e_ji) = 0
*
* Random slope is more work. See RHS p. 191. I refuse to write zeta, though :)
* Combined error is {b0_j + b1_j homework + e_ji}
* Apply the variance law to terms grouped like this (b0_j + b1_j homework) + e_ji)
* Var(b0_j + b1_j homework + e_ji) = Var(b0_j + b1_j homework) + Var(e_ji)
*   + 2 Cov(b1_j, e_ji)
* < Last term is equal to 0 because b's and e are uncorrelated>*
*  Now solve
*  Var(b0_j + b1_j homework) + Var(e_ji)
* = Var(b0_j)  + homework^2 Var(b1_j) + 2 homework Cov(b0_j, b1_j) + Var(e_ji)



* 4.3.4
*  math = beta*_0 + beta*_1 homework + beta_2 white + beta_3 ratio + e
*
*  beta*_0 = beta_0 + b0_j
*  beta*_1 = beta_1 + beta_5 meanses + b1_j
*
*  math = beta_0 +  (beta_1 + beta_5 meanses + b1_j) homework + 
*         beta_2 white + beta_3 ratio + b0_j + e
*
*       = beta_0 +  beta_1 homework + beta_5 meanses homework + beta_2 white
*                + beta_3 ratio + b0_j  + b1_j homework +  e
*


*4.3.5

mixed math homework c.homework#c.meanses white ratio || schid: homework, mle
estimates store m3


* I think we need to insert main effect of meanses
mixed math c.homework##c.meanses white ratio || schid: homework, mle


estimates store m4

egen pickone = tag(schid)
predict eb0 eb1, reffects

predict m4hat, fitted
sort schid homework
predict b0_se b1_se, reses

* what is this? see p. 208. read help gsort

gsort + eb0 - pickone
generate rank = sum(pickone)

serrbar eb0 b0_se rank
* See how to elaborate that p. 208
gen labpos = eb0 + 1.96 * b0_se + 1
serrbar eb0 b0_se rank, scale (1.96) ///
addplot(scatter labpos rank, mlabel(schid) msymbol(none) mlabpos(0))




gsort + eb1 - pickone
generate rank_b1 = sum(pickone)

serrbar eb1 b1_se rank_b1
* See how to elaborate that p. 208
gen labpos_b1 = eb1 + 1.96 * b1_se + 1
serrbar eb1 b1_se rank_b1, scale(1.96) ///
addplot(scatter labpos_b1 rank_b1, mlabel(schid) msymbol(none))

* Don't understand why following fails, don't know rules on "m" notation
* serrbar eb1 b1_se rank_b1, scale(1.96) ///
* addplot(scatter labpos_b1 rank_b1, mlabel(schid) msymbol(none) mlabpos_b1(0))



* Horrible
hist eb0 if pickone==1
hist eb1 if pickone==1

* Can't make spaghetti plot unless we deal with white, ratio
* See the problem? Its something I deal with in rockchalk plotSlopes a lot
mixed math c.homework##c.meanses || schid: homework, mle
predict m4hat2, fitted
twoway(line m4hat2 homework, connect(ascending))




