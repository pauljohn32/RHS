* Paul E. Johnson
* 20170404

capture log close
set more off, permanently
log using Ex-5.7.log, replace text

* Returns to schooling
use returns.dta12

summarize

xtset nr year
xtsum

xtsum exp-blk

*2 fixed intercept model

xtreg lwage exp exp2 wks occ ind south smsa ms union ed fem blk, fe
estimates store fe

* See the problem: all the variables go whoosh!

predict fe_b, u
predict fe_xb, xbu


*3 Random effect model

xtreg lwage exp exp2 wks occ ind south smsa ms union ed fem blk, re

estimates store re

predict re_b, u
predict re_xb, xbu

graph twoway scatter re_b fe_b
cor re_b fe_b
graph twoway scatter re_xb fe_xb
cor re_xb fe_xb

* From one point of view, one might say the two models
* are very similar.  The estimates of the group-level
* constants are very similar (r=0.86) and their predicted 
* values (xbu) are nearly identical. 
*
* And yet Hausman test will reject

*4 Hausman test

hausman fe re

*5 The model that is fit in the IM seems not to match the question.
* Here is what they have.
* Treat wks south smsa ms exp exp2 occ ind and union as exogenous.
* fem blk and ed are endogenous.

xthtaylor lwage exp exp2 wks occ ind south smsa ms union ed fem blk, ///
endog(exp exp2 wks ms union ed)

estimates store ht

*6 Check variable partition

hausman fe ht


*7 Amemiya-McCurdy variant

xthtaylor lwage exp exp2 wks occ ind south smsa ms union ed fem blk, ///
endog(exp exp2 wks ms union ed) amacurdy



* 8 Paul's question: does using the Mundlak procedure help?

foreach var of varlist exp exp2 wks occ ind south smsa ms union {
    egen `var'_mn = mean(`var'), by(nr)
}

foreach var of varlist exp exp2 wks occ ind south smsa ms union {
    gen `var'_dev = `var' - `var'_mn
}



xtreg lwage exp_mn exp_dev exp2_mn exp2_dev wks_mn wks_dev ///
  occ_mn occ_dev ind_mn ind_dev south_mn south_dev smsa_mn smsa_dev ///
   ms_mn ms_dev union_mn union_dev ed fem blk, re

estimates store re2

predict re2_b, u
predict re2_xb, xbu

graph twoway scatter re2_b fe_b
cor re2_b fe_b
graph twoway scatter re2_xb fe_xb
cor re2_xb fe_xb

* How to get Hausman test of the new re2 model.

hausman fe2 re2, alleqs constant

