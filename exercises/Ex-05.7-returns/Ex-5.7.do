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


*3 Random effect model

xtreg lwage exp exp2 wks occ ind south smsa ms union ed fem blk, re

estimates store re


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


