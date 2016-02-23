* Paul E. Johnson
* 20160202
capture log close
set more off
log using 3.1.log, replace text


*use http://www.stata-press.com/data/mlmus3/neighborhood, clear
*save neighborhood.dta

use neighborhood.dta

xtmixed attain || neighid:  , mle
estimates store empty

xtmixed attain deprive || neighid: , mle
estimates store l2re

xtmixed attain deprive, mle
estimates store l2nore
lrtest l2re l2nore

xtmixed attain p7vrq p7read dadocc dadunemp daded momed male deprive || neighid: , mle
estimates store l1l2re

xtmixed attain p7vrq p7read dadocc dadunemp daded momed male deprive
estimates store l1l2nore

lrtest l1l2re l1l2nore

xtmixed attain p7vrq p7read dadocc i.dadunemp i.daded i.momed i.male deprive || neighid: , mle

