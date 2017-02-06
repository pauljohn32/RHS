* Paul E. Johnson
* 20160202
capture log close
set more off, permanently
log using "3.1.log", replace text


use neighborhood.dta12

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



* After I worked those out in 2017, I learned that the preferred method
* in newer Stata is the mixed function.

mixed attain || neighid:  , mle
estimates store empty2

xtmixed attain deprive || neighid: , mle
estimates store l2re2

xtmixed attain deprive, mle
estimates store l2nore2
lrtest l2re l2nore2
