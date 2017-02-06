* Paul E. Johnson
* 20170202

capture log close
set more off
log using "Ex-3.2.log", replace text

* Joop Hox's data
use gpa.dta12, replace

* Stack.
reshape long gpa job, i(student)
rename _j time1

xtmixed gpa time1 i.job highgpa i1.sex || student: , mle
estimates store gpa1

* Linearity of i.time1
xtmixed gpa i.time1  highgpa i1.sex || student: , mle
estimates store gpaitime1
lrtest gpaitime1 gpa1



* Linearity highgpa
hist highgpa
table highgpa

gen highgpasq = highgpa^2
xtmixed gpa time1  highgpa highgpasq i1.sex || student: , mle
estimates store gpahighgpasq

lrtest gpahighgpasq gpa1
predict lev2, reffects
predict comp_se, reses

mkspline hsgpa = highgpa, cubic 
xtmixed gpa time1  hsgpa1 hsgpa2 hsgpa3 hsgpa4 i1.sex || student: , mle
estimates store gpahsgpaspline
lrtest gpahsgpaspline gpa1


xtmixed gpa i.time1##i1.sex highgpa i1.sex || student: , mle
estimates store gpaint1
lrtest gpa1 gpaint1

estimates restore gpa1
predict lev2, reffects
hist lev2

predict compse, reses
generate diag_se = sqrt(exp(2*[lns1_1_1]_cons) - compse^2)
generate lev2std = lev2/diag_se

twoway (scatter lev2std lev2)
* want to run RHS p. 161, but idx missing
histogram lev2 if idx==1, normal
egen pickone = tag(student)
