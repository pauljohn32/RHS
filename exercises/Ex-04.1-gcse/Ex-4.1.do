* Paul E. Johnson
* 20160216

capture log close
set more off
log using Ex-4.1.log, replace text

* London inner schools data
* use http://www.stata-press.com/data/mlmus3/gcse, clear
* saveold gcse.dta12, version(12)

use gcse.dta12

sum

* RHS p. 212
* schgend 1: mixed school 2: boys 3: girls

* Everything is better with labels
label define schtype 1 "mixed" 2 "boys only" 3 "girls only", replace
label values schgend schtype
tab schgend


* From RHS p. 212
mixed gcse i.schgend##c.lrt || school: lrt, covariance(unstructured) mle

* I noticed mixed has different output format than xtmixed
* Also prevent printing of ridiculous se on random effects
mixed gcse i.schgend##c.lrt || school: lrt, /// 
   covariance(unstructured) mle stddev nostderr



* Make some plots. 

egen pickone = tag(school)

twoway (scatter gcse lrt), by(schgend)

twoway (scatter gcse lrt) , by(schgend, compact legend(off))
gsort +school +lrt
predict gcsefit, fitted
* Note difference from "fitted" and "xb" there.  "fitted" includes
* random effects

* Trellis plot
twoway (scatter gcse lrt, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(school, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))

* omit the by for spaghetti
twoway (scatter gcse lrt)(line gcsefit lrt, connect(ascending)), ///
		xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))


twoway (scatter gcse lrt, msize(small) msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
* I'm sorry to say that Stata makes this so tedious to color code the 
* dots with the school type that I'm stopping in anger.



* Goal: create a trellis plot for a subset of the groups
* This is the method in RHS p. 237. Creates a marker variable for the group
* then assigns random numbers to groups, then sorts, then creates numerical
* rank. 
* create 1 in first row for each school
* egen pickone = tag(school) 
set seed 234234
gen r = uniform() if pickone == 1
egen num = rank(r) if r < . 
egen number = mean(num), by(school)

* Here's a partial fail
twoway (scatter gcse lrt if number<=6, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(school, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
* Still a fail:
twoway (scatter gcse lrt if number<=6, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt if number<=6, connect(ascending)), ///
    by(school, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
    
* keep if number <= 6
* twoway (scatter gcse lrt, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
*    (line gcsefit lrt, connect(ascending)), ///
*    by(school, compact legend(off)) ///
*    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
* Oops. destroyed data. How to avoid?
* run this as a block
preserve
keep if number <= 6
twoway (scatter gcse lrt, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(school, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
 
preserve
keep if number <= 6 & number <= 12
twoway (scatter gcse lrt, msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(school, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
    
    
    
* Trellis plot.
twoway (scatter gcse lrt) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(schgend, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
                                               
twoway (scatter gcse lrt, ///
    msize(small) msymbol(smcircle_hollow) mlwidth(vvthin)) ///
    (line gcsefit lrt, connect(ascending)), ///
    by(schgend, compact legend(off)) ///
    xtitle(LRT) ytitle (GCSE) legend(order(1 "observed" 2 "predicted"))
                                               

* 4.1.2 
* Asks us to consider girl as a fixed predictor that interacts with the
* school type. Of course, the bad problem is that we have all girls and 
* all boys schools, and we can't look for an interaction there. We can
* only look in the mixed ones. 

* Insert girl as dichotomous predictor to get started
mixed gcse i.schgend##c.lrt i.girl || school: lrt, ///
    covariance(unstructured) mle

* Output from this makes you think you made an error because there are
* so many aliased coefficients
mixed gcse i.schgend##c.lrt##i.girl || school: lrt, ///
    covariance(unstructured) mle stddev
 
* That's really messy.   Too many aliased coefficients.
* It looks to me like you have to piece that together.
* The slope for "lrt" is the mixed schools slope for boys

*  To clean it up, manufacture some dummy variables for school type.

* Basically, we need to get 4 lines out:
* boys schools (intercept and lrt slope)
* girls schools (intercept and lrt slope)
* boys in mixed schools (just intercept, not diff slope of lrt)
* girls in mixed schools (ditto).
* a slope estimate for lrt in mixed schools.  Note question does not
* say allow slope to differe between boys and girls within mixed schools.

* Is just dummy coding nonsense.
 
* RECODE/Create dummy variables
* Here's the smart way to way to create dummies for a categorical variable
* RHS p. 43: This creates schgend1 schgend2 schgend3
tab schgend, generate(schgend)
* Don't do this
* gen mixedsch = schgend
* recode mixedsch 1=1 2=0 3=0
* gen boyschool = schgend
* recode boyschool 1=0 2=1 3=0

generate boy = 1-girl

mixed gcse i.schgend1#i.girl i.schgend2 i.schgend3 ///
    lrt i.schgend2#c.lrt i.schgend3#c.lrt ///
    || school: lrt, covariance(unstructured) mle stddev
* Which cases are "in" is the constant in that model?

* Another way to fit: suppress the constant
mixed gcse i.schgend1#i.girl i.schgend2 i.schgend3 ///
    lrt i.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev

* RHS use lincom, which I can't get to work with factor variables.

* They ask if girls do better in same sex school, why not
* estimate that directly

mixed gcse i.girl i.schgend1#i.girl i.schgend2 ///
    lrt i.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev

* Can use same trick to see boy difference in 2 types of schools
mixed gcse i.boy i.schgend1#i.boy i.schgend3 ///
    lrt i.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev


mixed gcse i.schgend1#c.lrt i.schgend2#c.lrt c.lrt i.schgend1 ///
   i.girl#i.schgend1 i.girl, noconstant || school: lrt, /// 
   covariance(unstructured) mle 


* I wondered why we did not check for lrt slope difference between
* boys and girls within mixed schools. How to do that?


mixed gcse  i1.schgend1#i.girl i1.schgend2 i1.schgend3 ///
    c.lrt#i1.schgend1#i.girl ///
    i1.schgend2#c.lrt i1.schgend3#c.lrt, /// 
    noconstant ///
    || school: lrt, covariance(unstructured) mle stddev

* Inserting the i1. notation cleaned up the output quite a bit.

* Before I realized that would work, I was following the RHS strategy
* of manually recoding interactions.
generate s1boy = schgend1 * (1 - girl)
generate s1girl = schgend1 * girl

xtmixed gcse  i.s1girl i.s1boy  i.schgend2 i.schgend3 ///
    c.lrt#i.s1girl c.lrt#i.s1boy i.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev 


* That still kinda messy. Manufacture more dummies to un-confuse Stata
generate s1boylrt = lrt*boy*schgend1
generate s1girllrt = lrt*girl*schgend1
xtmixed gcse  i.s1girl i.s1boy  i.schgend2 i.schgend3 ///
     s1boylrt s1girllrt i1.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev 

* Maybe leave lrt in, treat all variationts against it
xtmixed gcse  i.s1girl i.s1boy  i.schgend2 i.schgend3 ///
     lrt s1girllrt i1.schgend2#c.lrt i.schgend3#c.lrt, noconstant ///
    || school: lrt, covariance(unstructured) mle stddev 



clear all
use gcse.dta12

* HOW RHS did their homework:
tabulate schgend, generate(w)
rename w2 boys
rename w3 girls
generate boys_lrt = boys*lrt
generate girls_lrt = girls*lrt
xtmixed gcse lrt boys girls boys_lrt girls_lrt || school: lrt, ///
   cov(unstructured) mle

generate boy = 1-girl
rename w1 mixed
generate boy_mixed = boy*mixed
generate girl_mixed = girl*mixed
xtmixed gcse boy_mixed girl_mixed boys girls lrt boys_lrt girls_lrt, ///
   noconstant || school: lrt, cov(unstructured) mle
lincom girls - girl_mixed
