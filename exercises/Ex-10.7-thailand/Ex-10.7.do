* Paul Johnson
* 20160411

capture log close
set more off, permanently
log using Ex-10.7.log, replace text

* use http://www.stata-press.com/data/mlmus3/thailand, clear
* saveold thailand.dta12, version(12) replace
use thailand.dta12, clear

* Schools in Thailand

* 1.
gllamm rep male pped mses, i(school) link(logit) family(binom) weight(wt) adapt
estimates store gllamm1

meglm  rep male pped mses || school: , family(binom) link(logit)

* gllamm weight is frequency weights
meglm  rep male pped mses p[fw=wt]|| school: , family(binom) link(logit) 
estimates store meglm


* Compare estimates quickly
est tab meglm gllamm1

est tab meglm gllamm1, se


* 2.
estimates restore gllamm1
gllamm, eform


* 3.
estimates restore gllamm1
gllapred mu, mu
gllapred mumarg, mu marginal
gllapred xb, xb
gllapred latent, u




* 3.(a)
list schoolid rep male pped wt1 mu if schoolid==10104
list schoolid rep male pped wt1 mu if schoolid==10105

* 3. (b)
graph box mu if rep==0, by(male) over(pped)

margins


margins, at(male = (0 1)) 
marginsplot
