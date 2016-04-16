

* Paul E. Johnson
* 20160402

capture log close
set more off, permanently
log using toenail-1.log, replace text


use toenail.dta


xtset patient visit
xtdescribe if outcome < .

label define tr 0 "Itraconazole" 1 "Terbinafine"
label values treatment tr
graph bar (mean) proportion = outcome, over(visit) /// 
    by(treatment) ytitle(Proportion with onycholysis)


egen prop = mean(outcome), by(treatment visit)
egen mn_month = mean(month), by(treatment visit)
twoway line prop mn_month, by(treatment) sort xtitle(Time in months) ///
    ytitle(Proportion with onycholysis)
    
    
generate trt_month = treatment*month

* One marginal model: 
* Ignore individual effects in estimation
logit outcome treatment month trt_month, vce(cluster patient)
estimates store logit
logit, or    
   
predict logitprob, pr

twoway (line logitprob mn_month, sort) (line logitprob month, sort lpatt(dash)), ///
by(treatment) legend(order(1 "Observed proportions" 2 "Fitted probabilities")) ///
xtitle(Time in months) ytitle(Probability of onycholysis)
   

logit outcome i.treatment##c.month, vce(cluster patient)
estimates store logit2

margins

margins, at(month=(1/15)) atmeans

margins i.treatment, at(month=(1/15)) atmeans
marginsplot
translate @Graph "marginsplot-1.pdf", name("Graph")

* post 
margins i.treatment, at(month=(1/15)) atmeans post

margins, at(month=(1/15)) atmeans post   
* Caution: that obliterated the logit mode.

estimates restore logit2
margins i.treatment, at(month=(1/15)) atmeans
*margins, at(month=(1/15)) atmeans expression(predict(pr))
   
   
    
*xtlogit    
quietly xtset patient
* xtlogit only allows a random intercept, no random slope
* gaussian quadrature points is 30
xtlogit outcome i.treatment##c.month, intpoints(30)
estimates store xtlogit
xtlogit, or    

margins i.treatment, at(month=(1/15)) atmeans
marginsplot

predict xtlogitprob, pr


* xtmelogit
* Slower, more versatile. Numerical approximations
xtmelogit outcome treatment month trt_month || patient:, intpoints(30)
estimates store xtmelogit
predict xtmelogitpr, mu

meglm outcome treatment month trt_month || patient:, family(binom) ///
    link(logit) intpoints(30)

xtmelogit outcome i.treatment##c.month || patient:, intpoints(30)
estimates store xtmelogit2
predict xtmelogitpr2, mu

* gllamm
* If you don't have a recent gllamm
* ssc install gllamm, replace
gllamm outcome treatment month trt_month, i(patient) link(logit) /// 
    family(binomial) nip(30) adapt
estimates store gllamm
gllamm, eform

estimates restore gllamm
gllapred gllammeb, u
gllapred gllammmu, mu

estimates restore xtmelogit
predict xtmelogitebmodal, reffects
predict xtmelogitse2, reses
egen num0 = total(outcome==0), by(patient)
egen num1 = total(outcome==1), by(patient)
list patient num0 num1 gllammebm1 xtmelogitebmodal gllammebs1 xtmelogitse2 if visit==1&patient<=12, noobs



twoway (scatter xtmelogitebmodal gllammebm1 if visit==1, ///
        msize(small) msym(oh) mcol(black)) ///
    (function y=x, range(gllammebm1) lpatt(solid)), ///
    xtitle("gllamm prediction: mu") ytitle("xtmelogit prediction: mode") ///
    legend(off)


* Population averaged predicted probabilities

estimates restore gllamm
gllapred margprob, mu marginal

twoway (line logitprob month, sort) (line margprob month, sort lpatt(dash)), ///
by(treatment) legend(order(1 "Ordinary logit" 2 "Random-intercept logit")) ///
xtitle(Time in months) ytitle(Fitted marginal probabilities of onycholysis)


 

* following not quite right
twoway (line gllammmu month, connect(ascending)) ///
    (line margprob month, connect(ascending))

margins, at(month=(1/15)) atmeans asbalanced plot(__swapxp)

margins, predict(outcome(1)) at(month=(1/15)) atmeans asbalanced plot(__swapxp)	

margins , at(month=(1/15)) atmeans asbalanced plot(__swapxp)	

margins , at(month=(1/15) treatment=(1)) atmeans asbalanced expression(predict(xb))


* gllamm predictions of individual probabilties

generate zeta1 = 0
gllapred condprob0, mu us(zeta)
generate lower1 = -4
gllapred condprobm4, mu us(lower)
generate upper1 = 4
gllapred condprob4, mu us(upper)
replace lower1 = -2
gllapred condprobm2, mu us(lower)
replace upper1 = 2
gllapred condprob2, mu us(upper)

twoway (line prop mn_month, sort) ///
(line margprob month, sort lpatt(dash)) ///
(line condprob0 month, sort lpatt(shortdash_dot)) ///
(line condprob4 month, sort lpatt(shortdash)) ///
(line condprobm4 month, sort lpatt(shortdash)) ///
(line condprob2 month, sort lpatt(shortdash)) ///
(line condprobm2 month, sort lpatt(shortdash)), ///
by(treatment) ///
legend(order(1 "Observed proportion" 2 "Marginal probability" 3 ///
"Median probability" 4 "Conditional probabilities")) ///
xtitle(Time in months) ytitle(Probabilities of onycholysis)

