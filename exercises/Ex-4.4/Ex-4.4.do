* Paul E. Johnson
* 20160220

capture log close
set more off, permanently
log using Ex-4.4.log, replace text

* Wheat data
* use http://www.stata-press.com/data/mlmus3/wheat, clear
* saveold wheat.dta12, version(12) replace
use wheat.dta12

* Comment: This data has the smallest error variance I've ever seen
* in supposedly "real" empirical data.



* 4.4.1


* 4.4.2

* May need several models. First, just fixed effects

xtset variety
xtreg yield moist, mle
estimates store xt1

* Can use mixed to fit model with no random effects
mixed yield moist, mle stddev
estimates store m1

* Can't figure out why that slightly different from this
mixed yield moist || variety: , mle  stddev
estimates store m2
lrtest m1 m2

* Here's the one the question asks for
mixed yield moist || variety: moist, mle covariance(unstructured)  stddev
estimates store m3

* 4.4.3
lrtest m2 m3
* That is difficult to believe. Wait till you see plots

* Considered blowing this up by excluding the covariance, but not real reason
mixed yield moist || variety: moist, mle  stddev


* 4.4.4
predict b0 b1, reffects

egen pickone = tag(variety)

hist b0 if pickone == 1
hist b1 if pickone == 1, bin(6)
twoway(scatter b0 b1)




* RHS p. 203 "fitted" includes predicted random effects
predict yieldfit, fitted
gsort +variety +moist 



* Start by exploring a Trellis plot of observed scores
* The by(variety) gives Trellis plot
twoway (scatter yield moist) , by(variety) 
* Restyle
twoway (scatter yield moist) , by(variety, compact legend(off)) 

twoway (scatter yield moist) , by(variety, compact legend(off) cols(5)) 


* Insert predicted values from mixed model
twoway (scatter yield moist)(line yieldfit moist, connect(ascending)), ///
            by(variety, compact legend(off) cols(5)) 


twoway (scatter yield moist)(line yieldfit moist, connect(ascending)), ///
            by(variety, compact legend(off) cols(5)) xtitle(Soil Moisture) ///
	    ytitle (Yield) 
	    
	    

twoway (scatter yield moist)(line yieldfit moist, connect(ascending)), ///
            xtitle(Soil Moisture) ///
	    ytitle (Yield) legend(order(1 "observed" 2 "predicted"))
	    
