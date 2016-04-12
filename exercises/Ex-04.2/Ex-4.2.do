* Paul E. Johnson
* 20160216

capture log close
set more off, permanently
log using Ex-4.2.log, replace text

* High School and Beyond data
* Download OR use previously saved version
* use http://www.stata-press.com/data/mlmus3/hsb, clear
* saveold hsb.dta12, version(12) replace
use hsb.dta12


* For more about the HSB, see
* http://pj.freefaculty.org/guides/stat/DataSets/HSB
* Especially:
*    http://pj.freefaculty.org/guides/stat/DataSets/HSB/00-README.txt


* In Hierarchical Linear Models, 2ed, Raudenbush and Bryk make an
* extensive investigation of the High School and Beyond data. 

* Two areas of emphasis debate about their estimates project are
* 1. Substantive meaning of the predictor 
*    sesdev_ji = ses_ji - sesmean_j,
*    the deviations of individual SES about their school mean SES.
*
* There is widespread agreement that inclusion of sesdev_ji should
* also cause us to include sesmean_j in the model. There is a discussion
* of this in Rabe-Hesketh & Skrondal, 3ed, p. 151-155. Most MLM books
* include a discussion of this. "If one deicdes to center, the only
* advice we can give in this book is to add the subtracted mean to the
* model" (Ita Kreft and Jan De Leeuw, Introducing Multilevel Modeling,
* p. 191) See also Snijders and Bosker, 2ed, p. 88, where they 
* emphasize great caution about random slopes on group-centered predictors.
*
* 2. The model for Y includes sesdev_ji as a predictor, but
*    sesmean_j is a predictor in the intercept and slope formulae. This
*    results in a combined fitting model with an interaction
*    sesmean_j sesdev_ji 
*    which is extremely difficult to comprehend, since it is the school
*    mean SES times individual deviations about the school means.
*    See why it is baffling?
*    sesmean_j sesdev_ji = sesmean_j (ses_ji - sesmean_j)
*                        = sesmean_j ses_ji - sesmean_j* sesmean_j)

sum



* I don't trust the undocumented calculation columns in the hsb data, 
* easy enough to create new group mean and deviation SES
* Note my naming style. ses is the variable, the mean and group-level
* deviation are sesmean and sesdev. They stay together in the varlist.
egen sesmean = mean(ses), by(schoolid) 
gen sesdev = ses-sesmean

* The R&B p. 80 model, random intercept and slope given by 
* same formula
*
* They write 2 levels out using fixed effects gamma, but there's no need 
* to get exotic.

* PJ's way of doing the 2 stage model
* Y is mathach
* Y = Beta0star + Beta1star sesdev_ji + error_ji
* j is schoolid in hsb data.

* ## PJ: Whenever you need a fixed parameter, number next Beta
* ## PJ: I won't use gammas, no need for fresh set of letters.
* ## PJ: I use b0 and b1 for the random effects at j level

* The R&B formula for level 2 intercept and slope
* Beta0star = Beta0 + Beta2 sesmean_j + Beta3 sector_j + b0_j
* Beta1star = Beta4 + Beta5 sesmean_j + Beta6 sector_j + b1_j
* ## Insert into Y
* 
* Y = Beta0 + Beta2 sesmean_j + Beta3 sector_j + b0_j +
*      (Beta4 + Beta5 sesmean_j + Beta6 sector_j + b1_j) sesdev_ji + error_ji
* 
*   = Beta0 + Beta2 sesmean_j + Beta3 sector_j + b0_j +
*       Beta4 sesdev_ji + Beta5 sesmean_j sesdev_ji + 
*       Beta6 sector_j sesdev_ji +
*      
*   = Beta0 + Beta2 sesmean_j + Beta3 sector_j + 
*       Beta4 sesdev_ji + Beta5 sesmean_j sesdev_ji + 
*       Beta6 sector_j sesdev_ji +
*       {b0_j +   b1_j sesdev_ji  + error_ji}
*
* Hence, we need a model with a random slope on variable sesdev_ji


* Need to declare sesmean and sesdev as continuous when used in
* interactions, else you get this error:
* sesmean:  factor variables may not contain noninteger values
 

* Interesting to compare table R&B p. 82, p value.


mixed mathach c.sesmean i.sector c.sesdev c.sesmean#c.sesdev ///
     i.sector#c.sesdev || schoolid: c.sesdev, ///
     mle covariance(unstructured) stddev
estimates store mfull

* remove random slope
mixed mathach c.sesmean i.sector c.sesdev c.sesmean#c.sesdev ///
      i.sector#c.sesdev || schoolid: , ///
      mle covariance(unstructured) stddev
estimates store m1
lrtest mfull m1, stats
 
 
* Better run with covariance(independent) to make sure that is what we
* got when I realized my Oops!
mixed mathach c.sesmean i.sector c.sesdev c.sesmean#c.sesdev ///
    i.sector#c.sesdev || schoolid: c.sesdev, mle covariance(independent)
estimates store mind
lrtest mfull mind
 
 
* OOPS, could re-specify the model more compactly using ## notation,
* but this causes problem that Stata tries to insert sesdev into model 2
* times. Not an error, but a puzzling empty row in output

mixed mathach c.sesmean##c.sesdev ///
      i1.sector##c.sesdev || schoolid: c.sesdev, mle covariance(unstructured)
estimates store mfull2
 
* Notice the "sesdev" seems to be "omitted", but it really is not. 
* Only way I can find to specify that
mixed mathach c.sesmean##c.sesdev i.sector ///
      i.sector#c.sesdev || schoolid: c.sesdev, mle covariance(unstructured)
estimates store mfull3
* Confirm that same as mfull2
matrix list e(b)
* Here's a way to replay that stored object
mixed, variance



* mixed in Stata 14 defaults to report variance and covariance of
* random effects.  Can instead have standard deviation and correlation.
* No need to run again

estimates restore mfull
mixed, stddev
 
 
* One more thing. Most of us seem to agree that reporting standard
* errors and t-tests on random effect estimates is mistaken. Avoid seeing
* them:
mixed mathach c.sesmean i.sector c.sesdev c.sesmean#c.sesdev ///
      i.sector#c.sesdev || schoolid: c.sesdev, ///
      mle covariance(unstructured) nostderr
estimates store mfull




* 4.2.4 

* I've retooled this to eliminate the aliased coefficients.
mixed mathach c.sesmean##c.sesdev c.disclim c.sesdev#c.disclim ///
    i1.sector i1.sector#c.sesdev  minority || schoolid: ///
    c.sesdev, mle covariance(unstructured)
estimates store mdisc
