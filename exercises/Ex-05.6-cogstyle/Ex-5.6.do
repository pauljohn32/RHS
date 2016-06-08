* Paul E. Johnson
* 20160308

capture log close
set more off, permanently
log using Ex-5.6.log, replace text

* Cognitive style data (Broota, 1989)
* use http://www.stata-press.com/data/mlmus3/cogstyle.dta, clear
* saveold cogstyle.dta12, version(12) replace
use cogstyle.dta12, clear

* subj
* dv for field dependent
* rfn-rci word attributes

summarize


* 5.6.1 reshape

reshape long rf rc, i(subj) j(cue) string
reshape long r, i(subj cu) j(word) string


* 5.6.2 create words and cues variables, 1 2 3 
tabulate word
encode word, gen(words)
tabulate cue
encode cue, gen(cues)
recode cues 3=1 1=2 2=3
* new response variable
generate lnr = ln(r-134)

* 5.6.3. Box plot
graph box lnr, over(cues) over(words) asyvars by(dependent) legend(row(1))


* 5.6.4. random intercept with lots of interactions

xtmixed lnr i.dependent i.words i.cues i.words#i.dependent i.cues#i.dependent ///
    i.words#i.cues i.words#i.cues#i.dependent || subj:, mle nolstretch

* same as
xtmixed lnr i.words##i.cues##i.dependent || subj:, mle nolstretch


* 5.6.5 Test interactions

testparm words#cues#dependent

* Fit without that interaction
xtmixed lnr i.dependent i.words i.cues i.words#i.dependent ///
    i.cues#i.dependent i.words#i.cues || subj:, mle nolstretch

* same as
xtmixed lnr i.words##i.dependent i.cues##i.dependent/// 
    i.words##i.cues || subj:, mle nolstretch

* 5.6.6 Pairwise interactions at 0.05 level

testparm words#cues

testparm cues#dependent


testparm words#dependent

* Refit without insig interactions

xtmixed lnr i.dependent i.words i.cues i.words#i.cues || subj:, mle nolstretch

* 5.6.6.7 Remove main effects, strip down model rest of way

xtmixed lnr i.words i.cues i.words#i.cues || subj:, mle nolstretch

* margins is the thing I call predictOmatic in rockchalk
margins words#cues
marginsplot, xdim(cues)

