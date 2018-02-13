* Paul E. Johnson
* 20180213

capture log close
set more off
log using "Ex-01.3-smoking.log", replace text

use smoking.dta12, clear

keep if idx==1

generate education = hsgrad + 2*somecoll + 3*collgrad

table smoke education, contents(mean birwt sd birwt) by(male black)


graph box birwt, over(education) over(black) over(smoke) by(male) asyvars nooutsides


count if black ==1 & male ==1 & smok ==1 & somecoll==1


regress birwt smoke


regress birwt smoke mage male black hsgrad somecoll collgrad


regress birwt i.smoke mage male black i.education

margins smoke#education, at(male=0 black=0 mage) nolstretch
