* Assign workers to different groups by labor market attachment
*
*	- Does sample selection 
*
* Input: panel0513_clean
* Output: 	panel0513_main
* ------------------------------------------------------------------------------
local InputFile "$DataDir/panel0514_clean.dta"
local OutDir "$StataOut"
set more off
cd `OutDir'
use `InputFile', clear


g dummyU = 0
replace dummyU = 1 if WS<mw*30*4
replace dummyU = 1 if WS==. | WS==0
replace dummyU = 1 if UI>WS & days_t<6*30 


* Define employed vs unemployed
g work = 1-dummyU
g lrearns = log(rearns) 
replace lrearns=. if work!=1
replace rearns=. if work!=1


* ---------------------------------------
* SAMPLE SELECTION
* ---------------------------------------

if "$CountObs"=="Y" {
	qui count
	mat nobs = r(N)
	mat rownames nobs= "Begin Selection with"
}

* --- 1. Age 
drop if age<$minage | age>$maxage
drop if age<22 & edgroup==4

if "$CountObs"=="Y" {
	qui count
	mat nobs0 = r(N)
	mat rownames nobs0 = "Age between $minage and $maxage"
	mat nobs= nobs \ nobs0
}


* --- 5. no outliers
_pctile rearns, p(99.9)
drop if rearns>=r(r1) & rearns<.
_pctile rearns, p(0.01)
drop if rearns<=r(r1)

if "$CountObs"=="Y" {
	qui count
	mat nobs0 = r(N)
	mat rownames nobs0 = "Drop top and bottom 0.1%"
	mat nobs= nobs \ nobs0
}


* --- 3. No info on type of contract
drop if Ptemporal==. & dummyU==0

if "$CountObs"=="Y" {
	qui count
	mat nobs0 = r(N)
	mat rownames nobs0 = "Drop if missing contract type"
	mat nobs= nobs \ nobs0
}

* ----- Done with core sample sel. ------
* ---------------------------------------

* ---------------------------------------
* DEFINE INSTABILITY STATUS
* ---------------------------------------
rename Ptemporal_LT LTPtemporal

g LTtemp=(LTPtemporal>50 & LTPtemporal<.)
label var LTtemp "Lifetime-unstable worker"

g Ytemp=(Ptemporal_Y>50 & Ptemporal_Y<.)
label var Ytemp "Young-unstable worker"

if $SampleStable==1{
	*keep if LTtemp==0
	keep if Ytemp==0

	if "$CountObs"=="Y" {
		qui count
		mat nobs0 = r(N)
		mat rownames nobs0 = "Keep if Stable"
		mat nobs= nobs \ nobs0
	}
}
if $SampleStable==2{
	*keep if LTtemp==1
	keep if Ytemp==1

	if "$CountObs"=="Y" {
		qui count
		mat nobs0 = r(N)
		mat rownames nobs0 = "Keep if Unstable"
		mat nobs= nobs \ nobs0
	}
}

* ------- Done with LT/LP groups --------
* ---------------------------------------

label data "MCVL 2005-2014 -- main for analysis, stability groups defined"	
saveold "$DataDir/panel0514_main.dta", replace
