* PrepareData
*
*	- Cleans original data (inconsistencies and duplicates, mostly)
*	- Counts number of observations dropped in each step
*	- Asigns wokers to education groups
* 	- Generates FE to be used in the 1st Stage regression of earnings
*
* Input: MCVLfinal2005-2014
* Output: panel0514_clean
*
* Most of these steps take very long and are common to all analyses. 
* It is convenient to only run it once and use panel0513_clean.dta 
* as main data input in the subsequent codes.
*
* R.Madera
* Last revised: 1/21/2019
* ------------------------------------------------------------------------------

set more off
pause off

local InputFile "$DataFile"
local OutDir "$StataOut"
cd `OutDir'
use `InputFile',clear

if "$CountObs"=="Y" {
	qui count; mat nobs = r(N); mat rownames nobs= "Begin with"
}

* Define main person id
g pid=ident
label var ident "Person ID"
g age=anomcvl-ynacim

* ---------------------------------------
* Create education groups
* ---------------------------------------
* Export distribution over original education groups
if "$ComputeSumStats"=="Y" latab educa, replace tf($StataOut/educaorig)

g edlev=1 if educa>=2 & educa<4
replace edlev=2 if educa==4
replace edlev=3 if educa==5
replace edlev=4 if educa==6
label values edlev edgroup

* Drop if education missing or unknown
qui drop if edlev==.

if "$CountObs"=="Y" {
	qui count; mat nobs0 = r(N)
	mat rownames nobs0 = "Education missing"
	mat nobs= nobs \ nobs0
}	

* Export final distribution over newly defined education groups
if "$ComputeSumStats"=="Y" latab edlev, replace tf($StataOut/edlev)

* Define each individual's education level as the highest attained by age 22 
* the variable in the MCVL is already the highest attained anyway
g tempage = age-$mingradage if age>=$mingradage & edlev~=.
sort pid tempage
bys pid: g edgroup = edlev[1]

* ------ Done with education group variable ---------

* ---------------------------------------
* Last sample cleanings and export observations
* ---------------------------------------
* Drop duplicates in terms of all variables (some people appeared twice with 
* exactly the same information)
duplicates drop

if "$CountObs"=="Y" {
	qui count
	mat nobs0 = r(N)
	mat rownames nobs0 = "Drop duplicates"
	mat nobs= nobs \ nobs0
	mat colnames nobs= "Observations Remaining"

	* Export matrix of observations
	putexcel A1=matrix(nobs, names) using $StataOut/countN, modify sheet("Cleaning", replace)
	*outtable using $StataOut/nobs_case,mat(nobs) caption("Number of observations") nobox replace
	estout matrix(nobs)
}

* ------ Done with sample selection ---------
noi display "Done with cleaning"

* ---------------------------------------
* Generate controls for first stage regression
* ---------------------------------------

* -- Region FE (I use zip code, but we could change to prov)
recode dom (0=.)
rename dom region 									
qui xi i.region, prefix(DR) noomit 
drop DRregion_28079			// omitted is inner Madrid=28079

* -- Year FE
qui xi i.anomcvl, prefix(DY) noomit	
drop DYanomcvl_2013			// omitted is 2013

* -- Quartic in Age
g age2=age^2
g age3=age^3
g age4=age^4

* ---------------------------------------
* And some extra variables relevant for sample selection
* ---------------------------------------
// Minimum wage
g mw=.
replace mw=	21.51	if anomcvl==	2014
replace mw=	21.51	if anomcvl==	2013
replace mw=	21.38	if anomcvl==	2012
replace mw=	21.38	if anomcvl==	2011
replace mw=	21.11	if anomcvl==	2010
replace mw=	20.8	if anomcvl==	2009
replace mw=	20		if anomcvl==	2008
replace mw=	19.02	if anomcvl==	2007
replace mw=	18.03	if anomcvl==	2006
replace mw=	17.1	if anomcvl==	2005


* ---------------------------------------
* Define real earnings
* ---------------------------------------

* Main earnings variable - choose or construct (see PDF for an explanation)
g reyr = WS // annual earnings

* Define real earnings base 2011
merge m:1 anomcvl using $DataDir/ipc11.dta, keepusing(anomcvl ipc11) nogen
g rearns=100*reyr/ipc11

label data "MCVL 2005-2014 -- clean"	
saveold "$DataDir/panel0514_clean.dta", replace
