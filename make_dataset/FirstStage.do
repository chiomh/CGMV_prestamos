* extracts income residuals for covariance estimation and emp models
*
*	- Does sample selection 
*	- Defines real earnings (in 2011 euros)
*	- Does first stage to extract income residuals for estimation
*
* Input: panel0513_clean
* Output: 	panel0513_sampleA (for All workers)
*		panel0513_sampleP (for only Permanent workers)
*		panel0513_sampleT (for only Temporary workers)
* ------------------------------------------------------------------------------
local InputFile "$DataDir/panel0514_main.dta"
local OutDir "$StataOut"
set more off
cd `OutDir'
use `InputFile', clear

if $OnlyPerm==0 cd All
if $OnlyPerm==1 cd Per
if $OnlyPerm==2 cd Tem

* ---------------------------------------
* FIRST STAGE
* ---------------------------------------

g y=.
forvalues s=1/2{ //sex
	forvalues e=1/4{ //educ group
		* Log(y) on age_quartic anomcvl and region FE, for each (s,e) group
		qui reg lrearns age* DYanomcvl* DRregion* if sex==`s' & edgroup==`e'
		cap drop temp
		predict temp if e(sample), res
		qui replace y = temp if e(sample)
		estimates store fs_`s'_`e'       
		
		* Frequency tables for fixed effects (to add back in graphs)
		preserve
		foreach v of varlist age* DY* DR* {
			qui replace `v' = 0
		}
		predict constant if e(sample)
		collapse (count) nobs=pid if e(sample), by(constant) fast
		outsheet using constantdistn_s`s'_e`e'.txt, replace non nol
		restore
		
	}
}

* ---------------------------------------
* EXPORT OBS COUNT AND 1st STAGE RESULTS
* ---------------------------------------	

* Export first stage results
estimates table fs*, se
estout fs* using "fsmodels.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4) order(_cons age age2 age3 age4) ty


if "$CountObs"=="Y" {
	* Save number of observations matrix
	qui count
	mat nobs0 = r(N)
	mat rownames nobs0 = "Final N for the analysis"
	mat nobs= nobs \ nobs0
	mat colnames nobs= "Observations Remaning"
	putexcel A1=matrix(nobs, names) using $StataOut/countN, modify sheet("Sample Selection $OnlyPerm")

	* count by groups
	local ct=0
	forvalues s=1/2{ //sex
		forvalues e=1/4{ //educ group
			qui count if sex==`s' & edgroup==`e'
			mat nobs0 = r(N)
			mat rownames nobs0 = "sex=`s', edgroup=`e'"
			if `ct'==0 mat nobs=  nobs0
			else mat nobs= nobs \ nobs0
	local ct=`ct'+1
		}
	}
	mat colnames nobs= "Obs in each group Sex x Edlevel"
	putexcel A1=matrix(nobs, names) using $StataOut/countN, modify sheet("FinalCount_bygroups $OnlyPerm")
}

* Declare panel (life-cycle)
xtset pid age

drop  ynacim days*

if $OnlyPerm==0 {
	label data "MCVL 2005-2014 - Selected Sample, All contracts"
	save ${DataDir}/panel0514_sampleA.dta, replace
}
else if $OnlyPerm==1 {
	label data "MCVL 2005-2014 - Selected Sample, Only Young Perm. Workers"
	save ${DataDir}/panel0514_sampleP.dta, replace
}
else {
	label data "MCVL 2005-2014 - Selected Sample, Only Young Temp. Workers"
	save ${DataDir}/panel0514_sampleT.dta, replace
}

* ----------- Done exporting ------------
* ---------------------------------------
