* This code reads in output from fortran and creates a panel of simulated
* earnings for both males and females of a given education group `e'. 
* It then calculates lifetime earnings for each individual and ranks them
* in percentiles within sex and also in the total population of males and 
* females.
*
* The resulting panel is stored in simpanel_e`e'.dta.
*
*
* Rocio Madera
* Last: 12/20/2017
*---------------------------------------------------------------------------

clear all
cap log close
set more off 
pause off

global perc 100 // quantiles (100 for percentiles)
global qualityadj = 0

cd "$StataOut"


local e=4 // education, 4 is college grads
forvalues s = 1/2 { //sex
	global ModelDir = "${BaseDir}/fortran/simulations/sex_`s'/edgroup_`e'"
	
	************************
	di("import constant from first-stage income regression")
	************************
	preserve
	import delimited "${StataOut}/constantdistn_s`s'_e`e'.txt", clear case(preserve)
	qui des
	scalar ngroup = r(k)
	scalar constantdist_s`s'_e`e'1 = v1
	qui sum v2
	scalar constantdist_s`s'_e`e'2 = v2 / r(sum)
	restore

	************************
	di("Import age")
	************************
	preserve
	import delimited "${ModelDir}/agevec.txt", clear case(preserve)
	qui sum v1
	global nage = r(N)
	mkmat v1, matrix(age)
	restore
	
	************************
	di("import sim log earnings")
	************************
	preserve
	import delimited "${ModelDir}/ysim.txt", clear case(preserve)
	qui sum v1
	global nsim = r(N)
	mkmat v*, matrix(ysim)
	restore
	
	************************
	di("import sim earnings")
	************************
	preserve
	import delimited "${ModelDir}/expysim.txt", clear case(preserve)
	mkmat v*, matrix(expysim)
	restore
	
	************************
	di("import sim labor market status")
	************************
	preserve
	import delimited "${ModelDir}/uesim.txt", clear case(preserve)
	mkmat v*, matrix(uesim)
	restore
	
	*--------
	* CALCULATE LIFETIME INCOME PERCENTILES
	*--------
	mat statusM=uesim
	
	*adjust for correct distribution of constants
	mat groupsim = J($nsim,1,constantdist_s`s'_e`e'2) 
	mat constantsim = J($nsim,1,constantdist_s`s'_e`e'2)
	
	mat groupsim_mat = J($nsim,$nage,constantdist_s`s'_e`e'1)
	mat lrearnsM = ysim + groupsim_mat
	
	mat rearnsM = expysim*exp(constantdist_s`s'_e`e'1)
	
	forval i=1/$nsim {
		forval j=1/$nage {
			if statusM[`i',`j'] == 1 {
				mat lrearnsM[`i',`j'] = .
				mat rearnsM[`i',`j'] = .
			}
		}
	}
	clear		
	svmat rearnsM
	if $qualityadj== 1 {
		foreach v of varlist rearnsM* {
			replace `v'=1.1*`v'
		}
	}
	mata : st_matrix("lifetimeY", rowsum(st_matrix("rearnsM")))
	svmat lifetimeY


		* ----------------------------------------
		* Asign individuals to groups by percentile of lifetime income
		* ----------------------------------------
		preserve
		pctile PClifetime =  lifetimeY , nq($perc)
		qui sum lifetimeY
		replace PClifetime = r(max) in $perc
		drop if _n>$perc
		g perc = _n
		
		mkmat PClifetime 
		mkmat perc
		restore		
		
		g pcgroup = .
		replace pcgroup = perc[1,1] if lifetimeY<=PClifetime[1,1]
		forval i=2/$perc {
			local j=`i'-1
			replace pcgroup = perc[`i',1] if lifetimeY<=PClifetime[`i',1] & lifetimeY>PClifetime[`j',1]
		}

		g sex=`s'
	
		rename pcgroup pcgroup_s`s'
		label var pcgroup_s`s' "percentile group in the distribution within own sex"
		
	save simwide_s`s'_e`e', replace
}

use "${StataOut}/simwide_s1_e`e'",clear
append using "${StataOut}/simwide_s2_e`e'"
pause
* ----------------------------------------
* Asign individuals to groups by percentile of lifetime income
* ----------------------------------------
preserve
pctile PClifetime =  lifetimeY , nq($perc)
qui sum lifetimeY
replace PClifetime = r(max) in $perc
drop if _n>$perc
g perc = _n

mkmat PClifetime 
mkmat perc
restore		

g pcgroup = .
replace pcgroup = perc[1,1] if lifetimeY<=PClifetime[1,1]
forval i=2/$perc {
	local j=`i'-1
	replace pcgroup = perc[`i',1] if lifetimeY<=PClifetime[`i',1] & lifetimeY>PClifetime[`j',1]
}

label var pcgroup "percentile group in overall distribution (independently of sex)"
* Panel shape
g ind=_n
reshape long rearnsM, i(ind) j(age)

* Unemployed are imported with missing income, change to 0
replace rearnsM=0 if rearnsM==.

order ind age pcgroup 
sort ind age

order ind age sex rearnsM lifetimeY1
label var rearnsM "Real earnings at age a from simulation"
label var lifetimeY1 "Lifetime simulated real earnings"

label data "Panel of simulated earnings for both males and females with education e"
saveold "$StataOut/simpanel_e`e'", replace

erase simwide_s1_e`e'.dta
erase simwide_s2_e`e'.dta
