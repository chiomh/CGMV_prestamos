* Emp - employment models
*
* This code estimates transition probabilities between employment statuses
* (permanent, temporary, and unemployed) and the corresponding estimated
* earnings in the new status
*
* In particular, it follows the steps
* 	1. create Working Status
* 	2. Compute transitions
* 	3. Change of status earnings distribution
* 	4. Export sum stats for estimation and model fit
* 	5. Employment models 1: transition probabilities
* 	-- 5.1. "Downgrades", these probits depend on lagged earnings and age
* 	-- 5.2. "Upgrades", these probits depend on duration of spell and age
* 	6. Employment models 2: "Reentry" (change of status) earnings
* 	-- 6.1. Reentry earnings, just as a function of duration in the previous spell 
* 	-- 6.2. Reentry earnings, including last earnings
* 	7. Save and export model estimates to be used in Fortran
*
* R.Madera (code adapted from Dearden et al)
* Last: 9/10/2015
* ------------------------------------------------------------------------------
set more off
local InputFile "$DataDir/panel0514_sampleA.dta"
local OutDir "$StataOut/All"
cd `OutDir'
use `InputFile', clear

* --------------------------------------------------
* 1. create Working Status
* --------------------------------------------------

* 3 states: unemployed (0), permanent (1), tempo (2) 

g contractT=(Ptemporal>50)

g workStat=0 if work==0
replace workStat=1 if work==1 & contractT==0
replace workStat=2 if work==1 & contractT==1

label define workStat_lab 0 "Unemployed" 1 "Permanent" 2 "Temporary"
label val workStat workStat_lab

* --------------------------------------------------
* 2. Compute transitions
* --------------------------------------------------
* We want transition matrix M of the form
*
*	U'     UU	UP	 UT
*     ( P' )=( PU	PP	 PT ) * (U P T)
*	T'     TU	TP	 TT

forval statL=0/2 {
	forval stat=0/2 {
		g trans`statL'`stat'=0 if L.workStat==`statL'
		replace trans`statL'`stat' = 1 if trans`statL'`stat'==0 & workStat==`stat'
	}
}

* --------------------------------------------------
* 3. Change of status earnings distribution
* --------------------------------------------------
* "Entry" earnings by transition group (including stayers)
* i.e. earnings at t of indiv with status "stat" that was statL in t-1

forval statL = 0/2 {
	forval stat = 1/2 {
		* Log real earnings
		g lentryrearns`statL'`stat' = lrearns if L.workStat==`statL' & workStat==`stat'
		* Post 1st stage
		g entry_y`statL'`stat'  = y if L.workStat==`statL' & workStat==`stat'
	}
}

* "Exit" earnings distribution 
* i.e. earnings at t of indiv with status "stat" that will be statF in t-1
* NOTE: we are not going to use this one for the employment models

forval stat = 1/2 {
	forval statF = 0/2 {
		g lexitrearns`stat'`statF' = lrearns if workStat==`stat' & F.workStat==`statF'
		g exit_y`stat'`statF'  = y if workStat==`stat' & F.workStat==`statF'
	}
}

* --------------------------------------------------
* 4. Export sum stats for estimation and model fit
* --------------------------------------------------

* Generate indicator variables to export transtions
g perm  = (workStat==1)
g tempo = (workStat==2)
cap drop work
g work = (perm==1 | tempo==1)

#delimit;
forvalues s = 1/2 {;
	forvalues e = 1/4 {;
   		preserve;
		collapse (mean)	work perm tempo trans* lrearns rearns lentryrearns*
				lexitrearns* entry_y* exit_y* y /*Dy y2 Dy2 yL*y DyL*Dy*/
			 (sd)	Vlrearns = lrearns 
				Vrearns = rearns
				VlentryearnsUP=lentryrearns01
				VlentryearnsTP=lentryrearns21
				VlentryearnsUT=lentryrearns02
				VlentryearnsPT=lentryrearns12
				VlexitearnsPU=lexitrearns10
				VlexitearnsTU=lexitrearns20
		if sex==`s' & edgroup==`e', by(age);

   		foreach var of varlist V* {;
   			replace `var' = `var'^2;
   		};

   		outsheet using ageprofiles_s`s'_e`e'.txt, replace;
   		restore;
	};//e;
};//s;

#delimit cr

* --------------------------------------------------
* 5. Employment models 1: transition probabilities
* --------------------------------------------------
rename trans10 PU
rename trans20 TU
rename trans12 PT

rename trans01 UP
rename trans02 UT
rename trans21 TP

egen EU = rowtotal(PU TU) // 1 if emp to unemp
egen UE = rowtotal(UP UT) // 1 if unemp to emp

xtset pid age
g Ly = L.y
g Dy = y-Ly
g y2 = y^2
g Ly2 = L.y2
g Dy2 = y2-Ly2
noi display "Downgrades"
* -- 5.1. "Downgrades", these probits depend on lagged earnings and age
foreach transition in PU TU PT EU {
	forvalues s = 1/2 {
		forvalues e = 1/4 {
			probit `transition' Ly Ly2 age age2 age3 age4  if sex==`s' & edgroup==`e'			
			estimates store `transition'_`s'_`e'
		}
	}
}

* -- 5.2. "Upgrades", these probits depend on duration of spell and age
* Duration enters as a dummy for 1, 2, and more than 2 years non-employed.
noi display "UPgrades"
* we are going to need durations in the "bad" states
forvalues i = 1/2{
	g U_dur`i' = 0 if L.workStat==0
	g T_dur`i' = 0 if L.workStat==2
}
	g U_dur2g = 0 if L.workStat==0
	g T_dur2g = 0 if L.workStat==2
	
replace U_dur1 = 1 if L2.work == 1 & U_dur1 == 0
replace U_dur2 = 1 if L3.work == 1 & U_dur1 == 0 & U_dur2 == 0 
replace U_dur2g =1 if U_dur2==0 & U_dur1==0

replace T_dur1 = 1 if L2.workStat!=2 & T_dur1==0
replace T_dur2 = 1 if L3.workStat!=2 & T_dur1==0 & T_dur2==0
replace T_dur2g =1 if T_dur2==0 & T_dur1==0

foreach transition in UP UT UE {
	forvalues s = 1/2 {
		forvalues e = 1/4 {
			local tr=substr("`transition'",1,1)
			qui probit `transition' `tr'_dur1 `tr'_dur2 age age2 age3 age4 if sex==`s' & edgroup==`e'
			qui estimates store `transition'_`s'_`e'
		}
	}
}


foreach transition in TP {
	forvalues s = 1/2 {
		forvalues e = 1/4 {
			local tr=substr("`transition'",1,1)
			qui probit `transition' Ly Ly2 `tr'_dur1 `tr'_dur2 age age2 age3 age4 if sex==`s' & edgroup==`e'
			qui estimates store `transition'_`s'_`e'
		}
	}
}
* --------------------------------------------------
* 6. Employment models 2: "Reentry" (change of status) earnings
* --------------------------------------------------
noi display "Re-Entry earnings"
* -- 6.1. Reentry earnings, just as a function of duration in the previous spell 
* (spell = U or T)

forvalues s = 1/2 {
	forvalues e = 1/4 {
		* Estimated earnings for unemp just hired as temporary
   		qui reg y U_dur1 age age2 if sex==`s' & edgroup==`e' & L.workStat==0 & workStat==2 
   		cap drop temp
   		qui estimates store reUT_`s'_`e'
		* Estimated earnings for unemp just hired as permanent
		qui reg y U_dur1 age age2 if sex==`s' & edgroup==`e' & L.workStat==0 & workStat==1
   		cap drop temp
   		qui estimates store reUP_`s'_`e'
		* Estimated earnings for unemp just hired as permanent
		qui reg y U_dur1 age age2 if sex==`s' & edgroup==`e' & L.work==0 & work==1
   		cap drop temp
   		qui estimates store reUE_`s'_`e'
		* Estimated earnings for temporary just upgraded to permanent
   		qui reg y T_dur1 age age2 if sex==`s' & edgroup==`e' & L.workStat==2 & workStat==1
   		cap drop temp
   		qui estimates store reTP_`s'_`e'
	}
}


* -- 6.2. Reentry earnings, including last earnings
* Log earnings of a previously non-employed worker are a function of 
* (a quartic of) age, duration of non-employment (dummy for 1 or 2 year) 
* and last log annual earnings before becoming non-employed.

* Re entry from unemployment
noi display "Re-Entry earnings 2"
g lasty = y
* If employed, what I earn now
replace lasty = lasty if workStat==1 | workStat==2
* If unemployed, what I earned last year
replace lasty = L.lasty if workStat==0
* lagged (if employed, what I earned last year, if unemp, what I earned in t-2)
g Llasty = L.lasty

replace Llasty = 0 if Llasty==. & L.workStat==0
* last y missing
g LlastyM = 0 if L.workStat==0	
replace LlastyM = 1 if Llasty==0 & LlastyM == 0 

forvalues s = 1/2 {
	forvalues e = 1/4 {
   		qui reg y Llasty LlastyM U_dur1 age age2 age3 age4 if sex==`s' & edgroup==`e' & L.workStat==0 & workStat==1
   		cap drop temp
   		qui estimates store rlUP_`s'_`e'
		
		qui reg y Llasty LlastyM U_dur1 age age2 age3 age4 if sex==`s' & edgroup==`e' & L.workStat==0 & workStat==2
   		cap drop temp
   		qui estimates store rlUT_`s'_`e'
		
		qui reg y Llasty LlastyM U_dur1 age age2 age3 age4 if sex==`s' & edgroup==`e' & L.work==0 & work==1
   		cap drop temp
   		qui estimates store rlUE_`s'_`e'
	}
}

forvalues s = 1/2 {
	forvalues e = 1/4 {
   		qui reg y Llasty T_dur1 age age2 age3 age4 if sex==`s' & edgroup==`e' & L.workStat==2 & workStat==1
   		cap drop temp
   		qui estimates store rlTP_`s'_`e'
		
		* I include the estimated earnings for workers downgraded to temporary
		* only as a function of previous earnings	
   		qui reg y Llasty age age2 age3 age4 if sex==`s' & edgroup==`e' & L.workStat==1 & workStat==2
   		cap drop temp
   		qui estimates store rlPT_`s'_`e'
	}
}


* --------------------------------------------------
* 7. Save and export model estimates to be used in Fortran
* --------------------------------------------------
/*
estimates table PU*, se
estimates table PT*, se
estimates table TU*, se
estimates table UP*, se
estimates table UT*, se
estimates table TP*, se
estimates table reU*, se
estimates table reT*, se
estimates table rlU*, se
estimates table rlT*, se
*/
cd $StataOut/All

* upgrade prob
foreach tr in UP UT UE  {
	local t=substr("`tr'",1,1)
	estout `tr'* using "`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4 `t'_dur1 `t'_dur2) order(_cons age age2 age3 age4 `t'_dur1 `t'_dur2)
}

foreach tr in TP  {
	local t=substr("`tr'",1,1)
	estout `tr'* using "`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons Ly Ly2 age age2 age3 age4 `t'_dur1 `t'_dur2 ) order(_cons age age2 age3 age4 `t'_dur1 `t'_dur2 Ly Ly2)
}

* downgrade prob
foreach tr in PU PT TU EU {
	estout `tr'* using "`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4 Ly Ly2) order(_cons age age2 age3 age4 Ly Ly2) 
}

* transition earnings
foreach tr in UP UT UE {
	local stat0=substr("`tr'",1,1) 
	estout re`tr'* using "re`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 `stat0'_dur1) order(_cons age age2 `stat0'_dur1) stats(rmse, label(,none))
	estout rl`tr'* using "rl`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4 U_dur1 Llasty LlastyM) order(_cons age age2 age3 age4 U_dur1 Llasty LlastyM) stats(rmse, label(,none)) 
}
foreach tr in TP {
	local stat0=substr("`tr'",1,1) 
	estout re`tr'* using "re`tr'models.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 `stat0'_dur1) order(_cons age age2 `stat0'_dur1) stats(rmse, label(,none))
}

estout rlTP* using "rlTPmodels.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4 T_dur1 Llasty) order(_cons age age2 age3 age4 T_dur1 Llasty) stats(rmse, label(,none)) 

estout rlPT* using "rlPTmodels.txt", replace ml(,none) varl(,none) coll(,none) eqlabels(,none) keep(_cons age age2 age3 age4 Llasty) order(_cons age age2 age3 age4 Llasty) stats(rmse, label(,none)) 
