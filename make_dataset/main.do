* Main code for the empirical analysis of paper ???
*
* main.do defines globals and calls subcodes in the appropriate order.
*
* RUNNING INSTRUCTIONS:
*	- main.do defines paths and runs the do-files in the right order. 
*
* NOTE ON VERSION COMPATIBILITY AND PACKAGES: 
*	- I use putexcel, which is only available in 13+ versions. 
*	  If your stata version is 12 or older, make sure you select global
*	  CountObs=="N", or erase all statements that are bracketed by a
*	  "if $CountObs=="Y""
*	- Packages used: graph scheme plotplainblind, estout, latab, frmttable.
*	  You can get those writing "findit [name of the package]" in the command
*	  window.
*
* Rocio Madera
* Last revision: 12/1/2018
*---------------------------------------------------------------------------

clear all
cap log close
set more off 
pause off
*set scheme plotplainblind

*---------------------------------------------------------------------------
* Directories 
*---------------------------------------------------------------------------

global BaseDir 	 "~/Dropbox/research/cgm_education/Dual_Dynamics"	// My Mac

global CodesDir   	"$BaseDir/stata/codes"
global StataOut   	"$BaseDir/stata/output"
global FortranOut   "$BaseDir/fortran/simulations"
global DataDir   	"$BaseDir/stata/data"	
*global DataFile   	"$DataDir/MCVLfinal2005-2013_ipc.dta"
global DataFile   	"$DataDir/mcvl0514.dta"

cd $StataOut

* create directories in output if first time running
cap mkdir All
cap mkdir Perm
cap mkdir Tem

*---------------------------------------------------------------------------
* Running options 
*---------------------------------------------------------------------------

global SampleStable=0		// All (0),  Stable (1), Unstable (2)
global ComputeSumStats "N" 	// Calculate summary statistics for main variables?
global CountObs "N" 		// Keep count of the observations at each step

*---------------------------------------------------------------------------
* Parameters for sample selection
*---------------------------------------------------------------------------
*Lower bound on earnings, DGKW use 1000 pounds, I use 1000 euros
*global minearns=641.4 // NONE -- use minimum wage defined in FirstStage instead
global minearns "Minimum wage"

* age
global minage=19		// Min age (non-college grads)
global mingradage=22 	// Minimum age for graduates
global maxage=60		// Max age

*---------------------------------------------------------------------------
* Estimation options
*---------------------------------------------------------------------------
global nlags=6 			// Number of lags for Autocovariances

*******************************************************
*1. Prepare the data
*******************************************************
if "$CountObs"=="Y" {
	* Initialize count of observations 
	putexcel A1=("This file includes a count of observations at each step of the data cleaning and sample selection") using $StataOut/countN, replace sheet("readme")
	putexcel A2=("Data: MCVL, 2005-2014") using $StataOut/countN, modify sheet("readme")
	putexcel A6=("Rocio Madera") using $StataOut/countN, modify sheet("readme")
	putexcel A7=("`c(current_date)'") using $StataOut/countN, modify sheet("readme")
	putexcel A9=("MAIN PARAMETERS FOR SELECTION") using $StataOut/countN, modify sheet("readme")
	putexcel A10=("Min earnings") B10=("$minearns") using $StataOut/countN, modify sheet("readme")
	putexcel A11=("Min age") B11=($minage) using $StataOut/countN, modify sheet("readme")
	putexcel A12=("Max age") B12=($maxage) using $StataOut/countN, modify sheet("readme")
	putexcel A13=("Min grad age") B13=($mingradage) using $StataOut/countN, modify sheet("readme")
	putexcel A14=("Lags") B14=($nlags) using $StataOut/countN, modify sheet("readme")
}

* Make a clean panel
qui do $CodesDir/PrepareData.do

*************************************************************
*2. Extract income residuals 
*3. Estimate transition probabilities and transition earnings
*4. Compute Covariances of income residuals for estimation
*5. Calculate quantile statistics - for model fit purposes
*6. Repeat steps 4 and 5 for lifetime-perm and lifetime-temp
*************************************************************

global SampleStable=0
qui do $CodesDir/2.FirstStage.do
qui do $CodesDir/3.EmpModels.do
qui do $CodesDir/4.GetCovariance.do
qui do $CodesDir/5.Quantiles.do

global SampleStable=1
qui do $CodesDir/2.FirstStage.do
qui do $CodesDir/4.GetCovariance.do
qui do $CodesDir/5.Quantiles.do

global SampleStable=2
qui do $CodesDir/2.FirstStage.do
qui do $CodesDir/4.GetCovariance.do
qui do $CodesDir/5.Quantiles.do


*************************************************************
* Finally...
* Set up directories for fortran simulations and estimation
*************************************************************
cd $FortranOut
cap mkdir All
cap mkdir Per
cap mkdir Tem

foreach folder in All Per Tem {
	cd $FortranOut/`folder'
	cap mkdir sex_1
	cap mkdir sex_2
	
	forval s=1/2 {
		cd sex_`s'
		forval a=1/4 {
			cap mkdir edgroup_`a'
		}
		cd ..
	}
}
