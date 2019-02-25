
clear all
cap log close
set more off 
pause off

set matsize 11000, permanently

* Root
global BaseDir 	 	"~/Documents/GitHub/CGMV_prestamos"
global CodesDir   	"$BaseDir/stata"
global StataOut   	"$BaseDir/stata/output"		// Output from Stata 
global GraphDir   	"$BaseDir/paper_figures"	// graphs

* just in case it's first run
cap mkdir $GraphDir
cap mkdir $StataOut

* -------------------------------------

cd $BaseDir

*run $CodesDir/1_read_fortran_simul
*run $CodesDir/2_loans_lab
