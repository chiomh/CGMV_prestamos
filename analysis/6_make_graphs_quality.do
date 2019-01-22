* This code plots 
* 1. Number of years to repay all debt
* 2. Net present value of repayments
* 3. Subsidy 
* for each of the experiments calculated in rocio_dic17_2.do.
*
* Rocio Madera
* Last: 12/20/2017
*---------------------------------------------------------------------------
pause off

cap set scheme plotplainblind, permanently
local e=4

* choose sex
local s=3 // all

* choose case
// 0- baseline
// 1- principal (fees)
// 2- interest rate on debt
// 3- exempt amount
// 4- debt write-off year
// 5- repayment rate
local i=4

global StataOut "$StataOut/../output_q"
global GraphDir "$GraphDir/../graficos_q"

cd $StataOut
* GRAPHS
use "$StataOut/Case_s`s'_e`e'.dta", clear
keep if iRate_Discount==22
local unit="K"
if `i'==0 {
	local casename="baseline"
	preserve
	keep if Prin==21
	keep if Ex==15
	keep if Max==25
	keep if Repay==10
	keep if iRate_Debt==0
	
	twoway (line repaymyear pcgroup)
	pause
	graph save  "${GraphDir}/`casename'_REPAY.gph", replace
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace
	
	
	twoway (line NPV pcgroup)
	graph save  "${GraphDir}/`casename'_NPV.gph", replace
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	
	sum subsidy 
	global meansub=r(mean)
	g meansub=$meansub
	g currentsub=.8
	twoway (line subsidy pcgroup) (line meansub pcgroup) (line currentsub pcgroup), ///
			legend(pos(7) ring(0) col(1) size(normal) ///
			lab(1 "Subsidy by lifetime income") ///
			lab(2 "Average subsidy") ///
			lab(3 "Current subsidy")) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	restore
}
else if `i'==2 {

	local unit="%"
	global maxd=25000
	*case3: exempt;
	local casename="CASE`i'"
	local varname="iRate_Debt"
	local varlabname="Interest R."
	preserve
	keep if Ex==15
	keep if Max==25
	keep if Repay==10
	keep if Prin==21
	*keep if iRate_Debt==0
	
	local l1=0
	local l2=5
	local l3=22
	local l4=8
	local l5=15
	
	local color1="black"
	local color2="plb1"
	local color3="cranberry"
	local color4="gs13"
	local color5="gs13"

	
	twoway ///
	(line repaymyear pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid) ) ///
	(line repaymyear pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid) ) ///
	(line repaymyear pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line repaymyear pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line repaymyear pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)), ///
	legend(pos(7) ring(0) col(3) size(normal) ///
			lab(3 "`varlabname'=`l1'`unit' (Base)") ///
			lab(5 "`varlabname'=0.5`unit'") ///
			lab(7 "`varlabname'=2.2`unit'") ///
			order(3 5 7))  xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph save  "${GraphDir}/`casename'_REPAY.gph", replace
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace
	pause
	
	twoway  ///
	(line NPV pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid) ) ///
	(line NPV pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid) ) ///
	(line NPV pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line NPV pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line NPV pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)), ///
	legend(pos(11) ring(0) col(3) size(normal) ///
			lab(3 "`varlabname'=`l1'`unit' (Base)") ///
			lab(4 "`varlabname'=0.5`unit'") ///
			lab(5 "`varlabname'=2.2`unit'") ///
			order(3 4 5)) xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') ///
			ylabel(0(10000)$maxd)
	graph save  "${GraphDir}/`casename'_NPV.gph", replace
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	
	sum subsidy if `varname'==`l1'
	global meansub1=r(mean)
	g meansub1=$meansub
	sum subsidy if `varname'==`l2'
	global meansub2=r(mean)
	g meansub2=$meansub2
	sum subsidy if `varname'==`l3'
	global meansub3=r(mean)
	g meansub3=$meansub3
	sum subsidy if `varname'==`l4'
	global meansub4=r(mean)
	g meansub4=$meansub4
	
	g currentsub=.8
	twoway (connected currentsub pcgroup, mcolor(gs10) msymbol(X) connect(none)) ///
	(line subsidy pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid)) ///
	(line subsidy pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid)) ///
	(line subsidy pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line meansub1 pcgroup if `varname'==`l1', lcolor(`color1') lpattern(-)) ///
	(line subsidy pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line meansub2 pcgroup if `varname'==`l2', lcolor(`color2') lpattern(-)) ///
	(line subsidy pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)) ///
	(line meansub3 pcgroup if `varname'==`l3', lcolor(`color3') lpattern(-)), ///
			legend(pos(2) ring(0) col(2) size(normal) ///
			lab(4 "`varlabname'=`l1'`unit' (Base)") ///
			lab(6 "`varlabname'=0.5`unit'") ///
			lab(8 "`varlabname'=2.2`unit'") ///
			lab(1 "Current Subsidy") ///
			order(1 6 8 4)) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	restore
}
else {
	if `i'==1 {
		global maxd=40000
		*case1: different debt levels (PRINCIPAL);
		local casename="CASE`i'"
		local varname="Principal"
		local varlabname="Debt"
		preserve
		*keep if `varname'==21
		keep if Ex==15
		keep if Max==25
		keep if Repay==10
		keep if iRate_Debt==0
		
		local l1=21
		local l2=5
		local l3=40
		local l4=10
		local l5=30
	}
	else if `i'==3 {
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="Exempt"
		local varlabname="Exempt"
		preserve
		*keep if Ex==15
		keep if Max==25
		keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=15
		local l2=10
		local l3=25
		local l4=20
		local l5=20
	}
	else if `i'==4 {
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="MaxYears"
		local varlabname="Debt Write-Off"
		local unit="Yrs"
		preserve
		keep if Ex==15
		*keep if Max==25
		keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=25
		local l2=15
		local l3=30
		local l4=20
		local l5=20
	}
	else if `i'==5 {
		local unit="%"
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="RepaymRate"
		local varlabname="Repay.Rate"
		preserve
		keep if Ex==15
		keep if Max==25
		*keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=10
		local l2=5
		local l3=15
		local l4=8
		local l5=8
	}	
		
	
	local color1="black"
	local color2="plb1"
	local color3="cranberry"
	local color4="gs13"
	local color5="gs13"

	
	twoway ///
	(line repaymyear pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid) ) ///
	(line repaymyear pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid) ) ///
	(line repaymyear pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line repaymyear pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line repaymyear pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)), ///
	legend(pos(7) ring(0) col(3) size(normal) ///
			lab(3 "`varlabname'=`l1'`unit' (Base)") ///
			lab(5 "`varlabname'=`l2'`unit'") ///
			lab(7 "`varlabname'=`l3'`unit'") ///
			order(3 5 7))  xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph save  "${GraphDir}/`casename'_REPAY.gph", replace
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace
	pause
	
	twoway  ///
	(line NPV pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid) ) ///
	(line NPV pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid) ) ///
	(line NPV pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line NPV pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line NPV pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)), ///
	legend(pos(11) ring(0) col(3) size(normal) ///
			lab(3 "`varlabname'=`l1'`unit' (Base)") ///
			lab(4 "`varlabname'=`l2'`unit'") ///
			lab(5 "`varlabname'=`l3'`unit'") ///
			order(3 4 5)) xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') ///
			ylabel(0(10000)$maxd)
	graph save  "${GraphDir}/`casename'_NPV.gph", replace
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	
	sum subsidy if `varname'==`l1'
	global meansub1=r(mean)
	g meansub1=$meansub
	sum subsidy if `varname'==`l2'
	global meansub2=r(mean)
	g meansub2=$meansub2
	sum subsidy if `varname'==`l3'
	global meansub3=r(mean)
	g meansub3=$meansub3
	sum subsidy if `varname'==`l4'
	global meansub4=r(mean)
	g meansub4=$meansub4
	
	g currentsub=.8
	twoway (connected currentsub pcgroup, mcolor(gs10) msymbol(X) connect(none)) ///
	(line subsidy pcgroup if `varname'==`l4', lcolor(`color4') lpattern(solid)) ///
	(line subsidy pcgroup if `varname'==`l5', lcolor(`color5') lpattern(solid)) ///
	(line subsidy pcgroup if `varname'==`l1', lcolor(`color1') lpattern(solid) lwidth(.5)) ///
	(line meansub1 pcgroup if `varname'==`l1', lcolor(`color1') lpattern(-)) ///
	(line subsidy pcgroup if `varname'==`l2', lcolor(`color2') lpattern(solid) lwidth(.5)) ///
	(line meansub2 pcgroup if `varname'==`l2', lcolor(`color2') lpattern(-)) ///
	(line subsidy pcgroup if `varname'==`l3', lcolor(`color3') lpattern(solid) lwidth(.5)) ///
	(line meansub3 pcgroup if `varname'==`l3', lcolor(`color3') lpattern(-)), ///
			legend(pos(2) ring(0) col(2) size(normal) ///
			lab(4 "`varlabname'=`l1'`unit' (Base)") ///
			lab(6 "`varlabname'=`l2'`unit'") ///
			lab(8 "`varlabname'=`l3'`unit'") ///
			lab(1 "Current Subsidy") ///
			order(1 6 8 4)) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	restore
}

