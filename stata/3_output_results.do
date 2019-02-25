* This code plots 
* 1. Number of years to repay all debt
* 2. Net present value of repayments
* 3. Subsidy 
* for each of the experiments calculated in 2_loan_lab.do. 
*
* Rocio Madera
* Last: 2/22/2019
*---------------------------------------------------------------------------
pause off
cap set scheme plotplainblind, permanently

run $CodesDir/0_setdirs

// choose which results to export
global maketable=0 	// NOTE: to generate whole tables, make sure listcases includes "0 2 1 3 4 5"
global makegraphs=0
global makequality=1

* Select education
local e=4
* choose sex
local s=3 

global Q=0
* Setup file to export table
if ${maketable}==1 {
	shell rm $GraphDir/Table4.xlsx
	putexcel set $GraphDir/Table4.xlsx, replace
	putexcel set $GraphDir/Table4.xlsx, modify sheet(Q=${Q},replace)
}
		
* choose case
// 0- baseline
// 1- princial (fees)
// 2- interest rate on debt
// 3- exempt amount
// 4- debt write-off year
// 5- repayment rate
local listcases="0 2 1 3 4 5"
// local listcases="0 2 3"
foreach i of local listcases {


* GRAPHS
use "$StataOut/Case_s`s'_e`e'.dta", clear
g currentsub=.8
keep if iRate_Discount==22
g repaid = 1-subsidy
drop subsidy
g subsidy=1-NPV/(Principal*1000)


local unit="K"
if `i'==0 {
	local casename="baseline"
	preserve
	keep if Prin==21
	keep if Ex==15
	keep if Max==25
	keep if Repay==10
	keep if iRate_Debt==0
	
	if ${makegraphs}==1 {
	twoway (line repaymyear pcgroup)
	*graph save  "${GraphDir}/`casename'_REPAY.gph", replace
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace
	
	
	local color1="black"
	local color2="plb1"
	local color3="cranberry"
	local color4="gs11"
	local color5="gs13"
	
	
	twoway 	(line NPV pcgroup, lcolor(`color1') lpattern(-) lwidth(.4)) ///
			(line NPVICL pcgroup, lcolor(`color1') lpattern(solid) lwidth(.4)) ///
			(line NPVGTF pcgroup, lcolor(`color4') lpattern(solid) lwidth(.4)) ///			
			(line FGTF pcgroup, lcolor(`color4') lpattern(-) lwidth(.4)) ,  ///
			legend(pos(11) ring(0) col(1) size(normal) ///
			lab(1 "Repayments {&sum}{subscript:a}{&beta}{superscript:a}P{subscript:i,a} (ICL)") ///
			lab(2 "ICL Total Repayments + Tax") ///
			lab(3 "GTF Total Fees + Tax") ///
			lab(4 "Fees F(GTF)") ///
			order(2 1 3 4))  ytitle("Euros") xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	graph export  "${GraphDir}/`casename'_NPV_GTF.pdf", replace
pause

	twoway (line NPV pcgroup)
	*graph save  "${GraphDir}/`casename'_NPV.gph", replace
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	}
	
	sum subsidy 
	global meansub=r(mean)
	g meansub=$meansub
	
	* store for quality analysis
	global meansub_noQ=$meansub
	sum subsidy if pcgroup<=10
	global p10sub_noQ=r(mean)
	sum subsidy if pcgroup>=90
	global p90sub_noQ=r(mean)
	
	sum repaid
	global meanrep=r(mean)	
	sum repaid if pcgroup<=10
	global p10rep=r(mean)
	sum repaid if pcgroup>=90
	global p90rep=r(mean)
	
	sum NPVICL if pcgroup<=10
	global npviclp10 = r(mean)
	sum NPVICL if pcgroup>=90
	global npviclp90 = r(mean)
	global withinICL=$npviclp90/$npviclp10

	sum NPVGTF if pcgroup<=10
	global npvgtfp10 = r(mean)
	sum NPVGTF if pcgroup>=90
	global npvgtfp90 = r(mean)
	global withinGTF=${npvgtfp90}/${npvgtfp10}
	
	if ${maketable}==1 {
		local ic=2
		putexcel A`ic'="Baseline"
		local ic=`ic'+1	
		putexcel A`ic'=`i' D`ic'=100*$meansub E`ic'=100*$meanrep F`ic'=100*$p90rep G`ic'=100*$p10rep H`ic'=$withinGTF I`ic'=$withinICL 
	}
	
	if ${makegraphs}==1 {
	twoway (line subsidy pcgroup) (line meansub pcgroup) (line currentsub pcgroup), ///
			legend(pos(7) ring(0) col(1) size(normal) ///
			lab(1 "Subsidy by lifetime income - {it:Sub}{subscript:i}") ///
			lab(2 "ICL Subsidy - {it:Sub(ICL)}") ///
			lab(3 "GTF Subsidy - {it:Sub(GTF)}")) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	*graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	}
	restore
pause "DONE 0"
}
else if `i'==2 {
pause
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
	
	if ${makegraphs}==1 {
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
	*graph save  "${GraphDir}/`casename'_REPAY.gph", replace
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
	*graph save  "${GraphDir}/`casename'_NPV.gph", replace
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	}
	
	pause
	forval j=1/4 {
		sum subsidy if `varname'==`l`j''
		global meansub`j'=r(mean)
		sum subsidy if pcgroup<=10 & `varname'==`l`j''
		global p10sub`j'=r(mean)
		sum subsidy if pcgroup>=90 & `varname'==`l`j''
		global p90sub`j'=r(mean)
		
		sum repaid if `varname'==`l`j''
		global meanrep`j'=r(mean)	
		sum repaid if pcgroup<=10 & `varname'==`l`j''
		global p10rep`j'=r(mean)
		sum repaid if pcgroup>=90 & `varname'==`l`j''
		global p90rep`j'=r(mean)
		
		sum NPVICL if pcgroup<=10 & `varname'==`l`j''
		global npviclp10`j' = r(mean)
		sum NPVICL if pcgroup>=90 & `varname'==`l`j''
		global npviclp90`j' = r(mean)
		global withinICL`j'=${npviclp90`j'}/${npviclp10`j'}
	
		sum NPVGTF if pcgroup<=10 & `varname'==`l`j''
		global npvgtfp10`j' = r(mean)
		sum NPVGTF if pcgroup>=90 & `varname'==`l`j''
		global npvgtfp90`j' = r(mean)
		global withinGTF`j'=${npvgtfp90`j'}/${npvgtfp10`j'}
		

	}
	
	if ${maketable}==1 {
		local ic=`ic'+2
		putexcel A`ic'="Changing `varlabname' (first row is baseline)"
		forval j=2/4 {
			local ic=`ic'+1
			putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l`j''/10 ///
					 D`ic'=100*${meansub`j'} E`ic'=100*${meanrep`j'} ///
					 F`ic'=100*${p90rep`j'}  G`ic'=100*${p10rep`j'} H`ic'=${withinGTF`j'} ///
					 I`ic'=${withinICL`j'}
		}
	}
	
	sum subsidy if `varname'==`l1'
	global meansub1=r(mean)
	g meansub1=$meansub1
	sum subsidy if `varname'==`l2'
	global meansub2=r(mean)
	g meansub2=$meansub2
	sum subsidy if `varname'==`l3'
	global meansub3=r(mean)
	g meansub3=$meansub3
	sum subsidy if `varname'==`l4'
	global meansub4=r(mean)
	g meansub4=$meansub4
	
	if ${makegraphs}==1 {
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
			lab(1 "GTF Subsidy") ///
			order(1 6 8 4)) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	*graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	}
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
		
	if ${makegraphs}==1 {
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
	*graph save  "${GraphDir}/`casename'_REPAY.gph", replace
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
	*graph save  "${GraphDir}/`casename'_NPV.gph", replace
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
			lab(1 "GTF Subsidy") ///
			order(1 6 8 4)) ///
			ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
			xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
	*graph save  "${GraphDir}/`casename'_subsidy.gph", replace
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	}
	forval j=1/4 {
		sum subsidy if `varname'==`l`j''
		global meansub`j'=r(mean)
		sum subsidy if pcgroup<=10 & `varname'==`l`j''
		global p10sub`j'=r(mean)
		sum subsidy if pcgroup>=90 & `varname'==`l`j''
		global p90sub`j'=r(mean)
		
		sum repaid if `varname'==`l`j''
		global meanrep`j'=r(mean)	
		sum repaid if pcgroup<=10 & `varname'==`l`j''
		global p10rep`j'=r(mean)
		sum repaid if pcgroup>=90 & `varname'==`l`j''
		global p90rep`j'=r(mean)
		
		sum NPVICL if pcgroup<=10 & `varname'==`l`j''
		global npviclp10`j' = r(mean)
		sum NPVICL if pcgroup>=90 & `varname'==`l`j''
		global npviclp90`j' = r(mean)
		global withinICL`j'=${npviclp90`j'}/${npviclp10`j'}
	
		sum NPVGTF if pcgroup<=10 & `varname'==`l`j''
		global npvgtfp10`j' = r(mean)
		sum NPVGTF if pcgroup>=90 & `varname'==`l`j''
		global npvgtfp90`j' = r(mean)
		global withinGTF`j'=${npvgtfp90`j'}/${npvgtfp10`j'}
		

	}
	if ${maketable}==1 {
	local ic=`ic'+2
	putexcel A`ic'="Changing `varlabname' (first row is baseline)"
		forval j=2/4 {
			local ic=`ic'+1
			putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l`j''*1000 ///
					 D`ic'=100*${meansub`j'} E`ic'=100*${meanrep`j'} ///
					 F`ic'=100*${p90rep`j'}  G`ic'=100*${p10rep`j'} H`ic'=${withinGTF`j'} ///
					 I`ic'=${withinICL`j'}
			if `i'==5 putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l`j'' ///
					 D`ic'=100*${meansub`j'} E`ic'=100*${meanrep`j'} ///
					 F`ic'=100*${p90rep`j'}  G`ic'=100*${p10rep`j'} H`ic'=${withinGTF`j'} ///
					 I`ic'=${withinICL`j'}
		}
		
		
	}

	restore
}

if ${maketable}==1 {
	putexcel A1="CASE" B1="PARAMETER" D1="AVG. SUBSIDY (DISCOUNTED)" ///
			 E1="FRACTION REPAID (NOT DISCOUNTED)"  ///
			 F1="FRACTION REPAID TOP 10%" G1="FRACTION REPAID BOTTOM 10%" ///
			 H1="WITHIN PROG - GTF" I1="WITHIN PROG - ICL"		
}

}


if $makequality==1{

global Q=1
shell rm $GraphDir/Table3.xlsx
putexcel set $GraphDir/Table3.xlsx, modify sheet(Q=${Q},replace)


local listcases="0"

// local listcases="0 "
foreach i of local listcases {


* GRAPHS
use "$StataOut/Case_s`s'_e`e'_q.dta", clear

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
	
	
	sum subsidy 
	global meansub=r(mean)
	sum subsidy if pcgroup<=10
	global p10sub=r(mean)
	sum subsidy if pcgroup>=90
	global p90sub=r(mean)
	foreach mom in "mean" "p10" "p90" {
	g `mom'sub=${`mom'sub}
	g `mom'sub_noQ=${`mom'sub_noQ}
	}

	local ic=2
	putexcel A`ic'="Baseline"
	local ic=`ic'+1	
	putexcel A`ic'="Q"
	local ic=`ic'+1	
		putexcel A`ic'=0 D`ic'=100*$meansub_noQ E`ic'=100*$p10sub_noQ F`ic'=100*$p90sub_noQ
		local ic=`ic'+1	
		putexcel A`ic'=1 D`ic'=100*$meansub E`ic'=100*$p10sub F`ic'=100*$p90sub
	
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
	
	
	forval j=1/4 {
		sum subsidy if `varname'==`l`j''
		global meansub`j'=r(mean)
		sum subsidy if pcgroup<=10 & `varname'==`l`j''
		global p10sub`j'=r(mean)
		sum subsidy if pcgroup>=90 & `varname'==`l`j''
		global p90sub`j'=r(mean)
		foreach mom in "mean" "p10" "p90" {
		g `mom'sub`j'=${`mom'sub`j'}
		}
	}
		local ic=`ic'+2
		putexcel A`ic'="Changing `varlabname' (first row is baseline)"
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l1' D`ic'=100*$meansub1 E`ic'=100*$p10sub1 F`ic'=100*$p90sub1
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l2'/10 D`ic'=100*$meansub2 E`ic'=100*$p10sub2 F`ic'=100*$p90sub2
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l3'/10 D`ic'=100*$meansub3 E`ic'=100*$p10sub3 F`ic'=100*$p90sub3
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l4'/10 D`ic'=100*$meansub4 E`ic'=100*$p10sub4 F`ic'=100*$p90sub4
	
	
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

	
	forval j=1/4 {
		sum subsidy if `varname'==`l`j''
		global meansub`j'=r(mean)
		sum subsidy if pcgroup<=10 & `varname'==`l`j''
		global p10sub`j'=r(mean)
		sum subsidy if pcgroup>=90 & `varname'==`l`j''
		global p90sub`j'=r(mean)
		foreach mom in "mean" "p10" "p90" {
		g `mom'sub`j'=${`mom'sub`j'}
		}
	}
	local ic=`ic'+2
	putexcel A`ic'="Changing `varlabname' (first row is baseline)"
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l1'*1000 D`ic'=100*$meansub1 E`ic'=100*$p10sub1 F`ic'=100*$p90sub1
		if `i'==5 putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l1' D`ic'=100*$meansub1 E`ic'=100*$p10sub1 F`ic'=100*$p90sub1
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l2'*1000 D`ic'=100*$meansub2 E`ic'=100*$p10sub2 F`ic'=100*$p90sub2
		if `i'==5 putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l2' D`ic'=100*$meansub2 E`ic'=100*$p10sub2 F`ic'=100*$p90sub2
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l3'*1000 D`ic'=100*$meansub3 E`ic'=100*$p10sub3 F`ic'=100*$p90sub3
		if `i'==5 putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l3' D`ic'=100*$meansub3 E`ic'=100*$p10sub3 F`ic'=100*$p90sub3
		local ic=`ic'+1
		putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l4'*1000 D`ic'=100*$meansub4 E`ic'=100*$p10sub4 F`ic'=100*$p90sub4
		if `i'==5 putexcel A`ic'=`i' B`ic'="`varlabname'" C`ic'=`l4' D`ic'=100*$meansub4 E`ic'=100*$p10sub4 F`ic'=100*$p90sub4

	
	restore
}

}

putexcel A1="CASE" B1="PARAMETER" D1="AVG. SUBSIDY" E1="TO LOWER 10%" F1="TO TOP 10%"
}
