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
global makegraphs=1
global makequality=0

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
// local listcases="0 2 1 3 4 5"
local listcases="2 1 3 4 5"
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
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace	
	
	if "$figcolor"=="bw" {
		local typgraph="line"
		local color1="black"
		local color2="plb1"
		local color3="cranberry"
		local color4="gs10"
		
		local pattern1="lcolor(`color1') lpattern(-) msymbol(none) "
		local pattern2="lcolor(`color1') lpattern(solid) msymbol(none) "
		local pattern3="lcolor(`color4') lpattern(_.)  lwidth(.4)"
		local pattern4="lcolor(`color4') lpattern(.)  lwidth(1)"
	} 
	else if "$figcolor"=="bw2" {
		local typgraph="connected"
		local color1="black"
		local color2="plb1"
		local color3="cranberry"
		local color4="gs8"
		local color5="gs13"
		
		local pattern1="lcolor(`color1') lpattern(-) msymbol(none) "
		local pattern2="lcolor(`color1') lpattern(solid) msymbol(none) "
		local pattern3="lcolor(`color4') lpattern(solid) msymbol(Oh) mcolor(`color4') mlwidth(.2) lwidth(.2)"
		local pattern4="lcolor(`color4') lpattern(solid) msymbol(X) mcolor(`color4') mlwidth(.2) msize(1.1) lwidth(.2)"
	}
	else {
		local typgraph="line"
		local color1="black"
		local color2="plb1"
		local color3="cranberry"
		local color4="gs11"
		local color5="gs13"
	}
	
	qui sum NPVICL if pcgroup==100
	local g1max1 = r(max)
	qui sum NPV if pcgroup==100
	local g1max2 = r(max)
	qui sum NPVGTF if pcgroup==100
	local g1max3 = r(max)
	qui sum FGTF if pcgroup==100
	local g1max4 = r(max)
	twoway 	(`typgraph' NPVGTF pcgroup, `pattern3' ) ///	
			(`typgraph' FGTF pcgroup, `pattern4' ) ///
			(`typgraph' NPV pcgroup, `pattern1' lwidth(.7)) ///
			(`typgraph' NPVICL pcgroup, `pattern2' lwidth(.7)) ///	
			,  ///
			legend(off) ///
			text(`g1max1' 101 "ICL:" "Total Repaym. + Tax", place(e) color(`color1') size(medsmall) just(left)) ///
			text(`g1max2' 101 "ICL:" "Repayments {&sum}{subscript:a}{&beta}{superscript:a}P{subscript:i,a}", place(e) color(`color1') size(medsmall) just(left)) ///
			text(`g1max3' 101 "GTF:" "Total Fees + Tax", place(e) color(`color4') size(medsmall) just(left) ) ///
			text(`g1max4' 101 "GTF:" "Fees", place(e) color(`color4') size(medsmall) just(left)) ///			
			xscale(r(0 130)) ytitle("Euros") ylab(, nogrid) ///
			xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			xtitle(,margin(r+30))
	graph export  "${GraphDir}/`casename'_NPV_GTF.pdf", replace
	
	twoway (line NPV pcgroup)
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
	local typgraph="line"
	local color1="black"
	local color2="plb1"
	local color3="cranberry"
	local color4="gs10"
	
	local pattern1="lcolor(`color1') lpattern(solid) msymbol(none) lwidth(.7)"
	local pattern2="lcolor(`color4') lpattern(__.) msymbol(none) lwidth(.4)"
	local pattern3="lcolor(`color4') lpattern(.)  lwidth(1)"
	
	local text1=`" "Subsidy by Lifetime Income"  "{it:Sub}{subscript:i}" "'
	local text2=`" "ICL Average Subsidy" "{it:Sub(ICL)}" "'
	local text3=`" "GTF Average Subsidy" "{it:Sub(GTF)}" "'
	
	qui sum subsidy if pcgroup==100
	local ymax1 = r(max)
	qui sum meansub if pcgroup==100
	local ymax2 = r(max)
	qui sum currentsub if pcgroup==100
	local ymax3 = r(max)
	twoway 	(`typgraph' meansub pcgroup, `pattern2' ) ///
			(`typgraph' currentsub pcgroup, `pattern3') ///
			(`typgraph' subsidy pcgroup, `pattern1' ) ///	
			,  ///
			legend(off) ///
			text(`ymax1' 101 `text1', place(e) color(`color1') size(medsmall) just(left)) ///
			text(`ymax2' 101 `text2', place(e) color(`color4') size(medsmall) just(left)) ///
			text(`ymax3' 101 `text3', place(e) color(`color4') size(medsmall) just(left) ) ///
			yscale(r(0 1)) xscale(r(0 143)) ytitle("Subsidy As a Share of Loan") ylab(0(.2)1 , nogrid) ///
			xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			xtitle(,margin(r+43))
			
	graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	}
	restore
pause "DONE 0"
}
else if `i'==2 {
	local unit="%"
	global maxd=25000
	*case3: exempt;
	local casename="CASE`i'"
	local varname="iRate_Debt"
	local varlabname="Interest Rate"
	preserve
	keep if Ex==15
	keep if Max==25
	keep if Repay==10
	keep if Prin==21
	*keep if iRate_Debt==0
	
// 	local l1=0
// 	local l2=5
// 	local l3=22
// 	local l4=8
// 	local l5=15
	
	local l1=0
	local l2=5
	local l3=8
	local l4=15
	local l5=22
	
	if ${makegraphs}==1 {
	local typgraph="connected"
	local color1="black"
	local color3="gs10"
	local color5="gs7"
	local color4="gs9"
	local color2="gs9"
	
	local pattern1="lcolor(`color1') lpattern(solid) lwidth(.7) mcolor(`color1') msymbol(none) mlwidth(.2) msize(1.5)"
	local pattern2="lcolor(`color2') lpattern(solid) lwidth(.2) mcolor(`color2') msymbol(none) mlwidth(.2) msize(1.1)"
	local pattern3="lcolor(`color3') lpattern(solid) lwidth(.5) mcolor(`color3') msymbol(o) mlwidth(.2) msize(1.3)"
	local pattern4="lcolor(`color4') lpattern(solid) lwidth(.2) mcolor(`color4') msymbol(none) mlwidth(.2) msize(1.1)"
	local pattern5="lcolor(`color5') lpattern(solid) lwidth(.5) mcolor(`color5') msymbol(t) mlwidth(.2) msize(1.5)"
	
	local graph1 = ""
	local i=0
	forval a=5(-1)1{ 
		local ++i
		local graph1= "`graph1' (`typgraph' repaymyear pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	twoway `graph1', ///
	legend(pos(7) ring(0) col(3) size(normal) ///
			lab(5 "`varlabname' = 0.0`unit' (Base)") ///
			lab(3 "0.5`unit'") ///
			lab(1 "2.2`unit'") ///
			order(5 3 1))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			xsize(6) ysize(4)  ylab(, nogrid)
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace

	local graph1 = ""
	local i=0
	forval a=5(-1)1{ 
		local ++i
		local graph1= "`graph1' (`typgraph' NPV pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	twoway `graph1', ///
	legend(pos(11) ring(0) col(3) size(normal) ///
			lab(5 "`varlabname' = 0.0`unit' (Base)") ///
			lab(3 "0.5`unit'") ///
			lab(1 "2.2`unit'") ///
			order(5 3 1))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			ylabel(0(10000)$maxd, nogrid) ///
			xsize(6) ysize(4) 
			
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	}
	
	forval j=1/5 {
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
	sum subsidy if `varname'==`l5'
	global meansub5=r(mean)
	g meansub5=$meansub5
	
	if ${makegraphs}==1 {	
	
	local graph1 = ""
	local i=0
	forval a=5(-1)1{ 
		local ++i
		local graph1= "`graph1' (`typgraph' subsidy pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	qui sum meansub1 if pcgroup==100
	local extra = r(max)*0.03
	local ymax1 = r(max)+`extra'
	qui sum meansub5 if pcgroup==100
	local ymax2 = r(max)+`extra'
	qui sum currentsub if pcgroup==100
	local ymax3 = r(max)+`extra'
	
			
	twoway (connected currentsub pcgroup, `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none)) ///
		   (connected meansub1 pcgroup if `varname'==`l1', `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none) mlwidth(.2) msize(1.2)) ///
		   (connected meansub5 pcgroup if `varname'==`l5', `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none)   mlwidth(.2) msize(1.2)) ///
			`graph1' ///	
		   (scatter meansub1 pcgroup if `varname'==`l1' & pcgroup==100, `pattern1' msymbol(|) mlwidth(1.5)) ///
		   (scatter meansub5 pcgroup if `varname'==`l5' & pcgroup==100, `pattern5' msymbol(t) mlwidth(.4)) ///
			, ///
			text(1.02 104 "{it:Average}" "{it:Subsidy}", place(s) color("black") size(medium) bcolor(gs13%50) box margin(b=2 t=1 l=2 r=2.1) al(top) just(center)) ///
			text(`ymax1' 101 "{it:0.0`unit' (Base)}", place(e) color("black") size(small) just(left)) ///
			text(`ymax2' 101 "{it:2.2`unit'}", place(e) color("black") size(small) just(left)) ///
			text(`ymax3' 101 "{it:GTF}", place(e) color("black") size(small) just(left) al(bottom)) ///
			legend(pos(12) ring(0) col(3) size(normal) ///
			lab(8 "`varlabname' = 0.0`unit' (Base)") ///
			lab(6 "0.5`unit'") ///
			lab(4 "2.2`unit'") ///	
			order(8 6 4))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			ytitle("Subsidy As a Share of Loan") ///
			ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "', nogrid)  ///
			xsize(7) ysize(4)   ///
			plotregion(margin(r+0)) graphregion(margin(r+15)) xtitle(,margin(r+0))
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
		
		local l1=5
		local l2=10
		local l3=21
		local l4=30
		local l5=40
	}
	else if `i'==3 {
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="Exempt"
		local varlabname="Exempt Amount"
		preserve
		*keep if Ex==15
		keep if Max==25
		keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=10
		local l2=15
		local l3=15
		local l4=20
		local l5=25
	}
	else if `i'==4 {
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="MaxYears"
		local varlabname="Debt Write-Off"
		local unit=" Years"
		preserve
		keep if Ex==15
		*keep if Max==25
		keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=15
		local l2=20
		local l3=25
		local l4=25
		local l5=30
	}
	else if `i'==5 {
		local unit="%"
		global maxd=20000
		*case3: exempt;
		local casename="CASE`i'"
		local varname="RepaymRate"
		local varlabname="Repayment Rate"
		preserve
		keep if Ex==15
		keep if Max==25
		*keep if Repay==10
		keep if Prin==21
		keep if iRate_Debt==0
		
		local l1=5
		local l2=8
		local l3=10
		local l4=10
		local l5=15
	}	
		
	if ${makegraphs}==1 {
	local typgraph="connected"
	local color1="gs10"
	local color3="black"
	local color5="gs7"
	local color4="gs9"
	local color2="gs9"
	
	if `l2'==`l3' {
		local color2="black"
	}
	if `l4'==`l3' {
		local color4="black"
	}
	local pattern1="lcolor(`color1') lpattern(solid) lwidth(.5) mcolor(`color1') msymbol(o) mlwidth(.2) msize(1.5)"
	local pattern2="lcolor(`color2') lpattern(solid) lwidth(.2) mcolor(`color2') msymbol(none) mlwidth(.2) msize(1.1)"
	local pattern3="lcolor(`color3') lpattern(solid) lwidth(.7) mcolor(`color3') msymbol(none) mlwidth(.2) msize(1.3)"
	local pattern4="lcolor(`color4') lpattern(solid) lwidth(.2) mcolor(`color4') msymbol(none) mlwidth(.2) msize(1.1)"
	local pattern5="lcolor(`color5') lpattern(solid) lwidth(.5) mcolor(`color5') msymbol(t) mlwidth(.2) msize(1.5)"
	
	local graph1 = ""
	local i=0
	forval a=5(-1)1{ 
		local ++i
		local graph1= "`graph1' (`typgraph' repaymyear pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	twoway `graph1', ///
	legend(pos(7) ring(0) col(3) size(normal) ///
			lab(5 "`varlabname' = `l1'`unit'") ///
			lab(3 "`l3'`unit' (Base)") ///
			lab(1 "`l5'`unit'") ///
			order(5 3 1))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			xsize(6) ysize(4) ylab(,nogrid)
	graph export "${GraphDir}/`casename'_REPAY.pdf", replace

	local graph1 = ""
	local i=0
	forval a = 5(-1)1 { 
		local ++i
		local graph1= "`graph1' (`typgraph' NPV pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	twoway `graph1', ///
	legend(pos(11) ring(0) col(3) size(normal) ///
			lab(5 "`varlabname' = `l1'`unit'") ///
			lab(3 "`l3'`unit' (Base)") ///
			lab(1 "`l5'`unit'") ///
			order(5 3 1))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			ylabel(0(10000)$maxd, nogrid) ///
			xsize(6) ysize(4) 
			
	graph export  "${GraphDir}/`casename'_NPV.pdf", replace
	}
	forval j=1/5 {
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
	sum subsidy if `varname'==`l5'
	global meansub5=r(mean)
	g meansub5=$meansub5
	
	if ${makegraphs}==1 {	
	
	local graph1 = ""
	local i=0
	forval a=5(-1)1{ 
		local ++i
		local graph1= "`graph1' (`typgraph' subsidy pcgroup if `varname'==`l`a'', `pattern`a'')"
	}
	di "`graph1'"
	
	qui sum meansub1 if pcgroup==100
	local extra = r(max)*0.03
	local ymax1 = r(max)+`extra'
	qui sum meansub3 if pcgroup==100
	local ymax2 = r(max)+`extra'
	qui sum meansub5 if pcgroup==100
	local ymax3 = r(max)+`extra'
	qui sum currentsub if pcgroup==100
	local ymax4 = r(max)+`extra'
	
	twoway (connected currentsub pcgroup, `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none)) ///
		   (connected meansub1 pcgroup if `varname'==`l1', `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none) mlwidth(.2) msize(1.2)) ///
		   (connected meansub3 pcgroup if `varname'==`l3', `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none) ) ///
		   (connected meansub5 pcgroup if `varname'==`l5', `pattern1' lcolor("gs13%70") lwidth(1) msymbol(none)   mlwidth(.2) msize(1.2)) ///
			`graph1' ///	
		   (scatter meansub1 pcgroup if `varname'==`l1' & pcgroup==100, `pattern1' msymbol(o) mlwidth(.4)) ///
		   (scatter meansub3 pcgroup if `varname'==`l3' & pcgroup==100, `pattern3' msymbol(|) mlwidth(1.5) ) ///
		   (scatter meansub5 pcgroup if `varname'==`l5' & pcgroup==100, `pattern5' msymbol(t) mlwidth(.4)) ///
			, ///
			text(1.02 104 "{it:Average}" "{it:Subsidy}", place(s) color("black") size(medium) bcolor(gs13%50) box margin(b=2 t=1 l=2 r=2.1) al(top) just(center)) ///
			text(`ymax1' 102 "{it:`l1'`unit'}", place(e) color("black") size(small) just(left)) ///
			text(`ymax2' 102 "{it:`l3'`unit' (Base)}", place(e) color("black") size(small) just(left) al(top)) ///
			text(`ymax3' 102 "{it:`l5'`unit'}", place(e) color("black") size(small) just(left)) ///
			text(`ymax4' 102 "{it:GTF}", place(e) color("black") size(small) just(left) al(bottom)) ///
			legend(pos(7) ring(0) col(3) size(normal) ///
			lab(9 "`varlabname' = `l1'`unit'") ///
			lab(7 "`l3'`unit' (Base)") ///
			lab(5 "`l5'`unit'") ///	
			lab(1 "GTF Subsidy") ///
			order(9 7 5))  xlabel(0 `" "0"  "Poorest" "' 10(10)90 100 `" "100"  "Richest" "', nogrid) ///
			ytitle("Subsidy As a Share of Loan") ///
			ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "', nogrid)  ///
			xsize(7) ysize(4)   ///
			plotregion(margin(r+0)) graphregion(margin(r+15)) xtitle(,margin(r+0))
			graph export  "${GraphDir}/`casename'_subsidy.pdf", replace
	

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
