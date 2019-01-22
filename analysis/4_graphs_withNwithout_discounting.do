pause off

cap set scheme plotplainblind, permanently

local s=3
local i=0
local e=4

* *graphS
use "$StataOut/Case_s`s'_e`e'.dta", clear
local unit="K"

local casename="baseline"
preserve
global maxd=25000
keep if Prin==21
keep if Ex==15
keep if Max==25
keep if Repay==10
keep if iRate_Debt==0

g debtline=21000
twoway (line NPV pcgroup if iRate_Discount==0) ///	
(line NPV pcgroup if iRate_Discount==22) ///
(line debtline pcgroup), ///
legend(pos(5) ring(0) col(1) size(normal) ///
lab(2 "Discounting") ///
lab(1 "No Discounting") order(1 2)) ///
ytitle("NPV") ///
xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') ///
ylabel(0(5000)$maxd)
*graph save  "${GraphDir}/`casename'_NPV.gph", replace
graph export  "${GraphDir}/`casename'_NPV_disc.pdf", replace
pause
sum subsidy if iRate_Discount==0
global meansub=r(mean)
g meansubND=$meansub
sum subsidy if iRate_Discount==22
global meansub=r(mean)
g meansubD=$meansub
g currentsub=.8
twoway (line subsidy pcgroup if iRate_Discount==0) ///
	   (line subsidy pcgroup if iRate_Discount==22) ///
	   (line meansubND pcgroup)  ///
	   (line meansubD pcgroup), ///
		legend(pos(2) ring(0) col(1) size(normal) ///
		lab(1 "Subsidy (No Discounting)") ///
		lab(2 "Subsidy (Discounting {&beta}=0.978)") ///
		lab(3 "Average subsidy (No Discounting)") ///
		lab(4 "Average subsidy (Discounting)")) ///
		ytitle("Subsidy as a share of loan") ylabel(0 `" "0"  "full" "tuition" "' .2(.2).8 1 `" "1"  "free" "tuition" "') ///
		xlabel(0 `" "0"  "poorest" "' 10(10)90 100 `" "100"  "richest" "') 
*graph save  "${GraphDir}/`casename'_subsidy.gph", replace
graph export  "${GraphDir}/`casename'_subsidy_disc.pdf", replace
restore

