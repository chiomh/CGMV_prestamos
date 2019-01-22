* This code runs several experiments using the simulated earnings and 
* percentile rankings calculated in rocio_dic17. See main texts for
* a detailed description of the experiments and parameters. It then calculates 3
* indicators of payments burden over the lifetime-earnings distribution:
*
* 1. Number of years to repay all debt
* 2. Net present value of repayments
* 3. Subsidy 
* 
* All the experiment combinations are stored in Case_s`s'_e`e'.dta, 
* for sex=1(males),2(females),3(all) and education=4(college grads).
*
* Rocio Madera
* Last: 12/20/2017
*---------------------------------------------------------------------------

clear all
cap log close
set more off 
pause off
*set scheme colorblind, perm
set matsize 11000, permanently

global StataOut "$StataOut/../output_q"
cd "$StataOut"

global perc 100 

* Model parameters
global agemin = 22
global agemax = 60

* Base parameters
global debt_base=21000	// Amount of debt at the end of college
global r_base=0.0	// nominal interest rate 
global exempt_base=15000 // exemption income level
global maxyears_base=25 // years after which the debt is "forgiven"
global repaym_base=0.1   // repayment rate
global disc_base=0.022   // repayment rate
	
* LIST Different versions
* Add/remove values as needed
global debtlist $debt_base // 5000 10000  30000 40000
global rlist $r_base // 0.005 0.008 0.015 0.022
global exemptlist $exempt_base // 10000 20000 25000
global maxyrlist $maxyears_base // 15 20 30 
global repaymlist $repaym_base // 0.05 0.08 0.15 
global disclist $disc_base // 0

set more off
forvalues e = 4/4 {	 // Education - 4 (College grads)
forvalues s = 3/3 {	// Sex - 1 (Males) 2 (Females) 3 (All)

	local h=0 // this is just to create database with all cases
		foreach d of global debtlist{	// debt principal	
		foreach r of global rlist{		// interest rate
		foreach x of global exemptlist{	// exemption level
		foreach m of global maxyrlist {	// years before debt write-off
		foreach p of global repaymlist{	// repayment rate
		foreach b of global disclist{	// discounting 
		local ++h
		
		qui { 
		
		use "${StataOut}/simpanel_e`e'",clear
		
		if `s'<3 {
			drop pcgroup
			keep if sex==`s'
			rename pcgroup_s`s' pcgroup
			
		}
		drop pcgroup_*
		
		pause
		*--------
		* LOANS
		*--------
		xtset ind age
		* ----------------------------------------
		* repayments 
		* (only dependent on income and exemption);
		* ----------------------------------------
		replace rearnsM=rearnsM
		* Income considered for repayment calculations: excess above x
		g y_abovemin=rearnsM-`x'
		replace y_abovemin=0 if y_abovemin<0
		
		* Annual payments: linear in y
		g anpaym = y_abovemin * `p'
		
		* Calculate accumulated payments;
		g accpaym = anpaym if age ==1
		replace accpaym=L.accpaym + anpaym if age>1				
		
		* ----------------------------------------
		* debt (recursive, depends on 
		* outstanding debt and repayments each 
		* year);
		* ----------------------------------------
		xtset ind age
		
		* AGE=1
		* debt == principal
		g debt = `d' if age==1
		* outstanding = debt - paym
		g outs_debt = `d'-anpaym if age==1
		
		* AGE>1
		* debt=(1+r)*(outs_debt)
		* outs_debt = debt - paym
		
		forval ag=2/39 {
			replace debt = L.outs_debt*(1+`r') if ag==`ag'
			replace outs_debt = debt-anpaym if ag==`ag'
		}

		* ----------------------------------------
		* years until repayment;
		* ----------------------------------------
		
		* whenever the corresponding debt at the beginning of the period is 
		* less than the annual payment
		g yesnorepaid = (anpaym > debt) 
		label define repaid 0 "Not Paid" 1 "Paid", modify
		label values yesnorepaid repaid
		
		* fill in year for individual
		bys ind: g temp = sum(yesnorepaid)
		g repaymyear0 = age if temp == 1
		bys ind: egen repaymyear = mean(repaymyear0)
		
		* last year and years after repayment
		replace anpaym=debt if age==repaymyear
		replace accpaym=L.accpaym+anpaym if age==repaymyear
		replace anpaym=0 if age>repaymyear
		replace accpaym=L.accpaym if age>repaymyear
		replace debt=0 if age>repaymyear
		replace outs_debt=0 if age>=repaymyear

		* calcular total pagado en la vida (tiene que ser 21000 si no hay 
		* intereses y se paga todo)
		
		* --- PRIMERO, SIN IMPONER MAXIMUM YEARS
		* el que paga todo "at some point" antes de los 60
		gen totalpaym0 = accpaym if age==repaymyear
		* el que no paga todo ni siquiera a los 60 
		replace totalpaym0 = accpaym if age==39 & repaymyear==.			
		* fill in all years by individual
		bys ind: egen totalpaym = mean(totalpaym0)

		* Just for comparison, I save the amounts if there was no writeoff year
		g repaymyear_nowriteoff = repaymyear
		g accpaym_nowriteoff = accpaym
		g totalpaym_nowriteoff = totalpaym
		
		* --- IMPOSE MAXIMUM YEAR FOR REPAYMENT
		replace repaymyear = `m' if temp==0
		replace repaymyear = `m' if repaymyear>`m'
		drop temp repaymyear0 totalpaym0

		* last year and years after repayment
		replace anpaym=0 if age>repaymyear
		replace accpaym=L.accpaym if age>repaymyear
		replace debt=0 if age>repaymyear
		replace outs_debt=0 if age>=repaymyear
		g totalpaym0 = accpaym if age==repaymyear
		bys ind: egen temp = mean(totalpaym0)
		replace totalpaym=temp
		drop temp totalpaym0
		
		* ----------------------------------------
		* Calculate present value
		* ----------------------------------------
		g yy = rearnsM 
		global r_disc=`b'
		g NPV0 = .
		replace NPV0=anpaym/((1+${r_disc})^(age-1))
		g NPV0y = .
		replace NPV0y=yy/((1+${r_disc})^(age-1))
	
		bys ind: egen NPV=sum(NPV0)
		bys ind: egen NPVy=sum(NPV0y)

		pause
		
		* SUBSIDY (different definitions)
		g subsidy=1-NPV/`d'		
		*g subsidy=1-totalpaym/totalpaym_nowriteoff
		*g subsidy=1-totalpaym/`d'

		* ------------------------------------------------------
		* Calculate earnings net of debt at different ages
		* ------------------------------------------------------
		// g LTynet=NPVy-NPV
		replace age=age+25-1
		levelsof age, local(agelist)
		foreach a of local agelist {
			g ynet`a'=yy-debt if age==`a'
			g lynet`a'=log(ynet`a')
		}
		g LTynet=NPVy-NPV
		g lLTynet=log(LTynet)
		
		preserve
		keep ind age *ynet* 
		
		local d1=`d'/1000
		local x1=`x'/1000
		local r1=`r'*1000
		local r2=${r_disc}*1000
		local p1=`p'*100

		*replace subsidy=subsidy*100
		
		* present value of repayments
		
		g Principal=`d1'
		g MaxYears=`m'
		g iRate_Debt=`r1'
		g iRate_Discount=`r2'
		g ExemptLev=`x1'
		g RepaymRate=`p1'
		
		save "${StataOut}/ynet", replace
		restore
		collapse (mean) repaymyear NPV NPVy subsidy *ynet*, by(pcgroup)

		*-------
		* SAVE COLLAPSED
		*-------
		
		label var repaymyear "Repayment Year (Years since graduation)"
		label var NPV "Net Present Value of Repayments"
		label var pcgroup "Percentiles of Lifetime Income Distribution"
		label var subsidy "Subsidy (Discounted)"

		local d1=`d'/1000
		local x1=`x'/1000
		local r1=`r'*1000
		local r2=${r_disc}*1000
		local p1=`p'*100

		*replace subsidy=subsidy*100
		
		* present value of repayments
		
		g Principal=`d1'
		g MaxYears=`m'
		g iRate_Debt=`r1'
		g iRate_Discount=`r2'
		g ExemptLev=`x1'
		g RepaymRate=`p1'
		
		label var Principal "Loan principal, i.e. total cost of college (in 1000s)"
		label var MaxYears "Maximum years for repayment"
		label var iRate_Debt "Interest rate on debt (x1000)"
		label var iRate_Discount "Interest rate to discount, i.e. (1/beta)-1 (x1000)"
		label var ExemptLev "Amount exempt from payment (in 1000s)"
		label var RepaymRate "Amount exempt from payment (in percent)"
		
		if `h'>1 append using Case_s`s'_e`e'
		saveold Case_s`s'_e`e', replace
		
		
		} // p
		} // m
		} // x
		} // r
		} // d		
		} // b
		
		
		} // qui/noi
	
} //e
} // s 
*reshape wide , i(pcgroup) j(MaxYears)
