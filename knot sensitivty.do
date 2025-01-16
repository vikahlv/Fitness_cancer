use "...\Analytical data v3.dta", clear
drop std_rpg1 std_rpg2 std_rpg3 std_rpg4

rename outcome_ANY_cancer_incidence outcome_cancerinc
rename outcome_ANY_cancer_death outcome_cancerdeath

rename v2_FU_ANY_cancer_incidence v2_FU_cancerinc
rename v2_FU_ANY_cancer_death v2_FU_cancerdeath

///Restrict to years 1972-1995
keep if monstar >=1972 & monstar <=1995

///Drop weirdos and women
drop if FU_ANY_cancer ==0
keep if kon == "1"

///Exclude those with missing exposure data
gen wmax_missing = 1 if wmax ==.
replace wmax_missing = 0 if wmax_missing ==.
keep if wmax_missing == 0


///Define covariates
gen education_mor = 1 if sun2020niva_old_mor == 1 | sun2020niva_old_mor ==2
replace education_mor = 2 if sun2020niva_old_mor == 3 | sun2020niva_old_mor == 4
replace education_mor = 3 if sun2020niva_old_mor == 5 
replace education_mor = 4 if sun2020niva_old_mor == 6 | sun2020niva_old_mor == 7

gen education_far = 1 if sun2020niva_old_far == 1 | sun2020niva_old_far ==2
replace education_far = 2 if sun2020niva_old_far == 3 | sun2020niva_old_far == 4
replace education_far = 3 if sun2020niva_old_far == 5 
replace education_far = 4 if sun2020niva_old_far == 6 | sun2020niva_old_far == 7

egen maxedu = rowmax(education_mor education_far)

egen maxincome = rowmax(quintile_income_mor quintile_income_far)

gen rounded_monstar=round(monstar, 5)
gen rounded_birthyear=round(birthyear, 5)

gen bmi_squared = bmi^2

///Exclude those with missing covariate data
gen bmi_missing = 1 if bmi ==.
replace bmi_missing = 0 if bmi_missing ==.
drop if bmi_missing==1
drop if maxedu ==.
drop if maxincome ==.

///Exclude extreme exposure values 
drop if wmax <100
drop if bmi <=15 | bmi>=60

///Define exposure
xtile wmax_quartile = wmax, nq(4)

///Create dummy variables
tab wmax_quartile, gen(dummywmax)
rename dummywmax1 wmax1
rename dummywmax2 wmax2
rename dummywmax3 wmax3
rename dummywmax4 wmax4

//Loopa över varje covariat och skapa dummies
foreach covar in rounded_monstar maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}
*Droppa referenser för covariater
*Spara alla dummies i ett gemensamt namn "$covar"
ds *dum*
global covar = "`r(varlist)'"

///FULL COHORT ANALYSES

**Count outcomes across quartiles
foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
bysort wmax_quartile: tab outcome_`outcome'
}

**Median age at event
gen age_cancerinc = v2_FU_cancerinc if outcome_cancerinc==1
gen age_cancerdeath = v2_FU_cancerdeath if outcome_cancerdeath==1
gen age_c43 = v2_FU_C43 if outcome_C43==1
gen age_c44 = v2_FU_C44 if outcome_C44==1
gen age_pro = v2_FU_PRO if outcome_PRO==1
gen age_OES = v2_FU_OES if outcome_OES==1
gen age_sto = v2_FU_STO if outcome_STO==1
gen age_col = v2_FU_COL if outcome_COL==1
gen age_rect = v2_FU_RECT if outcome_RECT==1
gen age_lbdg = v2_FU_LBDG if outcome_LBDG==1
gen age_pan = v2_FU_PAN if outcome_PAN==1
gen age_hn = v2_FU_HN if outcome_HN==1
gen age_kid = v2_FU_KID if outcome_KID==1
gen age_myel = v2_FU_MYEL if outcome_MYEL==1
gen age_lung = v2_FU_LUNG if outcome_LUNG==1
gen age_blad = v2_FU_BLAD if outcome_BLAD==1
sum age_cancerinc age_cancerdeath age_c43 age_c44 age_pro age_OES age_sto age_col age_rect age_lbdg age_pan age_hn age_kid age_myel age_lung age_blad, d

**Stpm2
range time 0 65 2

foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {
	 	
		if `knot'==3 local bknots = "10 90"
		if `knot'==3 local intknots = "50"

		if `knot'==4 local bknots = "5 95"
		if `knot'==4 local intknots = "35 65"
		
		if `knot'==5 local bknots = "5 95"
		if `knot'==5 local intknots = "27.5 50 72.5"
		
		if `knot'==6 local bknots = "5 95"
		if `knot'==6 local intknots = "23 41 59 77"
		
		if `knot'==7 local bknots = "2.5 97.5"
		if `knot'==7 local intknots = "18.33 34.17 50 65.83 81.67"
				
				stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 age_conscription bmi bmi_squared $covar, knots(`intknots') bknots(`bknots') knscale(centile) scale(hazard) eform
est store `outcome'_knot`knot'
	 }
	 
}
	foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {
	 	estimates restore `outcome'_knot`knot'
	est save "...\\`outcome'_knot`knot'", replace	
	 }
	}
		estout cancerdeath_knot*, cells("b(fmt(3)) ci(fmt(3)) ") keep(xb:wmax*)  eform //p(fmt(3))

		
		
		
			foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {

	 	
	 estimates use "...\\`outcome'_knot`knot'"	
	 	 	estimates store `outcome'_knot`knot'
		
		estimates restore `outcome'_knot`knot'
		estimates esample
		stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
		
		preserve 
		standsurv, failure at1(wmax2 0 wmax3 0 wmax4 0) at2(wmax2 1 wmax3 0 wmax4 0) at3(wmax2 0 wmax3 1 wmax4 0) at4(wmax2 0 wmax3 0 wmax4 1) timevar(time) atvar(`outcome'_1 `outcome'_2 `outcome'_3 `outcome'_4) ci contrast(difference) contrastvars(`outcome'_cont1 `outcome'_cont2 `outcome'_cont3)
		
	keep in 2/2
		keep `outcome'*
		save "...\\`outcome'_knot`knot'_std.dta", replace
		restore
		
	 }
	 }
	 
	 
	 
	
	 
	 
///SIB ANALYSES

//Restrict to siblings
gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

**Count outcomes across quartiles
foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
bysort wmax_quartile: tab outcome_`outcome' if antal >=2 & antal!=.
}

**Define within-family term for exposures and covariates 
egen wmax2_bw = mean(wmax2), by(famid)
egen wmax3_bw = mean(wmax3), by(famid)
egen wmax4_bw = mean(wmax4), by(famid)

foreach var in bmi bmi_squared rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}

**Stpm2

 
	 drop time
	 range time 0 65 2
	 
foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {
	 	
		if `knot'==3 local bknots = "10 90"
		if `knot'==3 local intknots = "50"

		if `knot'==4 local bknots = "5 95"
		if `knot'==4 local intknots = "35 65"
		
		if `knot'==5 local bknots = "5 95"
		if `knot'==5 local intknots = "27.5 50 72.5"
		
		if `knot'==6 local bknots = "5 95"
		if `knot'==6 local intknots = "23 41 59 77"
		
		if `knot'==7 local bknots = "2.5 97.5"
		if `knot'==7 local intknots = "18.33 34.17 50 65.83 81.67"
				
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw,  knots(`intknots') bknots(`bknots') knscale(centile) scale(hazard) vce(robust) eform
est store `outcome'_knot`knot'_bw
	 }
	 
}
	foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {
	 	estimates restore `outcome'_knot`knot'_bw
	est save "...\\`outcome'_knot`knot'_bw", replace
	 }
	}
		estout *knot* using output.xls, cells("b(fmt(3)) ci(fmt(3)) z(fmt(3))") keep(xb:wmax*) drop(*bw)  eform  replace

	
	
	
	
	foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    {

	 	
	 estimates use "...\\`outcome'_knot`knot'_bw"		
	 	 	estimates store `outcome'_knot`knot'_bw
		
		estimates restore `outcome'_knot`knot'_bw
		estimates esample
		stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
		
		preserve 
		standsurv, failure at1(wmax2 0 wmax3 0 wmax4 0) at2(wmax2 1 wmax3 0 wmax4 0) at3(wmax2 0 wmax3 1 wmax4 0) at4(wmax2 0 wmax3 0 wmax4 1) timevar(time) atvar(`outcome'_1 `outcome'_2 `outcome'_3 `outcome'_4) ci contrast(difference) contrastvars(`outcome'_cont1 `outcome'_cont2 `outcome'_cont3)
		
		keep in 2/2
		keep `outcome'*
		
		save "...\\`outcome'_knot`knot'_std_bw.dta", replace
		restore
		
	 }
	 }
	 
	 
	 
	 ********************
clear
		 gen knot = .
		 gen outcome = ""
		 
foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    { 
	 
	 append using "...\\`outcome'_knot`knot'_std.dta", 

		 replace knot = `knot' if knot==.
		 
		 		 replace outcome = "`outcome'" if outcome==""
	
	 }
}




	 
	 ********************
clear
		 gen knot = .
		 gen outcome = ""
		 
foreach outcome in cancerinc cancerdeath {
	
	 foreach knot of numlist  3(1)7    { 
	 
	 append using "...\\`outcome'_knot`knot'_std_bw.dta", 

		 replace knot = `knot' if knot==.
		 
		 		 replace outcome = "`outcome'" if outcome==""
	
	 }
}