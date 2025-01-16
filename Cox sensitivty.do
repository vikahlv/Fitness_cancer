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

foreach outcome in cancerinc cancerdeath {
	

				
				stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stcox wmax2 wmax3 wmax4 age_conscription bmi bmi_squared $covar,  hr
est store `outcome'_Cox

	est save "...\results output\\`outcome'_Cox", replace	
	 
	 
}

		*estout cancerdeath_knot*, cells("b(fmt(3)) ci(fmt(3)) ") keep(xb:wmax*)  eform //p(fmt(3))

		
		
		
	
	 
	 
	 
	
	 
	 
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


	 
foreach outcome in cancerinc cancerdeath {
	

	
				
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stcox wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw,   vce(robust) hr 
est store `outcome'_Cox_bw

	est save "...\results output\\`outcome'_Cox_bw", replace
	 
	}
		*estout *knot* using output.xls, cells("b(fmt(3)) ci(fmt(3)) z(fmt(3))") keep(xb:wmax*) drop(*bw)  eform  replace

	
	