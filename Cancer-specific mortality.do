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

**Stpm2

foreach outcome in  C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {

egen `outcome'_fe = rowmin(emigration_date death_date)


generate outcome_`outcome'_mort = 3 if emigration_date==`outcome'_fe & `outcome'_fe!=.
replace outcome_`outcome'_mort = 2 if death_date==`outcome'_fe & `outcome'_fe!=. &  `outcome'_mortality==.
replace outcome_`outcome'_mort = 1 if death_date==`outcome'_fe & `outcome'_fe!=. & `outcome'_mortality==1
replace outcome_`outcome'_mort = 0 if outcome_`outcome'_mort==. | `outcome'_fe>td(1jan2024)
replace `outcome'_fe = td(1jan2024) if `outcome'_fe==. | `outcome'_fe>td(1jan2024)
label define outcome_`outcome'_mort 0 "End of follow-up" 1 "Cancer-spec death" 2 "Other death" 3 "Emigration"
label values outcome_`outcome'_mort outcome_`outcome'_mort
}

foreach outcome in  C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD  {
generate FU_`outcome'_mort = (`outcome'_fe-date(fodelsemanad, "YM"))/365.25
}
drop *_fe

foreach outcome in  C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {

tab outcome_`outcome'_mort if outcome_`outcome'_mort==1

}




*C44 MYEL BLAD
foreach outcome in   C43  PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG  {
stset FU_`outcome'_mort, fail(outcome_`outcome'_mort==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform  failconvlininit 
est store `outcome'_quartile


}



	foreach outcome in C43  PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG  {
	
	
	 	estimates restore `outcome'_quartile
	est save "...\\`outcome'_mort_specific", replace
	 
	}

esttab *_quartile,   cells("b(fmt(2)) ci(fmt(2))") keep(xb:*wmax*) eform replace


esttab C43_quartile using output.xlsx,   cells("b(fmt(2)) ci(fmt(2))") keep(xb:*wmax*) eform replace
	