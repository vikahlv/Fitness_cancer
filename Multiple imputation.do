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

///Exclude extreme exposure values 
drop if wmax <100
drop if (bmi <=15 | bmi>=60 ) & bmi!=.


///Define exposure
xtile wmax_quartile = wmax, nq(4)

///Create dummy variables
tab wmax_quartile, gen(dummywmax)
rename dummywmax1 wmax1
rename dummywmax2 wmax2
rename dummywmax3 wmax3
rename dummywmax4 wmax4

//Loopa över varje covariat och skapa dummies
foreach covar in  rounded_monstar maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}
*Droppa referenser för covariater
*Spara alla dummies i ett gemensamt namn "$covar"
ds *monstar*dum*  age_conscription 
global aux = "`r(varlist)'"

ds *maxedu*dum* *maxinco*dum*  bmi  lngd_i_cm mass_i_kg wmax2 wmax3 wmax4
global target = "`r(varlist)'"

mdesc $aux
mdesc $target

*sample 5
 mi set wide
 mi register imputed wmax_quartile maxedu maxincome  bmi  lngd_i_cm mass_i_kg bmi_squared

 
mi impute chained (regress) bmi bmi_squared lngd_i_cm mass_i_kg (mlogit) wmax_quartile maxedu maxincome = age_conscription i.rounded_monstar i.outcome_cancerinc i.outcome_cancerdeath v2_FU_cancerinc v2_FU_cancerdeath, add(20)  augment chaindots
*mi passive: gen bmi_squared = bmi^2 // generate the passive in each imputation set


foreach outcome in cancerinc cancerdeath {
	

mi stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
mi estimate, dots  cmdok 	sav("...\results output\\`outcome'_MI",replace): stpm2 i.wmax_quartile bmi bmi_squared i.maxedu i.maxincome age_conscription i.rounded_monstar , knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard)  nolog eform 

}
/*
///Exclude those with missing covariate data
gen bmi_missing = 1 if bmi ==.
replace bmi_missing = 0 if bmi_missing ==.
drop if bmi_missing==1
drop if maxedu ==.
drop if maxincome ==.

///Exclude those with missing exposure data
gen wmax_missing = 1 if wmax ==.
replace wmax_missing = 0 if wmax_missing ==.
*keep if wmax_missing == 0
*/

estimates use "...\results output\cancerinc_MI"
estimates store cancerinc_MI

estimates use "...\results output\cancerdeath_MI"
estimates store cancerdeath_MI

estout *MI,  cells("b(fmt(3)) ci(fmt(3)) z(fmt(3))") keep(xb:*.wmax*)   eform  replace

***** SIBLINGS
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

///Exclude extreme exposure values 
drop if wmax <100
drop if (bmi <=15 | bmi>=60 ) & bmi!=.


///Define exposure
xtile wmax_quartile = wmax, nq(4)

///Create dummy variables
tab wmax_quartile, gen(dummywmax)

//Loopa över varje covariat och skapa dummies
foreach covar in  rounded_monstar maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}
*Droppa referenser för covariater
*Spara alla dummies i ett gemensamt namn "$covar"
ds *monstar*dum*  age_conscription 
global aux = "`r(varlist)'"

ds *maxedu*dum* *maxinco*dum*  bmi  lngd_i_cm mass_i_kg wmax2 wmax3 wmax4
global target = "`r(varlist)'"

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

  mi set wide
 mi register imputed wmax_quartile maxedu maxincome  bmi  lngd_i_cm mass_i_kg bmi_squared
mi impute chained (regress) bmi bmi_squared lngd_i_cm mass_i_kg (mlogit) wmax_quartile maxedu maxincome = age_conscription i.rounded_monstar i.outcome_cancerinc i.outcome_cancerdeath v2_FU_cancerinc v2_FU_cancerdeath, add(20)  augment chaindots
*mi passive: gen bmi_squared = bmi^2 // generate the passive in each imputation set

/*foreach var in wmax2 wmax3 wmax4  {
mi xeq: drop `var'_bw 
}

foreach var in wmax2 wmax3 wmax4  {
mi xeq: drop `var'
}
*/
 foreach num of numlist   1(1)4 {
mi xeq: generate wmax`num' = 1 if wmax_quartile==`num'
mi xeq:  replace wmax`num' = 0 if wmax_quartile!=`num'
}



foreach var in wmax2 wmax3 wmax4 bmi bmi_squared rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
mi xeq: bys famid: egen `var'_bw = mean(`var')

}


foreach outcome in cancerinc cancerdeath {
mi stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
mi estimate, dots  cmdok 	sav("...\results output\\`outcome'_MI_bw",replace): stpm2 wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw,  knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

}


estimates use "...\results output\cancerinc_MI_bw"
estimates store cancerinc_MI

estimates use "...\results output\cancerdeath_MI_bw"
estimates store cancerdeath_MI

estout *MI,  cells("b(fmt(3)) ci(fmt(3)) z(fmt(3))") keep(xb:wmax*) drop(*bw)   eform  replace stats(N)


