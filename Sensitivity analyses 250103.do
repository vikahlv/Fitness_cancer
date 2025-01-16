use ".../Analytical data v3.dta"
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


///Full cohort BMI interaction


///Create dummy BMI cats
tab bmicat, gen(dummybmi)
rename dummybmi1 bmi1
rename dummybmi2 bmi2
rename dummybmi3 bmi3
rename dummybmi4 bmi4

gen bmi1wmax2=bmi1*wmax2
gen bmi2wmax2=bmi2*wmax2
gen bmi3wmax2=bmi3*wmax2
gen bmi4wmax2=bmi4*wmax2

gen bmi1wmax3=bmi1*wmax3
gen bmi2wmax3=bmi2*wmax3
gen bmi3wmax3=bmi3*wmax3
gen bmi4wmax3=bmi4*wmax3

gen bmi1wmax4=bmi1*wmax4
gen bmi2wmax4=bmi2*wmax4
gen bmi3wmax4=bmi3*wmax4
gen bmi4wmax4=bmi4*wmax4


///Cancer mort stpm2

stset v2_FU_cancerdeath, fail(outcome_cancerdeath==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 age_conscription bmi1 bmi3 bmi4 bmi1wmax2 bmi3wmax2 bmi4wmax2 bmi1wmax3 bmi3wmax3 bmi4wmax3 bmi1wmax4 bmi3wmax4 bmi4wmax4 $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform


///STANDSURV FOR BMI CATEGORIES

range time 0 65 2

///HR in underweight 
standsurv, hazard ///
at1(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 1 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 1 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 1 bmi1wmax4 0) ///
at4(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_uwcontrast1 death_uwcontrast2 death_uwcontrast3)
	list *_uwcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in normalweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_normcontrast1 death_normcontrast2 death_normcontrast3)

	list *_normcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in overweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 0) ///
at2(bmi1 0 bmi3 1 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi3wmax2 1 bmi3wmax3 0 bmi3wmax4 0) ///
at3(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi3wmax2 0 bmi3wmax3 1 bmi3wmax4 0) ///
at4(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_owcontrast1 death_owcontrast2 death_owcontrast3)

	list *_owcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
		///HR in obesity 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 0 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 1 wmax2 1 wmax3 0 wmax4 0 bmi4wmax2 1 bmi4wmax3 0 bmi4wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 1 wmax4 0 bmi4wmax2 0 bmi4wmax3 1 bmi4wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 1 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_obecontrast1 death_obecontrast2 death_obecontrast3)

	list *_obecontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in total population 
	standsurv, hazard ///
at1(wmax2 0 wmax3 0 wmax4 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 ) ///
at3(wmax2 0 wmax3 1 wmax4 0) ///
at4(wmax2 0 wmax3 0 wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_totalcontrast1 death_totalcontrast2 death_totalcontrast3)

	list *death_totalcontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	********************
	
	
///Cancer incidence stpm2

stset v2_FU_cancerinc, fail(outcome_cancerinc==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 age_conscription bmi1 bmi3 bmi4 bmi1wmax2 bmi3wmax2 bmi4wmax2 bmi1wmax3 bmi3wmax3 bmi4wmax3 bmi1wmax4 bmi3wmax4 bmi4wmax4 $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform


///STANDSURV FOR BMI CATEGORIES

///HR in underweight 
standsurv, hazard ///
at1(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 1 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 1 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 1 bmi1wmax4 0) ///
at4(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_uwcontrast1 inc_uwcontrast2 inc_uwcontrast3)

	list *inc_uwcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	
	///HR in normalweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_normcontrast1 inc_normcontrast2 inc_normcontrast3)

	list *inc_normcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in overweight 
standsurv, hazard /// 
at1(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 0) ///
at2(bmi1 0 bmi3 1 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi3wmax2 1 bmi3wmax3 0 bmi3wmax4 0) ///
at3(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi3wmax2 0 bmi3wmax3 1 bmi3wmax4 0) ///
at4(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_owcontrast1 inc_owcontrast2 inc_owcontrast3)

	list *inc_owcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

		///HR in obesity 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 0 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 1 wmax2 1 wmax3 0 wmax4 0 bmi4wmax2 1 bmi4wmax3 0 bmi4wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 1 wmax4 0 bmi4wmax2 0 bmi4wmax3 1 bmi4wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 1 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_obecontrast1 inc_obecontrast2 inc_obecontrast3)

	list *inc_obecontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	///HR in total population 
	standsurv, hazard ///
at1(wmax2 0 wmax3 0 wmax4 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 ) ///
at3(wmax2 0 wmax3 1 wmax4 0) ///
at4(wmax2 0 wmax3 0 wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_totalcontrast1 inc_totalcontrast2 inc_totalcontrast3)

	list *inc_totalcontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	
///Full cohort without BMI adjustment 

foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 age_conscription $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform
est store `outcome'_quartile
}
						
///Sib analysis, BMI interaction and HRs in separate BMI categories

//Restrict to siblings
gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.


**Define within-family term for exposures and covariates 
egen wmax2_bw = mean(wmax2), by(famid)
egen wmax3_bw = mean(wmax3), by(famid)
egen wmax4_bw = mean(wmax4), by(famid)


foreach var in bmi bmi_squared bmi1 bmi2 bmi3 bmi4 bmi1wmax2 bmi2wmax2 bmi3wmax2 bmi4wmax2 bmi1wmax3 bmi2wmax3 bmi3wmax3 bmi4wmax3 bmi1wmax4 bmi2wmax4 bmi3wmax4 bmi4wmax4 rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}

///Cancer mort stpm2

stset v2_FU_cancerdeath, fail(outcome_cancerdeath==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw bmi1 bmi3 bmi4 bmi1_bw bmi3_bw bmi4_bw bmi1wmax2 bmi3wmax2 bmi4wmax2 bmi1wmax3 bmi3wmax3 bmi4wmax3 bmi1wmax4 bmi3wmax4 bmi4wmax4 bmi1wmax2_bw bmi3wmax2_bw bmi4wmax2_bw bmi1wmax3_bw bmi3wmax3_bw bmi4wmax3_bw bmi1wmax4_bw bmi3wmax4_bw bmi4wmax4_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///STANDSURV FOR BMI CATEGORIES

range time 0 65 2

///HR in underweight 
standsurv, hazard ///
at1(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 1 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 1 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 1 bmi1wmax4 0) ///
at4(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_uwcontrast1 death_uwcontrast2 death_uwcontrast3)
	list *_uwcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in normalweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_normcontrast1 death_normcontrast2 death_normcontrast3)

	list *_normcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in overweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 0) ///
at2(bmi1 0 bmi3 1 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi3wmax2 1 bmi3wmax3 0 bmi3wmax4 0) ///
at3(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi3wmax2 0 bmi3wmax3 1 bmi3wmax4 0) ///
at4(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_owcontrast1 death_owcontrast2 death_owcontrast3)

	list *_owcontrast* if time==65, noobs
	
	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
		///HR in obesity 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 0 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 1 wmax2 1 wmax3 0 wmax4 0 bmi4wmax2 1 bmi4wmax3 0 bmi4wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 1 wmax4 0 bmi4wmax2 0 bmi4wmax3 1 bmi4wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 1 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_obecontrast1 death_obecontrast2 death_obecontrast3)

	list *_obecontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in total population 
	standsurv, hazard ///
at1(wmax2 0 wmax3 0 wmax4 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 ) ///
at3(wmax2 0 wmax3 1 wmax4 0) ///
at4(wmax2 0 wmax3 0 wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(death_totalcontrast1 death_totalcontrast2 death_totalcontrast3)

	list *death_totalcontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	********************
	
	
///Cancer incidence stpm2

stset v2_FU_cancerinc, fail(outcome_cancerinc==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw bmi1 bmi3 bmi4 bmi1_bw bmi3_bw bmi4_bw bmi1wmax2 bmi3wmax2 bmi4wmax2 bmi1wmax3 bmi3wmax3 bmi4wmax3 bmi1wmax4 bmi3wmax4 bmi4wmax4 bmi1wmax2_bw bmi3wmax2_bw bmi4wmax2_bw bmi1wmax3_bw bmi3wmax3_bw bmi4wmax3_bw bmi1wmax4_bw bmi3wmax4_bw bmi4wmax4_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///STANDSURV FOR BMI CATEGORIES

///HR in underweight 
standsurv, hazard ///
at1(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 1 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 1 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 1 bmi1wmax4 0) ///
at4(bmi1 1 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_uwcontrast1 inc_uwcontrast2 inc_uwcontrast3)

	list *inc_uwcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	
	///HR in normalweight 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi1wmax2 0 bmi1wmax3 0 bmi1wmax4 0) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_normcontrast1 inc_normcontrast2 inc_normcontrast3)

	list *inc_normcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	///HR in overweight 
standsurv, hazard /// 
at1(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 0 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 0) ///
at2(bmi1 0 bmi3 1 bmi4 0 wmax2 1 wmax3 0 wmax4 0 bmi3wmax2 1 bmi3wmax3 0 bmi3wmax4 0) ///
at3(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 1 wmax4 0 bmi3wmax2 0 bmi3wmax3 1 bmi3wmax4 0) ///
at4(bmi1 0 bmi3 1 bmi4 0 wmax2 0 wmax3 0 wmax4 1 bmi3wmax2 0 bmi3wmax3 0 bmi3wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_owcontrast1 inc_owcontrast2 inc_owcontrast3)

	list *inc_owcontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

		///HR in obesity 
standsurv, hazard ///
at1(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 0 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 0) ///
at2(bmi1 0 bmi3 0 bmi4 1 wmax2 1 wmax3 0 wmax4 0 bmi4wmax2 1 bmi4wmax3 0 bmi4wmax4 0) ///
at3(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 1 wmax4 0 bmi4wmax2 0 bmi4wmax3 1 bmi4wmax4 0) ///
at4(bmi1 0 bmi3 0 bmi4 1 wmax2 0 wmax3 0 wmax4 1 bmi4wmax2 0 bmi4wmax3 0 bmi4wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_obecontrast1 inc_obecontrast2 inc_obecontrast3)

	list *inc_obecontrast* if time==65, noobs
		drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci

	///HR in total population 
	standsurv, hazard ///
at1(wmax2 0 wmax3 0 wmax4 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 ) ///
at3(wmax2 0 wmax3 1 wmax4 0) ///
at4(wmax2 0 wmax3 0 wmax4 1) ///
timevar(time) ///
ci contrast(ratio) contrastvars(inc_totalcontrast1 inc_totalcontrast2 inc_totalcontrast3)

	list *inc_totalcontrast* if time==65, noobs

	drop _at1 _at1_lci _at1_uci _at2 _at2_lci _at2_uci _at3 _at3_lci _at3_uci _at4 _at4_lci _at4_uci
	
	*******
	

///Sib analysis without BMI adjustment 
foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax2_bw wmax3_bw wmax4_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform
est store `outcome'_quartilebw
}
						
///Standard analysis in sib cohort 

foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 bmi bmi_squared age_conscription rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform
}
