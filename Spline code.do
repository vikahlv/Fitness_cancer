mkspline wmaxsplines = wmax, nknots(4) cubic displayknots
mat knots = r(knots)

foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stcox wmaxsplines* age_conscription bmi bmi_squared $covar, ///
			 nohr
			
levelsof wmax
        xbrcspline wmaxsplines, values(`r(levels)') ref(217) matknots(knots) eform gen(`outcome'_graph `outcome'_hr lb_`outcome' ub_`outcome')	
		 }
		 
keep *_graph* *_hr* *lb_* ub_*
save ".../Spline graph data cohort.dta", replace
								
/////Sib-splines							

mkspline wmaxsplines = wmax, nknots(4) cubic displayknots
mat knots = r(knots)
			
keep if antal >=2 & antal!=.
egen wmaxspline_bw1 = mean(wmaxsplines1), by(famid)
egen wmaxspline_bw2 = mean(wmaxsplines2), by(famid)
egen wmaxspline_bw3 = mean(wmaxsplines3), by(famid)

foreach outcome in cancerinc cancerdeath C43 C44 PRO OES STO COL RECT LBDG PAN HN KID MYEL LUNG BLAD {
stset v2_FU_`outcome', fail(outcome_`outcome'==1) enter (age_conscription)
stcox wmaxsplines* wmaxspline_bw* bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, ///
			 nohr 
			
levelsof wmax
        xbrcspline wmaxsplines, values(`r(levels)') ref(217) matknots(knots) eform gen(`outcome'_graph_bw `outcome'_hr_bw lb_`outcome'_bw ub_`outcome'_bw)	
		 }
		 
keep *_graph_bw* *_hr_bw* *lb_* ub_*

save ".../Spline graph data siblings.dta", replace

