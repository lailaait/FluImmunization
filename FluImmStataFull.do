cd C:\Users\Laila\Desktop\Stata_microecon
import excel florida_flu_merged.xlsx,  firstrow clear
set more off

* 1. First observations
destring imm_rate, replace
destring ab_flu_per, replace
destring ab_non_flu_per, replace
destring bmi, replace

gen age = age_months / 12
tab school
tab school_grade
 gen grade1=.
 replace grade1=1 if school_grade=="A"
 replace grade1=2 if school_grade=="B"
 replace grade1=3 if school_grade=="C"
 replace grade1=4 if school_grade=="D"
 replace grade1=5 if school_grade=="F"
 
 label define labgrade1 1 "A" 2 "B" 3 "C" 4 "D" 5 "F"
 label values grade1 labgrade1 
 
 tab grade1 // first impression is that the grades distribution is quite biased towards higher grades
 tab grade1 if year==2011
 tab grade1 if year==2012
 tab grade1 if year==2013  
 // General obs:  in 2011, huge bias towards grade A (68.41%); in 2012, big amount of grades A and B (resp. 53.67% and 21.58%), and in 2013, 1/3 in A, and around 25% each for B and C.
 
sort id year school teacher grade1
order id year school teacher age_months bmi grade1 race ab_flu flu_days ab_flu_per ab_non_flu_per imm cf v imm_rate 

* a. Immunization variables
tab imm
tab v
tab cf
tab imm cf // nb of common obs drops from  11,287 (for cf) and 12,477 (for imm) to 8,643 (cross table) << problem of missing values
tab v cf  // 11 287 values, id est size of cf << no longer any missing values - no need to use imm until robustness check, we'll work with v

gen v01=0 if v~=""   // new variable, equals 0 if variable not missing
replace v01=1 if v=="Yes" // we fill the TRUE values
tab v01

gen imm01=0 if imm~=""   // new variable, equals 0 if variable not missing
replace imm01=1 if imm=="TRUE" // we fill the TRUE values
tab imm01

gen cf01=0 if cf~=""
replace cf01=1 if cf=="Yes"
tab cf01

// Conclusion : we'll use v as a proxy to measure the immunization of the kid, and the consent form as an instrument influencing the propensity of a kid to be immunized or not.

* b. Social & ethnic variables
tab lunch lunch_rec // checking the corresponding codes
tab lunch_rec

gen lunch01= lunch
recode lunch01 (0/1=0) (2/9=1) 

gen cat01=0 if cat~=""
	replace cat01=1 if cat=="normal"
	replace cat01=2 if cat=="overweight"
	replace cat01=3 if cat=="obese"
	label define labcat01 1 "normal" 2 "overweight" 3 "obese"
	label values cat01 labcat01

* Since the behaviour of all races isn't heterogeneous, we won't use the white/nonwhite variable but more the race variable.
* Indians aren't really numerous and behave quite like mixed race children, so we group them in 1 category
* Recoding it in numbers:
tab race
 gen race5=.
 replace race5=1 if race=="W"
 replace race5=2 if race=="B"
 replace race5=3 if race=="M" | race=="I"
 replace race5=4 if race=="H" 
 replace race5=5 if race=="A"
label define labrace5 1 "W" 2 "B" 3 "M&I" 4 "H" 5 "A"
label values race5 labrace5
tab race5 
 tabstat ab_flu_per  ab_non_flu_per, by(race5) // Unconditionally, B have more ab than W.
// But conditionally, in the biprobit (ab_flu01_2, ab_flu01_3, grippe), they have less absenteeism!

 tab school   // lots of unique values -  Stata will get rid of them during the estimation though
 tab school, gen(scol)
 tabstat scol*, stat(sum)
 // Checking that the imm_rate comes from another bulk of data since we can get an immrate for school w/ only 1 obs!
 list school year teacher imm imm_rate if scol18==1 // 

 
 
* c. Absences
// Idea: we can't know why kids are missing school (not precised in the data). Median of missing days during the flu season = 1, however if kids are sick.
tabstat flu_days, by(year) // 10 63 48 : huge gap between 2011 and the other years
// We'll assess now several thresholds:
tab ab_flu 
gen ab_flu01_2=ab_flu
recode ab_flu01_2 (0/2=0) (3/max=1) // recoding that way helps getting thresholds for the flu
tab ab_flu01_2
gen ab_flu01_3=ab_flu
recode ab_flu01_3 (0/3=0) (4/max=1)
tab ab_flu01_3

gen ab_flu01_4=ab_flu
recode ab_flu01_4 (0/4=0) (5/max=1)
tab ab_flu01_4

gen ab_flu01_5=ab_flu
recode ab_flu01_5 (0/5=0) (6/max=1)
tab ab_flu01_5

* Percentage of kids for each absence duration during the flu period :
	tabstat flu_days, by(year) // 10 63 48
	tab ab_flu if year==2011  // 0 à 2j = 96% ; 0 à 3j = 98% ; 0 à 4j = 99%
	tab ab_flu if year==2012  // 0 à 2j = 53% ; 0 à 3j = 65% ; 0 à 4j = 75%
	tab ab_flu if year==2013  // 0 à 2j = 58% ; 0 à 3j = 71% ; 0 à 4j = 79%

	/// Conclusion : even though we can't attribute absenteeism to influenza, we can admit that the real gap in number of days where the kid is missing is between 2 and 3 days (intuitively), but we'll test all the thresholds.
* 2. Choice of instrumental variables
* a. The choice of instrumental variables and the limits

*----- Should we use cf with imm ?
// We need to evaluate what kind of data we get rid of when we use imm01 and cf too
 
// nb of common obs betwenn imm and cf drops to 8,643
 gen cfmissing=(imm01<.) & (cf01==.)   // if cfmissing = 1 then the value is missing
 tab race5 cfmissing, row col chi2     // rien à dire
 tab lunch_rec cfmissing, row col chi2 // pas flagrant
 tab imm01 cfmissing, row col chi2     // there more missing cf for the non-immunized (2,269 missing for non imm but 1,565 for the immunized) >> they're not randomly distributed
// it would create a bias in the estimation and thus have a lower impact of coefft of imm on the number of ab_flu days (because there are mainly immunized kids).
ttest ab_flu_per, by(cfmissing)
ttest ab_non_flu_per, by(cfmissing) // (b) ab_non_flu_per significativement plus fort lorsque cfmissing=1
 // Conclusion: Si on inclut la var cf dans l'analyse, du fait qu'elle est souvent missing, on va rejeter de l'analyse proportionnellement plus d'enfants non-immunized et qui s'absentent plus souvent en période de grippe. L'inclusion de cf entrainera un effet biasé vers le bas de imm sur ab. In the bivariate probit models, we will be using imm_rate as an instrumental varaible for imm01 (and hence, cf01 ne sera pas retenue comme IV supplémentaire) <<<<<<<
 *--------------------------------------

// Conclusion : no
 *---- Should we use cf with v?
 // We need to evaluate what kind of data we get rid of when we use v01 and cf too

 tab cf01 v01
 tab cf01
 tab v01
 // Same size, no missing values
  gen cfmissing2=(v01<.) & (cf01==.)
 tab cfmissing2 // no missing values problem, so we'll estimate the first biprobits with v01

* We mainly have a dataset with pooled cross-section data but some kids can be observed twice in 2011 and 2013
sort id year
by id: gen nb=_N
tab nb    // 29.84% are present twice but mainly kids are here once// we can deduce that all kids from all schools aren't surveyed since the proba isn't high enough.

***b. First, with fixed effects 

//Remark: we need to test for endogeneity -> we decide to choose imm_rate as an instrument.

xtset id year
xtreg ab_flu01_3 ab_non_flu_per   lunch01 i.race5 i.year imm_rate i.imm01 , fe
gen smpl=e(sample)
by id: egen nb_ds_smpl=sum(smpl) if smpl==1
tab nb_ds_smpl
xtreg ab_flu01_3 ab_non_flu_per   lunch01 i.race5 i.year imm_rate i.imm01 if nb_ds_smpl==2 , fe

***d. Then, using random effects

xtreg ab_flu01_3 ab_non_flu_per   lunch01 i.race5 i.year imm_rate i.imm01 , re
xtreg ab_flu01_3 ab_non_flu_per   lunch01 i.race5 i.year imm_rate i.imm01 if nb_ds_smpl==2 , re
// may not be a good idea for now to assess the variation with panel estimation because no biprobit xtreg exists

***3. Biprobit regression

biprobit (ab_flu01_2 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
su ab_flu01_2 if e(sample) 			// ab_flu01_2=33.8%
tabstat ab_flu_per if e(sample), by(ab_flu01_2) stat(mean N) // Compare the percentage of days missed <<<<<
margins, dydx(v01) predict(pmarg1) force

* We use imm_rate as an instrument for the endogeneous variable v01:
// and the rho test is satisfying as well: chi2(1) =  13.7609    Prob > chi2 = 0.0002

biprobit (ab_flu01_3 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
su ab_flu01_3 if e(sample)
tabstat ab_flu_per if e(sample), by(ab_flu01_3) stat(mean N)  // Compares the percentage of days missed 
margins, dydx(v01) predict(pmarg1) force

* We use imm_rate as an instrument for the endogeneous variable v01:
biprobit (ab_flu01_4 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
su ab_flu01_4 if e(sample)
tabstat ab_flu_per if e(sample), by(ab_flu01_4) stat(mean N)  // Compare the percentage of days missed <<<<<
margins, dydx(v01) predict(pmarg1) force
// the marginal effect is -.056414
// and the rho test isn't much satisfying either chi2(1) =  .241312    Prob > chi2 = 0.6233

* We use imm01 as an instrument for the endogeneous variable v01:
biprobit (ab_flu01_5 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
su ab_flu01_5 if e(sample)
tabstat ab_flu_per if e(sample), by(ab_flu01_5) stat(mean N)  // Compare the percentage of days missed <<<<<
margins, dydx(v01) predict(pmarg1) force
// and the rho test isn't much satisfying either chi2(1) =  .278503    Prob > chi2 = 0.5977

*** BASIC CODE FOR TABLES IN TEX ***
biprobit (ab_flu01_2 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex replace
biprobit (ab_flu01_2 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex append

biprobit (ab_flu01_3 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex replace
biprobit (ab_flu01_3 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex append

biprobit (ab_flu01_4 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex replace
biprobit (ab_flu01_4 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex append

biprobit (ab_flu01_5 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex replace
biprobit (ab_flu01_5 = ab_non_flu_per lunch01 i.race5 i.year i.v01 cat01) (v01= ab_non_flu_per lunch01 i.race5 i.year imm_rate cat01)
outreg2 using tableau, tex append


***4. Poisson model: Robustness Tests

etpoisson ab_flu ab_non_flu_per lunch01 i.race5 i.year cat01, treat (v01 = ab_non_flu_per lunch01 i.race5 i.year cat01 imm_rate) vce(robust)
estat summarize
margins r.v01
etpoisson ab_flu ab_non_flu_per lunch01 i.race5 i.year cat01, treat (v01 = ab_non_flu_per lunch01 i.race5 i.year cat01 imm_rate) vce(robust)
outreg2 using tableau, tex replace
etpoisson ab_flu ab_non_flu_per lunch01 i.race5 i.year cat01, treat (v01 = ab_non_flu_per lunch01 i.race5 i.year cat01 imm_rate) vce(robust)
outreg2 using tableau, tex append

