*THIS IS THE STATA DO-FILE THAT GENERATES THE RESULTS IN THE PAPER "THE USE OF VIOLENCE IN ILLEGAL MARKETS: EVIDENCE FROM MAHOGANY TRADE IN THE BRAZILIAN AMAZON," BY CHIMELI AND SOARES*
*COMMANDS TO BE USED WITH THE FILE "AEJApp-2016-0055-Stata Dataset.dta"*
*
* SET UP
*
clear all
set maxvar 10000
set matsize 10000
*
use "X:\YOUR FILE LOCATION\AEJApp-2016-0055-Stata Dataset.dta", clear
*
*
*STARTING SAMPLE IS COMPOSED OF MUNICIPALITIES IN STATES WITH AT LEAST SOME MAHOGANY OCCURRENCE; THESE CORRESPOND TO UF'S 11, 12, 13, 15, 17, 21, 51
*
*THE VARIABLE INDICATING THAT A MUNICIPALITY IS WITHIN THE AREA OF NATURAL OCCURRENCE OF MAHOGANY IS "mahog_area"
*
*SAMPLE SELECTION FOR MAIN RESULTS: MUNICIPALITIES THAT EXISTED IN 1995
*
replace pop = 0 if pop==.
gen temp = 1 if pop>0 & year==1995
	by code, sort: egen muni_exist_1995 = mean(temp)
	replace muni_exist_1995 = 0 if muni_exist_1995==.
	drop temp
keep if muni_exist_1995==1
*
*
*CREATING CONTROLS & AUX VARS
*
replace mahogx_state=0 if  mahogx_state==.
replace otherx_state=0 if otherx_state==.
gen total_mahogx = ((otherx_state + mahogx_state)/1000000)
gen gdp_pc = (gdp/pop)
gen ln_gdp_pc = ln(gdp_pc)
gen gdpfr_ag = (gdp_ag/gdp)
gen area_plant = (plant/(area*100))
gen ln_otherx_state = ln(otherx_state)
gen trend = year - 1995
*
*GENERATING VARIABLES RELATED TO MORTALITY AND HOMICIDES
*-MORTALITY VARS APPEAR AS MISSING IN THE DATASUS DATASET WHEN THEY ARE ZERO, SO WE REPLACE ALL MISSING IN MORTALITY VARS BY ZERO
*-NON-EXISTING MUNICIPALITIES HAVE ZERO POPULATION; THESE WILL GENERATE MISSING VARIABLES FOR MORTALITY RATES
*-DISAGGREGATE MORTALITY BY TYPE OF HOMICIDE ONLY EXISTS STARTING IN 1996
*-OTHER MORTALITY DATA IN THIS DATASET COVERS ONLY THE PERIOD UNTIL 1997; NOT A PROBLEM, SINCE THESE WILL BE USED ONLY TO CREATE INITIAL VALUES TO BE INTERACTED WITH TIME DUMMIES
*
replace hom = 0 if hom==.
replace hom_male = 0 if hom_male==. & year>1995
replace hom_m_prime = 0 if hom_m_prime==. & year>1995
replace hom_m_sing = 0 if hom_m_sing==. & year>1995
replace hom_m_nothome = 0 if hom_m_nothome==. & year>1995
replace hom_m_firearm = 0 if hom_m_firearm==. & year>1995
replace heart = 0 if heart==. & year<2008
replace infecc = 0 if infecc==. & year<2008
replace neop = 0 if neop==. & year<2008
replace und5_mort = 0 if und5_mort==. & year<2008
replace traff = 0 if traff==. & year<2008               
replace suic = 0 if suic==. & year<2008
replace pol_deaths = 0 if pol_deaths==. & year<2008
*
gen hom_tx = (hom/pop)*100000
gen male_h = (hom_male/pop_male)*100000
gen male_h_prime = (hom_m_prime/pop_m_prime)*100000
gen male_h_nothome = (hom_m_nothome/pop_m_prime)*100000
gen male_h_sing = (hom_m_sing/pop_m_prime)*100000
gen male_h_firearm = (hom_m_firearm/pop_m_prime)*100000
gen und5_m = (und5_mort/pop_und5)*1000
gen heart_m = (heart/pop)*1000
gen infecc_m = (infecc/pop)*1000
gen neop_m = (neop/pop)*1000
gen suic_m = (suic/pop)*1000
gen traff_m = (traff/pop)*1000
gen pol_m = (pol_deaths/pop)*1000
gen pol_deaths_dummy = 1 if pol_deaths>0 & year<2008
	replace pol_deaths_dummy = 0 if pol_deaths_dummy==. & year<2008
*
*CREATING VARIABLES FIXED AT BASELINE VALUES (OR AVERAGE) TO BE USED INTERACTED WITH TIME DUMMIES AND TO BE USED AS WEIGHT (POP)
*
gen temp = hom_tx if year==1995
	by code: egen hom_base = mean(temp)
	drop temp
gen temp = area_plant if year==1995
	by code: egen area_base = mean(temp)
	drop temp
gen temp = ln_gdp_pc if year==1996
	by code: egen lngdp_base = mean(temp)
	drop temp
gen temp = gdpfr_ag if year==1996
	by code: egen gdpag_base = mean(temp)
	drop temp
gen temp = pol_m if year==1995
	by code: egen pol_base = mean(temp)
	drop temp
gen temp = und5_m if year==1995
	by code: egen und5_base = mean(temp)
	drop temp
gen temp = infecc_m if year==1995
	by code: egen infecc_base = mean(temp)
	drop temp
gen temp = heart_m if year==1995
	by code: egen heart_base = mean(temp)
	drop temp
gen temp = neop_m if year==1995
	by code: egen neop_base = mean(temp)
	drop temp
gen temp = suic_m if year==1995
	by code: egen suic_base = mean(temp)
	drop temp
gen temp = traff_m if year==1995
	by code: egen traff_base = mean(temp)
	drop temp
gen temp = pop if year==1995
	by code: egen pop1995 = mean(temp)
	drop temp
by code: egen avg_pop = mean(pop)
*
*TREATMENTS: CHANGE IN LAWS IN MARCH 1999 AND OCTOBER 2001, CHANGE IN ENFORCEMENT IN 2009
*
sort code year
gen treat1=1 if  mahog_area==1 & year>=1999 & year<=2001
	replace treat1=0 if treat1==.
gen treat2=1 if  mahog_area==1 & year>=2002 & year<=2008
	replace treat2=0 if treat2==.
gen treat3=1 if  mahog_area==1 & year>=2009
	replace treat3=0 if treat3==.
gen treatx_area1=mahog_exp_pre if mahog_area==1 & year>=1999 & year<=2001
	replace treatx_area1=0 if treatx_area1==.
gen treatx_area2=mahog_exp_pre if mahog_area==1 & year>=2002 & year<=2008
	replace treatx_area2=0 if treatx_area2==.
gen treatx_area3=mahog_exp_pre if mahog_area==1 & year>=2009
	replace treatx_area3=0 if treatx_area3==.
gen treatt_area1= total_mahogx if mahog_area==1 & year>=1999 & year<=2001
	replace treatt_area1=0 if treatt_area1==.
gen treatt_area2= total_mahogx if  mahog_area==1 & year>=2002 & year<=2008
	replace treatt_area2=0 if treatt_area2==.
gen treatt_area3= total_mahogx if  mahog_area==1 & year>=2009
	replace treatt_area3=0 if treatt_area3==.
gen trend1 = year - 1999 if year >1998 & year<2002
	replace trend1=0 if trend1==.
gen int1 = treat1*trend1
gen trend2 = year - 2002 if year >2001 & year<2009
	replace trend2=0 if trend2==.
gen int2 = treat2*trend2
gen trend3 = year - 2009 if year >2008
	replace trend3=0 if trend3==.
gen int3 = treat3*trend3
*
*CREATING THE PLACEBO
*
gen pre = 1 if mahog_area==1 & year>1996 & year<1999
	replace pre = 0 if pre==.
*
*GLOBAL COMMANDS DEFINING GROUPS OF VARS AND OUTFILE LOCATIONS
*
global treat "treat1 treat2 treat3"
global treat_trend "treat1 int1 treat2 int2 treat3 int3"
global treatx_area "treatx_area1 treatx_area2 treatx_area3"
global treatt_area "treatt_area1 treatt_area2 treatt_area3"
global int_trend "i.year*hom_base i.year*area_base i.year*lngdp_base i.year*gdpag_base i.year*pol_base i.year*und5_base i.year*infecc_base i.year*heart_base i.year*neop_base i.year*suic_base i.year*traff_base"
global w "avg_pop"
global tables_benchmark ""X:\YOUR FILE LOCATION\table_2.xml""
global tables_states ""X:\YOUR FILE LOCATION\table_3.xml""
global tables_placebo ""X:\YOUR FILE LOCATION\table_4.xml""
global tables_demog ""X:\YOUR FILE LOCATION\table_5.xml""
global tables_app_weight&spatial ""X:\YOUR FILE LOCATION\table_A.4.1.xml""
*
tsset code year
*
* TABLE 2: BENCHMARK SPECIFICATION
*
xi: areg hom_tx $treat i.year [aweight = $w], absorb(code) cluster(code)
	outreg2 $treat using $tables_benchmark, replace drop(_* o.*) ct(mahog_states) bracket excel
xi: areg hom_tx $treat i.year i.uf*i.year [aweight = $w], absorb(code) cluster(code)
	outreg2 $treat using $tables_benchmark, append drop(_* o.*) ct(with_state_t) bracket excel
xi: areg hom_tx $treat i.year i.uf*i.year $int_trend [aweight = $w], absorb(code) cluster(code)
	outreg2 $treat using $tables_benchmark, append drop(_* o.*) ct(with_interac_t) bracket excel
xi: areg hom_tx $treat_trend i.uf*i.year i.year [aweight = $w], absorb(code) cluster(code)
	outreg2 $treat_trend using $tables_benchmark, append drop(_* o.*) ct(treat_trend) bracket excel
xi: areg hom_tx $treatx_area i.year i.uf*i.year [aweight = $w], absorb(code) cluster(code)
	outreg2 $treatx_area using $tables_benchmark, append drop(_* o.*) ct(treatx_area) bracket excel
xi: areg hom_tx $treatt_area i.year i.uf*i.year [aweight = $w], absorb(code) cluster(code)
	outreg2 $treatt_area using $tables_benchmark, append drop(_* o.*) ct(treatt_area) bracket excel
*
* TABLE 3: PARÁ AND OTHER MAHOGANY OCCURRING STATES SEPARATELY	
*	
xi: areg hom_tx $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_states, replace drop(_* o.*) ct(PA) bracket excel
xi: areg hom_tx $treat i.year $int_trend [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_states, append drop(_* o.*) ct(PA_with_interac_t) bracket excel
xi: areg hom_tx $treat_trend i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat_trend using $tables_states, append drop(_* o.*) ct(PA_treat_trend) bracket excel
xi: areg hom_tx $treat i.year [aweight = $w] if uf~=15, absorb(code) cluster(code)
	outreg2 $treat using $tables_states, append drop(_* o.*) ct(mahog_states) bracket excel
xi: areg hom_tx $treat i.year i.uf*i.year [aweight = $w] if uf~=15, absorb(code) cluster(code)
	outreg2 $treat using $tables_states, append drop(_* o.*) ct(with_state_t) bracket excel
xi: areg hom_tx $treat i.year i.uf*i.year $int_trend [aweight = $w] if uf~=15, absorb(code) cluster(code)
	outreg2 $treat using $tables_states, append drop(_* o.*) ct(with_interac_t) bracket excel
xi: areg hom_tx $treat_trend i.uf*i.year i.year [aweight = $w] if uf~=15, absorb(code) cluster(code)
	outreg2 $treat_trend using $tables_states, append drop(_* o.*) ct(treat_trend) bracket excel
*
* TABLE 4: PLACEBO, MUNICIPALITY SPECIFIC LINEAR TRENDS, OTHER CONCURRENT CHANGES
*
xi: areg hom_tx $treat pre i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat pre using $tables_placebo, replace drop(_* o.*) ct(PA) bracket excel
xi: areg hom_tx $treat i.code*trend i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(PA_with_muni_t) bracket excel
xi: areg hom_tx $treat pre i.year [aweight = $w] if uf==15 & ln_gdp_pc~=., absorb(code) cluster(code)
	outreg2 $treat pre using $tables_placebo, append drop(_* o.*) ct(PA_1996_>1998) bracket excel
xi: areg hom_tx $treat i.code*trend i.year [aweight = $w] if uf==15 & ln_gdp_pc~=., absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(PA_with_muni_t_1996_>1998) bracket excel
xi: areg ln_gdp_pc $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(gdp dep) bracket excel
xi: areg ln_gdp_pc $treat i.code*trend i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(gdp muni_t) bracket excel
xi: areg gdpfr_ag $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(gdpag dep) bracket excel
xi: areg gdpfr_ag $treat i.code*trend i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_placebo, append drop(_* o.*) ct(gdpag muni_t) bracket excel
*
* TABLE 5: DEMOGRAPHIC CHARACTERIZATION OF HOMICIDES
*
xi: areg hom_tx $treat i.year [aweight = $w] if year>1995 & uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, replace drop(_* o.*) ct(all) bracket excel
xi: areg male_h $treat i.year [aweight = $w]  if year>1995 & uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(mmale) bracket excel
xi: areg male_h_prime $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(mprime) bracket excel
xi: areg male_h_sing $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(msingle) bracket excel
xi: areg male_h_nothome $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(nothome) bracket excel
xi: areg male_h_firearm $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(firearm) bracket excel
xi: areg pol_m $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(political) bracket excel
xi: areg pol_deaths_dummy $treat i.year [aweight = $w] if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_demog, append drop(_* o.*) ct(polit_dummy) bracket excel
*	
* APPENDIX TABLE A.4.1: UNWEIGHTED AND SPATIAL CORRELATION
*
xi: areg hom_tx $treat i.year if uf==15, absorb(code) cluster(code)
	outreg2 $treat using $tables_app_weight&spatial, replace drop(_* o.*) ct(PA_unweighted) bracket excel
xi: xtscc hom_tx $treat i.year i.code [aweight = $w] if uf==15, pooled
	outreg2 $treat using $tables_app_weight&spatial, append drop(_* o.*) ct(PA_spatial) bracket excel
*
clear
*

