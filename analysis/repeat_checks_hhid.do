/*==============================================================================
DO FILE NAME:			repeat_checks_hhid.do
PROJECT:				Household Short Dat Report 
DATE: 					16 July 2021
AUTHOR:					A Schultze 
								
DESCRIPTION OF FILE:	series of basic data cleaning and sensitivity checks 
DATASETS USED:			output/input.csv
DATASETS CREATED: 		none 
OTHER OUTPUT: 			logfile, printed to folder output/statalogs 
							
==============================================================================*/

/* HOUSEKEEPING===============================================================*/

* create folders that do not exist on server 
capture	mkdir "`c(pwd)'/output/statalogs"

* open a log file
cap log close
log using "`c(pwd)'/output/statalogs/repeat_checks_hhid.log", replace 

* IMPORT DATA=================================================================*/ 
import delimited `c(pwd)'/output/input.csv, clear

* RAW DATA DESCRIPTION========================================================*/ 

desc

summarize household_id, d 
summarize household_size, d
summarize percent_tpp, d
summarize age, d

tab mixed_household, m
tab rural_urban, m  
tab care_home_type, m
tab imd, m 

* DATA CLEANING===============================================================*/ 
* minimal data cleaning as want to capture potential discrepancies 

* IMD 
replace imd = . if imd <= 0 

/* SENSE CHECKS=================================================================
Four main aspects to quality check: 
1) Completeness of variables 

2) Household size 

3) Consistency of household level characteristics across a HHID

4) Special considerations when household is care home 

==============================================================================*/ 

* 1 COMPLETENESS 

* Count and drop those with invalid HHID <= 0 
count
drop if household_id <= 0 
count

* After dropping those with invalid HHIDs, expect low missingness/issues in other household level variables 

* Household Size 
summarize household_size, d
gen invalid_household_size =1 if household_size <=0 
tab invalid_household_size, m 

* IMD 
tab imd, m 

* Rural Urban
tab rural_urban, m 

* Care Home Status 
tab care_home_type, m 

* Percent TPP 
summarize percent_tpp, d 
gen invalid_percent_tpp=1 if percent_tpp <= 0 
tab invalid_percent_tpp, m 

* Individuals may have 0 TPP percent validly because of rounding errors in calculation 

* Mixed household 
tab mixed_household, m 

* 2 HOUSEHOLD SIZE 
* Describing this just overall, but note means etc will be higher because more people will live in a large household 
* This is just to get a sense for the variables 

* Recalculate household size 
bysort household_id: egen size_check = count(patient_id)

* Cross check household size 
summarize household_size 
summarize size_check 

gen discrepant_size = (household_size != size_check)

tab discrepant_size, m

* household size keeping one row per household 
preserve 
duplicates drop household_id, force 

* Overall 
summarize household_size , d
summarize size_check, d 

restore 

* 3 CONSISTENCY 
* Are variables such as size consistent across a household? 
* code from https://www.stata.com/support/faqs/data-management/listing-observations-in-group/ 

* Size 
sort household_id household_size 
by household_id (household_size), sort: gen not_consistent_hhsize = household_size[1] != household_size[_N]

tab not_consistent_hhsize, m 

** if discrepant, how discrepant 
sort household_id household_size 
by household_id (household_size), sort: gen discrepancy_hhsize = abs(household_size[1] - household_size[_N]) 

summarize discrepancy_hhsize 

* Care Home Status 
sort household_id care_home_type 
by household_id (care_home_type), sort: gen not_consistent_chstatus = care_home_type[1] != care_home_type[_N]

tab not_consistent_chstatus, m 

* 4 CARE HOMES 

* Household size in care homes (multiple patients)
bysort care_home_type: summarize household_size, d 

* household size keeping one row per household 
preserve 
duplicates drop household_id, force 

* By care home type 
bysort care_home_type: summarize household_size, d 

restore 

* Care home residents in household with >3 people > 65 (replicating short data report)

tab care_home_type, m 
gen old = (age >= 65) 

bysort household_id: egen care_home_check = total(old)

gen household_care_home = (care_home_check >= 3)

* overlap with care home linkage 

gen care_home = 1 if care_home_type == "PC" 
replace care_home = 1 if care_home_type == "PN"
replace care_home = 1 if care_home_type == "PS"
replace care_home = 0 if care_home == . 

tab care_home 
tab care_home care_home_type 

tab care_home household_care_home 

* Close log 
log close 






