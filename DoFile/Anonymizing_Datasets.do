********* Anonymize the datasets for publishing ****
****** Remove the idenfitying information variables

** set the global working directory***

global Data = "C:\Users\Cyrus Wakibia\OneDrive - Zizi Afrique Foundation\Documents\wakibia-pc\strathmore\Analysis\Data"


**# Volunteer data
use "$Data\my_village_2023_volunteers_originial.dta", clear

drop vol_name


save "$Data\my_village_2023_volunteers.dta", replace


**# Remediation Camps Data

use "$Data\mv-camp-English-2025-wide-data_originial.dta", clear

drop child_name


save "$Data\mv-camp-English-2025-wide-data.dta", replace


**# Household Data

use "$Data\my-village-English-baselines-data_originial.dta", clear


** drop non-data variables

drop duration_minutes duration_flag

** drop identifying variables

drop hh_headname mothers_name fathers_name ///
	guardian_name childschool_name child_name hh_respondent
	
	
	
save "$Data\my-village-English-baselines-data.dta", replace
	