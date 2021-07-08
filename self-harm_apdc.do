
******************self-harm records extracted from APDC*******************************
use "H:\apdc.dta" 
gen date_admission=dofc(admdt)
format date_admission %td
generate self_harm=0
****primary diagnosis*********************************
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X60"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X61"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X62"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X63"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X64"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X65"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X66"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X67"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X68"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X69"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X70"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X71"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X72"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X73"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X74"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X75"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X76"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X77"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X78"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X79"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X80"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X81"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X82"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X83"
replace self_harm_P=1 if substr( diagnosis_codeP ,1,3)=="X84"

****secondary diagnosis************************************
gen self_harm_S=.
foreach x of numlist 1/49 {
replace self_harm_S=1 if substr( diagnosis_code`x' ,1,3)=="X60"| substr( diagnosis_code`x' ,1,3)=="X61" | substr( diagnosis_code`x' ,1,3)=="X62"| substr( diagnosis_code`x' ,1,3)=="X63" | substr( diagnosis_code`x' ,1,3)=="X64"| substr( diagnosis_code`x' ,1,3)=="X65" |substr( diagnosis_code`x' ,1,3)=="X66" |substr( diagnosis_code`x' ,1,3)=="X67" |substr( diagnosis_code`x' ,1,3)=="X68" |substr( diagnosis_code`x' ,1,3)=="X69" |substr( diagnosis_code`x' ,1,3)=="X70" |substr( diagnosis_code`x' ,1,3)=="X71"|substr( diagnosis_code`x' ,1,3)=="X72" |substr( diagnosis_code`x' ,1,3)=="X73" |substr( diagnosis_code`x' ,1,3)=="X74" |substr( diagnosis_code`x' ,1,3)=="X75" |substr( diagnosis_code`x' ,1,3)=="X76" |substr( diagnosis_code`x' ,1,3)=="X77" |substr( diagnosis_code`x' ,1,3)=="X78" |substr( diagnosis_code`x' ,1,3)=="X79"|substr( diagnosis_code`x' ,1,3)=="X80" |substr( diagnosis_code`x' ,1,3)=="X81" |substr( diagnosis_code`x' ,1,3)=="X81"|substr( diagnosis_code`x' ,1,3)=="X82" |substr( diagnosis_code`x' ,1,3)=="X83" |substr( diagnosis_code`x' ,1,3)=="X84"
}


*****include all self_harm records*****************
gen self_harm=.
replace self_harm=1 if self_harm_P==1 | self_harm_S==1
keep if self_harm==1


*******exlcude rehabilitation*****************
gen rehabilitation=.
rename diagnosis_codeP diagnosis_code0
foreach x of numlist 1/49 {
replace rehabilitation=1 if substr( diagnosis_code`x' ,1,3)=="Z50"
}

drop if rehabilitation==1
drop if caretype==2


save "H:\apdc_allrecords.dta" 
clear
 
 
 
 
******date of index episode************************
use "H:\apdc_allrecords.dta"

*****keep the first admission for the same participant************ 
sort ppn date_admission
duplicates drop ppn 

***check sequnce number to double-check the records are the index admission*****
codebook episode_sequence_number
drop if episode_sequence_number!=1
rename date_admission date_index_admission
keep ppn date_index_admission
gen index _admission=1

save "H:\apdc_date_fristadmission.dta" 
clear


********continous care within one episode can be identified by "episode_sequence_number"**************
use "H:\apdc_allrecords.dta" 
gen continous_care=.
replace continous_care=1 if episode_sequence_number!=1
save

******number of seperate episodes of the same participant***************
use "H:\apdc_allrecords.dta" 
keep if episode_sequence_number==1

sort date_admission
by group: generate number_episode= _n

save







 
 
 
