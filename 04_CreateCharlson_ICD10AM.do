


/*	THE HOSPITAL SEPARATION DATA	*/
clear
import delimited "4.0 Analysis\Source\apdc_2016_14_1.csv"

*	Create the diagnoses for the original ICD10 codes. 
rename diagnosis_codep diagnosis_code0 
keep ppn project_recid diagnosis_code*


*	Remove the dots (".") from the middle of the diagnosis codes.
forvalues i = 0(1)50 {
	replace diagnosis_code`i' = subinstr(diagnosis_code`i', ".", "", 1)
}


*	Create a series of variables to store the 'penalty' for the disease
forvalues i = 1(1)17 {
	generate grp`i' = 0
}

forvalues i = 0(1)50 {
	*   AMI
	replace grp1 = 1 if inlist(diagnosis_code`i',  "I21", "I252")
	
	*	CHF (note: stata only allows up to 10 values in a character list)
	replace grp2 = 1 if inlist(diagnosis_code`i', "I110", "I130", "I132", "I42" "I43", "I50", "I517") 
	
		*Peripheral Vascular disease*
	replace grp2 = 1 if inlist(diagnosis_code`i', "I70", "I71", "I72", "I731", "I738", "I739")
	
	* CEREBROVASCULAR DISEASE*
	replace grp2 = 1 if inlist(diagnosis_code`i', "G450", "G451", "G452", "G458", "G459", "H340", "I60", "I62", "I63")   ///
	 | inlist(diagnosis_code`i', "I64", "I65", "I66", "R47")
	 
	 *DEMENTIA*
	 replace grp2 = 1 if inlist(diagnosis_code`i', "F00", "F01", "F02", "F03", "G30", "G310", "G311")
	 
	 *CPD*
	 replace grp1 = 1 if inlist(diagnosis_code`i', "I260", "I270", "I278", "I279", "J41", "J42", "J43", "J44")  /// 
	  | inlist(diagnosis_code`i', "J45", "J46", "J47")
	  
	  *RHEUMATOLOGIC DISEASE*
	  replace grp1 = 1 if inlist(diagnosis_code`i', "M05", "M060", "M061", "M063", "M064", "M069", "M080", "M082")   ///
     | inlist(diagnosis_code`i', "M083", "M084", "M32", "M33", "M34", "M35")
	 
	 *PEPTIC ULCER DISEASE*
	  replace grp1 = 1 if inlist(diagnosis_code`i', "K25", "K26", "K27", "K28")
	  
	  *MILD LIVER DISEASE*
	   replace grp1 = 1 if inlist(diagnosis_code`i', "K70.2", "K703", "K721", "K730", "K731", "K738", "K739", "K74", "K760")
	  
	  *DIABETES (MILD TO MODERATE*
	  replace grp1 = 1 if inlist(diagnosis_code`i', "E100", "E101", "E1060", "E109", "E110", "E111", "E1160", "E119")  /// 
	 | inlist(diagnosis_code`i', "E120", "E121", "E1260", "E129", "E130", "E131", "E1360", "E139")   ///
	 | inlist(diagnosis_code`i', "E140", "E141", "E1460", "E149")
	 
	 * DIABETES WITH CHRONIC COMPLICATIONS*
	  replace grp1 = 1 if inlist(diagnosis_code`i', "
	  
	*	HEMIPLEGIA OR PARAPLEGIA;
	replace grp12 = 2 if inlist(diagnosis_code`i', "G041", "G114", "G8001", "G8002", "G81")  ///
		| inlist(diagnosis_code`i', "G82", "G830", "G831", "G832", "G833", "G834", "G839")

	
	* 	METASTATIC CANCER;
	replace grp16 = 6 if diagnosis_code`i' >= "C77" & diagnosis_code`i' <= "C80"
	
	* 	AIDS/HIV;	
	replace grp17 = 6 if inlist(diagnosis_code`i', "B20", "B21", "B22", "B24")	
}

generate charlson = 0
forvalues i = 1(1)17 {
	replace charlson = charlson + grp`i' 
}

charlson = SUM(of grp1-grp17)

keep PPN project_recid charlson

save "4.0 Analysis\Data\charlson.dta", replace
