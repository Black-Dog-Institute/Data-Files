/* 	Project: 	Dual diagnosis and injury paper
	Aim: 		Generate estimate of dual diagnosis among people with hospital contact
	Date: 		5 May 2018
	Analyst:	Lisa Sharwood (from J.Young code)	*/
	
/*	Analysis Notes:
		-----
		The following syntax will ascertain exclusive categories for:
		1) no mental disorder;
		2) substance use disoder only;
		3) mental illness only; and
		4) dual diagnosis
		from all diagnoses in hospital records and principal 
		diagnoses in ED records.
		-----

*/
		

/*	The following code is for hospital records
	It assumes:
	a) there is ICD-10 codes only
	b) there is principal and secondary ICD-10 codes
	c) the dataset is in a long file format (panel data) */
	
/*	Rename all variables to lower case	*/
rename *, lower

/*	Sort by personid and admission date/time	*/
sort ppn admitdttm


/*	Generate a variable with only external cause of morbidity codes	*/
gen ecause = icd_code if icd_type == "External_cause"
la var ecause "ICD external cause"
tab ecause

/* 	iterate and clean ICD-10-AM codes to make compatible with stata icd10 command */
icd10 check diag, year(2010)
icd10 gen diag2 = diag, sh
icd10 check diag2, year(2010) g(prob)
list diag2 if prob == 99
icd10 gen diagcat = diag2, cat
icd10 check diagcat, year(2010) g(prob2)
tab diagcat if prob2 == 99
drop prob prob2

/*	identify diagnoses of mental illness	*/
icd10 gen mhdx = diagcat, r(F0/F09 F20/F99)

/*	identify diagnoses of substance use disorder	*/
icd10 gen suddx = diagcat, r(F10/F19)

/*	create panel wide indicator variables	*/
foreach x of var mhdx suddx {
bys ppn admitdttm: egen `x'xt = max(`x')
}

/*	identify a history of self-harm	*/
/* 	iterate and clean ICD-10-AM codes to make compatible with stata icd10 command */
icd10 check ecause, year(2010) g(prob)
icd10 gen ecause2 = ecause, sh
icd10 check ecause2, year(2010) g(prob2)
tab ecause2 if prob2 == 99
icd10 gen ecause_cat = ecause2, cat
icd10 check ecause_cat, year(2010) g(prob3)
tab ecause_cat if prob3 == 99
drop prob*

/*	identify hospital admissions due to self-harm	*/
icd10 gen selfharm = ecause_cat, r(X60/X84)
recode selfharm (. = 0)
xttab selfharm

/*	generate panel wide indicator variable	*/
bysort ppn admitdttm: egen selfharmxt = max(selfharm)
xttab selfharmxt

/*	The following code is for ED records
	It assumes:
	a) there is ICD-10 codes only
	b) there is principal diagnosis ICD-10 code only
	c) the dataset is in a long file format (panel data) */
	
/*	Load dataset with use command	*/	

/*	Rename all variables to lower case	*/
rename *, lower

/*	Sort by personid and admission date	*/
sort ppn admitdttm

/*	Generate a variable with only principal diagnosis ICD-10 codes	*/
gen diag = e_diag_icd_code_prim

/* 	iterate and clean ICD-10-AM codes to make compatible with stata icd10 command */
icd10 check diag, year(2010) g(prob)
replace diag = "" if prob == 1
icd10 check diag, year(2010)
icd10 gen diag2 = diag, sh
icd10 check diag2, year(2010) g(prob2)
tab diag2 if prob == 99
icd10 gen diagcat = diag2, cat
icd10 check diagcat, year(2010) g(prob3)
tab diagcat if prob3 == 99
drop prob*

/*	identify diagnoses of mental illness	*/
icd10 gen ed_mhdx = diagcat, r(F0/F09 F20/F99)

/*	identify diagnoses of substance use disorder	*/
icd10 gen ed_suddx = diagcat, r(F10/F19)

/*	create panel wide indicator variables	*/
foreach x of var ed_mhdx ed_suddx {
bys ppn admitdttm: egen `x'xt = max(`x')
}


/* 	collapse to wide file and merge ed indicators on to the hospital dataset master	*/


/*	create an aggregate dual diagnosis indicator variable with mutually 
	exclusive categoeries for:
		0) no mental disorder
		1) substance use disorder only
		2) mental illness only
		3) dual diagnosis only	*/
		
		
gen dualdxxt = 0 if mhdxxt == 0 & ed_mhdxxt == 0 & suddxxt == 0 & ed_suddxxt == 0
replace dualdxxt = 1 if mhdxxt == 0 & ed_mhdxxt == 0 & (suddxxt == 1 | ed_suddxxt == 1)
replace dualdxxt = 2 if (mhdxxt == 1 | ed_mhdxxt == 1) & suddxxt == 0 & ed_suddxxt == 0
replace dualdxxt = 3 if (mhdxxt == 1 | ed_mhdxxt == 1) & (suddxxt == 1 | ed_suddxxt == 1)
la var dualdxxt "dual diagnosis indicator"
la de dualdxxt 0 "no mental disorder" 1 "sud only" 2 "mi only" 3 "dual diagnosis"
la val dualdxxt dualdxxt
