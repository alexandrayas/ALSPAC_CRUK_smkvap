*** Syntax template for direct users preparing datasets using child and parent based datasets.

* Updated 25th January 2024 - updated withdrawal of consent frequencies (child c)

****************************************************************************************************************************************************************************************************************************
* This template is based on that used by the data buddy team and they include a number of variables by default.
* To ensure the file works we suggest you keep those in and just add any relevant variables that you need for your project.
* To add data other than that included by default you will need to add the relvant files and pathnames in each of the match commands below.
* There is a separate command for mothers questionnaires, mothers clinics, partner, mothers providing data on the child and data provided by the child themselves.
* Each has different withdrawal of consent issues so they must be considered separately.
* You will need to replace 'YOUR PATHNAME' in each section with your working directory pathname.

*****************************************************************************************************************************************************************************************************************************.

* G0 Mother (pregnancy) based files - include here all files related to the pregnancy and/or mother

* If no mother variables are required, KEEP this section and remove the instruction below to run it..

clear
set maxvar 32767	
use "\\path\to\alspac\filestore\Data\Current\Other\Cohort Profile\mz_6a.dta", clear
sort aln
gen in_mz=1
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\a_3e.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\b_4f.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\c_8a.dta", nogen
keep aln mz010a preg_in_alsp preg_in_core preg_enrol_status mz005l mz005m mz013 mz014 mz028b ///
a006 a525 ///
b032 b650 b663 - b667 ///
c645a c755 c765 ///
bestgest

*keep only those pregnancies enrolled in ALSPAC
keep if preg_enrol_status < 3

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before bestgest, so replace the *** line above with additional variables. 
* If none are required remember to delete the *** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that mother based WoCs are set to .a

order aln mz010a, first
order bestgest, last

do "\\path\to\alspac\filestore\Data\Syntax\Withdrawal of consent\mother_WoC.do"

* Check withdrawal of consent frequencies=30 and baseline number is 15447
tab1 mz010a, mis

save "\\path\to\ieu\project\folder\working\data\vaping_transitions\mother.dta", replace



*****************************************************************************************************************************************************************************************************************************.
* G1 Child BASED files - in this section the following file types need to be placed:
* Mother completed Qs about YP
* Obstetrics file OA

* ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need to remove the *****

use "\\path\to\alspac\filestore\Data\Current\Other\Cohort Profile\cp_3a.dta", clear
sort aln qlet
gen in_kz=1

keep aln qlet kz021 kz030 kz011b ///
in_core in_alsp in_phase2 in_phase3 in_phase4 tripquad


* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that child based WoCs are set to .b


order aln qlet kz021, first
order in_alsp tripquad, last

do "\\path\to\alspac\filestore\Data\Syntax\Withdrawal of consent\child_based_WoC.do"

* Check withdrawal of consent frequencies child based=32 (two mums of twins have withdrawn consent)
tab1 kz011b, mis

drop kz021
save "\\path\to\ieu\project\folder\working\data\vaping_transitions\childB.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* G1 Child COMPLETED files - in this section the following file types need to be placed:
* YP completed Qs
* Puberty Qs
* Child clinic data
* Child biosamples data
* School Qs
* Obstetrics file OC
* G1 IMD for years goes in this section e.g. jan1999imd2010_crimeq5_YP
* Child longitudinal data

* NOTE: Always keep this section even if no child completed variables requested. Remove the ***** line.
* NOTE: Keep kz021 tripquad just to make the withdrawal of consent work - these are dropped for this file as the ones in the child BASED file are the important ones and should take priority

use "\\path\to\alspac\filestore\Data\Current\Other\Cohort Profile\cp_3a.dta", clear
sort aln qlet
gen in_kz=1
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPA_r1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPB_r1e.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPC_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPD_1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPH_3a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPK_1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\F24_6a.dta", nogen

keep aln qlet kz021 kz030 ///
YPA9020 YPA6000 YPA6001 YPA6002 YPA6010 YPA6011 YPA6020 YPA6030 YPA0002 YPA9010 YPA9011 YPA9021 ///
YPB9992 YPB4000 YPB4001 YPB4010 YPB4011 YPB4090 YPB4100 YPB4110 YPB4120 YPB0002 YPB9990 YPB9991 YPB9993 ///
YPC2650 YPC1350 YPC1351 YPC1360 YPC1361 YPC1390 YPC1400 YPC1401 YPC1402 YPC1403 YPC1404 YPC1410 YPC1420 YPC1430 YPC1440 YPC1450 YPC1460 YPC1470 YPC1480 YPC1490 YPC0002 YPC2600 YPC2601 YPC2750 YPC0003 ///
YPD9650 YPD7000 YPD7001 YPD7010 YPD7011 YPD7040 YPD7050 YPD7051 YPD7052 YPD7053 YPD7054 YPD7060 YPD7070 YPD7080 YPD7090 YPD7100 YPD7110 YPD7120 YPD7130 YPD7140 YPD0002 YPD9600 YPD9601 YPD9750 YPD0003 ///
FKAR0010 FKSM1010 FKSM1020 FKSM1030 FKSM1040 FKSM1050 FKSM1200 FKSM1210 FKSM1220 FKSM1230 FKAR0040 FKAR0041 ///
YPH9520 YPH5510 YPH5610 YPH5620 YPH5630 YPH5640 YPH0002 YPH9021 YPH9022 YPH9910 YPH9920 ///
YPK9510 YPK6010 YPK6020 YPK6030 YPK6040 YPK6050 YPK6060 YPK6070 YPK6090 YPK6100 YPK6110 YPK6120 YPK6130 YPK6140 YPK6150 YPK6160 YPK6170 YPK6180 YPK6190 YPK6280 YPK6510 YPK6520 YPK6530 YPK6540 YPK6550 YPK6560 YPK6570 YPK6580 YPK0002 YPK9021 YPK9022 YPK9910 YPK9920 ///
tripquad

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before tripquad, so replace the ***** line with additional variables.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file.  Note that mother based WoCs are set to .b

order aln qlet kz021, first
order tripquad, last

do "\\path\to\alspac\filestore\Data\Syntax\Withdrawal of consent\child_completed_WoC.do"

* Check withdrawal of consent frequencies child completed=31
tab1 kz021, mis

drop tripquad
save "\\path\to\ieu\project\folder\working\data\vaping_transitions\childC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
** Matching all data together and saving out the final file*.
* NOTE: any linkage data should be added here*.

use "\\path\to\ieu\project\folder\working\data\vaping_transitions\childB.dta", clear
merge 1:1 aln qlet using "\\path\to\ieu\project\folder\working\data\vaping_transitions\childC.dta", nogen
merge m:1 aln using "\\path\to\ieu\project\folder\working\data\vaping_transitions\mother.dta", nogen

* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1

drop in_alsp tripquad
save "\\path\to\ieu\project\folder\working\data\vaping_transitions\vaping_vars_ALSPAC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* QC checks*
use "\\path\to\ieu\project\folder\working\data\vaping_transitions\vaping_vars_ALSPAC.dta", clear

* Check that there are 15645 records.
count
