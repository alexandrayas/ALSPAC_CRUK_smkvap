*** Syntax template for direct users preparing datasets using child and parent based datasets.

* Created 29th October 2014 - always create a datafile using the most up to date template.
* Updated 24th May 2018 - mothers questionnaire and clinic data now dealt with separately in order to take into account separate withdrawal of consent requests.
* Updated 1st October 2018 - adding partners withdrawal of control
* Updated 12th October 2018 - cohort profile dataset has been updated and so version number updated to reflect
* Updated 9th November 2018 - ends of file paths for A, B and C files
* Updated 13th February 2019 - added checks in each section for correct withdrawal of consent frequencies
* Updated 21st February 2019 - updated withdrawal of consent frequencies
* Updated 5th March 2019 - updated withdrawal of consent frequencies
* Updated 11th March 2019 - updated withdrawal of consent frequencies
* Updated 9th May 2019 - updated withdrawal of consent frequencies
* Updated 17th March 2019 - updated withdrawal of consent frequencies
* Updated 9th August 2019 - updated withdrawal of consent frequencies
* Updated 4th Sept 2019 - updated withdrawal of consent frequencies
* Updated 24th March 2020 - updated withdrawal of consent frequencies
* Updated 5th August 2020 - updated withdrawal of consent frequencies
* Updated 9th September 2020 - updated withdrawal of consent frequencies
* Updated 25th May 2021 - updated withdrawal of consent frequencies
* Updated 27th May 2021 - updated withdrawal of consent frequencies
* Updated 3rd June 2021 - added clarification of where to inlcude variable lists
* Updated 6th Sept 2021 - updated withdrawal of consent frequencies
* Updated 21st Sept 2021 - updated withdrawal of consent frequencies (child-complete)
* Updated 2nd February 2022- information added re WOCs for longitudinal datasets
* Updated 16th February 2022 - updated withdrawal of consent frequencies (partner, child-complete)
* Updated 25th February 2022 - updated withdrawal of consent frequencies (mother q, mother c, child b), ethnicity variables removed from template to be add if needed and covered by proposal
* Updated 10th March 2022 - updated withdrawal of consent frequencies (partner)
* Updated 1st April 2022 - updated withdrawal of consent frequencies (mother q, mother c, child b)
* Updated 28th April 2022 - updated withdrawal of consent frequencies (mother q, mother c, child b)
* Updated 18th August 2022 - updated following combination of CP and KZ files
* Updated 8th September 2022 - updated withdrawal of consent frequencies (mother q, mother c, child b)
* Updated 16th September 2022 - updated withdrawal of consent frequencies (mother q, mother c, child b)
* Updated 19th October 2022 - updated withdrawal of consent frequencies (child-complete)

****************************************************************************************************************************************************************************************************************************
* This template is based on that used by the data buddy team and they include a number of variables by default.
* To ensure the file works we suggest you keep those in and just add any relevant variables that you need for your project.
* To add data other than that included by default you will need to add the relvant files and pathnames in each of the match commands below.
* There is a separate command for mothers questionnaires, mothers clinics, partner, mothers providing data on the child and data provided by the child themselves.
* Each has different withdrawal of consent issues so they must be considered separately.
* You will need to replace 'YOUR PATHNAME' in each section with your working directory pathname.

*****************************************************************************************************************************************************************************************************************************.

* Mother questionnaire files - in this section the following file types need to be placed:
* Mother completed Qs about herself
* Maternal grandparents social class
* Partner_proxy social class
* G0 IMD for years goes in this section e.g. jan1992imd2000q5_M.
* Mother longitudinal data

* ALWAYS KEEP THIS SECTION IF YOU ARE USING MOTHER-BASED DATA EVEN IF ONLY MOTHER CLINIC REQUESTED

clear
set maxvar 32767	
use "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Other\Sample Definition\mz_5a.dta", clear
sort aln
gen in_mz=1
merge 1:1 aln using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Mother\a_3e.dta", nogen
merge 1:1 aln using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Mother\b_4f.dta", nogen
merge 1:1 aln using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Mother\c_8a.dta", nogen
merge 1:1 aln using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Useful_data\bestgest\bestgest.dta", nogen

keep aln mz001 mz010a mz013 mz014 mz028b ///
a006 a525 ///
b032 b650 b663 - b667 ///
c645a c755 c765 ///
bestgest

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before bestgest, so replace the *** line above with additional variables. 
* If none are required remember to delete the *** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that mother based WoCs are set to .a


order aln mz010a, first
order bestgest, last

do "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Syntax\Withdrawal of consent\mother_quest_WoC.do"

* Check withdrawal of consent frequencies mum quest=26
tab1 mz010a, mis

save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\motherQ.dta", replace



*****************************************************************************************************************************************************************************************************************************.
* Child BASED files - in this section the following file types need to be placed:
* Mother completed Qs about YP
* Obstetrics file OA

* ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need to remove the *****

use "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Other\Sample Definition\cp_3a.dta", clear
sort aln qlet
gen in_kz=1
/* remove this line and end of commenting on the next line to add in any other file names if relevant files are needed in this section
merge 1:1 aln qlet using "", nogen   */


keep aln qlet kz011b kz021 kz030 ///
in_core in_alsp in_phase2 in_phase3 in_phase4 tripquad


* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that child based WoCs are set to .b


order aln qlet kz021, first
order in_alsp tripquad, last

do "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Syntax\Withdrawal of consent\child_based_WoC.do"

* Check withdrawal of consent frequencies child based=28 (two mums of twins have withdrawn consent)
tab1 kz021, mis

save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\childB.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* Child COMPLETED files - in this section the following file types need to be placed:
* YP completed Qs
* Puberty Qs
* Child clinic data
* Child biosamples data
* School Qs
* Obstetrics file OC
* G1 IMD for years goes in this section e.g. jan1999imd2010_crimeq5_YP
* Child longitudinal data

* If there are no child completed files, this section can be starred out.
* NOTE: having to keep kz021 tripquad just to make the withdrawal of consent work - these are dropped for this file as the ones in the child BASED file are the important ones and should take priority

use "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Other\Sample Definition\cp_3a.dta", clear
sort aln qlet
gen in_kz=1
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Other\Cohort Profile\cp_2b.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccr_r1b.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccs_r1b.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\cct_1c.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccu_2b.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\YPA_r1a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\YPB_r1e.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\YPC_2a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\YPD_1a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Quest\Child Completed\YPH_1a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Clinic\Child\tf1_3b.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Clinic\Child\tf2_5a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Clinic\Child\tf3_4c.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Clinic\Child\tf4_6a.dta", nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Current\Clinic\Child\F24_6a.dta", nogen

keep aln qlet kz021 ///
ff6610 ff6614 ff6615 ff6617 ff6618 ///
fg4822 fg4824 fg4827 ///
ccr700 ccr705 ccr710 ccr730 ccr735 ccr740 ///
fh8410 fh8430 fh8411 fh8432 fh8441 fh8450 fh8451 fh8455 fh8456 ///
ccs4000 ccs4005 ccs4010 ccs4030 ccs4040 ///
FJSM050 FJSM100 FJSM150 FJSM250 FJSM300 FJSM350 FJSM400 FJSM450 FJSM500 ///
cct5000 cct5001 cct5005 cct5010 cct5011 cct5012 cct5013 cct5014 cct5015 ///
CCU3000 CCU3001 CCU3005 CCU3010 CCU3011 CCU3012 CCU3013 CCU3014 CCU3015 ///
YPA6000 YPA6001 YPA6002 YPA6010 YPA6011 YPA6020 YPA6021 YPA6030 YPA6031 ///
YPB4000 YPB4001 YPB4010 YPB4011 YPB4020 YPB4024 YPB4030 YPB4031 ///
YPC1350 YPC1351 YPC1360 YPC1361 YPC1370 YPC1371 YPC1380 YPC1381 ///
YPD7000 YPD7001 YPD7010 YPD7011 YPD7020 YPD7021 YPD7030 YPD7031 ///
FKSM1010 FKSM1020 FKSM1030 FKSM1040 FKSM1050 FKSM1060 FKSM1070 FKSM1080 FKSM1090 ///
YPH5510 YPH5520 YPH5530 YPH5540 YPH5550 ///
tripquad

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before tripquad, so replace the ***** line with additional variables.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file.  Note that mother based WoCs are set to .b

order aln qlet kz021, first
order tripquad, last

do "\\ads.bris.ac.uk\Filestore\SSCM ALSPAC\Data\Syntax\Withdrawal of consent\child_completed_WoC.do"

* Check withdrawal of consent frequencies child completed=29 
tab1 kz021, mis

drop kz021 tripquad
save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\childC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
** Matching all data together and saving out the final file*.
* NOTE: any linkage data should be added here*.

use "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\childB.dta", clear
merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\childC.dta", nogen
merge m:1 aln using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\motherQ.dta", nogen


* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1

drop in_alsp tripquad
save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\smoking_vars_ALSPAC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* QC checks*
use "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p7\025\working\data\smoking_vars_ALSPAC.dta", clear

* Check that there are 15645 records.
count
