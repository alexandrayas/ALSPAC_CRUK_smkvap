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
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\d_4b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\e_4f.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\f_2b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\g_5c.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\h_6d.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\j_5b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\k_r1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\l_r1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\m_2a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\n_3a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\p_r1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\q_r1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\r_r1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\s_r1a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\t_2a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\v_2a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\w_1a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Mother\Y_2a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Other\Geodata\G0_IMD_1b.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Other\Samples\Mother\Mother_samples_6c", nogen

keep aln mz010a preg_in_alsp preg_in_core preg_enrol_status mz005l mz005m mz013 mz014 mz028b ///
a006 a525 a636 a902 ///
b032 b106 b107 b122 b123 b360 b361 b362 b363 b364 b365 b366 b367 b368 b369 b370 b650 b653 b654 b655 b656 b663 - b667 b680 b681 b682 b683 b685 b695 b700 b701 b702 b706 b707 b708 b709 b710 b711 b712 b713 b714 b720 b721 b722 b730 b924 ///
c645 c645a c665 c666 c686 c706 c710 c711 c712 c713 c714 c715 c716 c717 c718 c719 c720 c721 c730 c731 c732 c733 c734 c735 c736 c737 c738 c739 c740 c741 c753 c755 c764 c765 c800 c801 c804 ///
d167 d168 d169 d170 d171 d172 d528 d529 d530 d531 d536 d537 d578 d579 d580 d581 d586 d587 d991 ///
e170 e171 e172 e173 e174 e175 e176 e177 e178 e179 e185 e186 e190 e191 e192 e193 e195 e196 e197 e198 e199 e200 e201 e202 e203 e205 e206 e207 e208 e209 e210 e211 e212 e213 e220 e221 e296 e297 e301 e326 e327 e380 e381 e382 e383 e384 e385 e387 e388 e389 e390 e391 e392 e393 e394 ///
f020 f021 f063 f190 f191 f192 f193 f194 f195 f196 f197 f198 f199 f200 f201 f202 f205 f304 f460 f518 f519 f526 f527 f600 f620 f625 f993 ///
g020 g021 g047 g048 g049 g053 g056 g059 g120 g280 g281 g282 g283 g284 g285 g286 g287 g288 g289 g290 g291 g292 g496 g515 g517 g613 g648 g750 g820 g822 g990 ///
h012 h013 h037 h038 h039 h043 h046 h190 h191 h192 h193 h194 h195 h196 h197 h198 h199 h200a h200b h200c h366 h385 h386 h489 h490 h498 h525 h602 h720 h723 h991a h991b h262 ///
j011 j012 j042 j043 j044 j048 j051 j054 j290 j292 j369 j370 j607 j608 j616 j630 j650 j735 j914 ///
k1010 k1011 k1020 k1022 k1042 k1043 k1044 k1050 k1053 k1057 k5010 k6180 k6190 k6280 k6281 k6282 k6283 k6284 k6285 k6286 k6287 k6288 k6289 k6290 k6291 k6292 k6293 k6295 k6300 k6301 k6302 k6303 k6304 k6305 k6306 k6307 k6308 k6309 k6310 k6311 k6312 k6313 k6314 k6315 k9991a ///
l3010 l3011 l3020 l3022 l3042 l3043 l3044 l3050 l3053 l3057 l5050 l5051 l6023 l6024 l6031 l6032 l6070 l6071 l6190 l9991a ///
m2010 m3030 m3040 m5160 m6100 m6161 m9991a ///
n1057 n1058 n1059 n1060 n1061 n1062 n2009 n2010 n2013 n2029 n2030 n2033 n4000 n4001 n4002 n4003 n4004 n4005 n4006 n4007 n4008 n4009 n4010 n4011 n4012 n4013 n4015 n4020 n4021 n4022 n4023 n4024 n4025 n4026 n4027 n4028 n4029 n4030 n4031 n4032 n4033 n4034 n4035 n4040 n4041 n4042 n4043 n4044 n4045 n4046 n4047 n4048 n4049 n4050 n4051 n4052 n4053 n4054 n4055 n4060 n4061 n4062 n4063 n4064 n4065 n4066 n4067 n4068 n4069 n4070 n4071 n4072 n4073 n4074 n4075 n4100 n4101 n4102 n4103 n4104 n4105 n4106 n4107 n4108 n4109 n4110 n4111 n4112 n4120 n4121 n4122 n4123 n4124 n4125 n4126 n4127 n4128 n4129 n4130 n4131 n4132 n5000 n5002 n5003 n5004 n5005 n5006 n5007a n5007b n5008 n5010 n5030 n5031 n5032 n5040 n5042 n5060 n5061 n8040 n8130 n9991a ///
p1010 p1011 p1020 p1022 p1052 p1053 p1054 p1060 p1063 p1067 p3023 p3024 p3032 p3070 p3071 p3190 p9991a ///
q2010 q3031 q3040 q4100 q4103 q4110 q4113 q9991a ///
r2019 r2020 r2021 r2022 r6010 r6012 r6013 r6014 r6015 r6016 r6017a r6017b r6018 r6040 r6060 r6061 r9020 r9021 r9022 r9023 r9024 r9025 r9991a ///
s1010 s1011 s1020 s1022 s1300 s1301 s3023 s3024 s3031 s3032 s3070 s3071 s3190 s4100 s4103 s4110 s4113 s7350 s7351 s9991a ///
t1010 t1050 t1150 t1151 t1152 t1153 t1154 t1155 t1156 t1157 t1158 t1159 t1160 t1161 t1162 t1163 t1164 t1165 t1166 t1167 t1300 t1301 t1302 t1303 t1304 t1305 t1306 t1307 t5402 t5403 t5404 t5406 t5410 t5412 t5500 t5501 t5520 t5521 t5526 t5560 t5580 t5581 t9991a t9991c ///
V0301 V0310 V1150 V1151 V1152 V1153 V1154 V1155 V1156 V1157 V1158 V1159 V1160 V1161 V1162 V1163 V1164 V1165 V1166 V1167 V1168 V1169 V5500 V5501 V5520 V5521 V5526 V5560 V5580 V5581 V9991 ///
W2220 W2222 W2278 W2279 W2295 W3220 W3278 W3279 W3294 W4220 W4222 W4278 W4279 W4295 W5220 W5278 W5279 W5294 W6220 W6222 W6278 W6279 W6295 W7220 W7278 W7279 W7294 ///
Y4000 Y4010 Y4020 Y4030 Y4040 Y4050 Y4060 Y4070 Y4080 Y4090 Y4095 Y4096 ///
aimd2000q5 aTownsendq5 aur01ind bimd2000q5 bTownsendq5 bur01ind cimd2000q5 cTownsendq5 cur01ind dimd2000q5 dTownsendq5 dur01ind eimd2000q5 eTownsendq5 eur01ind fimd2000q5 fTownsendq5 fur01ind gimd2000q5 gTownsendq5 gur01ind himd2000q5 hTownsendq5 hur01ind jimd2000q5 jTownsendq5 jur01ind kimd2000q5 kTownsendq5 kur01ind limd2000q5 lTownsendq5 lur01ind mimd2000q5 mTownsendq5 mur01ind nimd2000q5 nTownsendq5 nur01ind pimd2000q5 pTownsendq5 pur01ind qimd2000q5 qTownsendq5 qur01ind rimd2000q5 rTownsendq5 rur01ind simd2000q5 sTownsendq5 sur01ind timd2000q5 tTownsendq5 tur01ind ///
cotinine_Preg_Trim1_s1 cotinine_Preg_Trim1_s2 cotinine_Preg_Trim1_avg ///
cotinine_Preg_Trim3_s1 cotinine_Preg_Trim3_s2 cotinine_Preg_Trim3_avg ///
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
* G0 PARTNER - ***UNBLOCK SECTION WHEN REQUIRED***
* G0 Partner files - include here all files related to the G0 partner/father


use "\\path\to\alspac\filestore\Data\Current\Other\Cohort Profile\pz_1a.dta", clear
sort aln
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Partner\FPA_2a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Partner\pq_r1a.dta", nogen
merge 1:1 aln using "\\path\to\alspac\filestore\Data\Current\Quest\Partner\pm_r1b.dta", nogen

keep aln partner_in_alspac partner_data partner_enrolled partner_in_core pz_mult pz_multid partner_changed partner_changed_when ///
fpa5520 fpa5500 fpa5402 fpa5403 fpa5404 fpa5406 fpa5410 fpa5411 fpa5521 ///
pm1052 pm1053 pm1060 pm1063 ///
pq1010 pq1011 pq1020 pq1300 pq1301 pq3190 ///
partner_age second_partner_age

*keep in just those enrolled partners
keep if partner_in_alspac >= 1

* Removing withdrawl of consent cases *** FOR LARGE DATASETS THIS CAN TAKE A FEW MINUTES
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that partner based WoCs are set to .c

order aln partner_in_alspac, first
order partner_age second_partner_age, last

do "\\path\to\alspac\filestore\Data\Syntax\Withdrawal of consent\partner_WoC.do"

* Check there is a total of n=12113 G0 partners.
* Check withdrawal of consent frequencies partner=5  - currently none of these are in the formal cohort
tab1 partner_in_alspac, mis

save "\\path\to\ieu\project\folder\working\data\vaping_transitions\partner.dta", replace 
*/



*****************************************************************************************************************************************************************************************************************************.
* G1 Child BASED files - in this section the following file types need to be placed:
* Mother completed Qs about YP
* Obstetrics file OA

* ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need to remove the *****

use "\\path\to\alspac\filestore\Data\Current\Other\Cohort Profile\cp_3a.dta", clear
sort aln qlet
gen in_kz=1
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kb_7b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kd_5a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kf_8b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kj_7a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kl_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kn_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kq_3a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kr_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\ku_r2b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Based\kw_r2b.dta", nogen

keep aln qlet kz021 kz030 kz011b ///
kb450 kb451 kb452 kb453 kb454 kb455 kb456 kb457 kb458 kb459 kb460 kb461 kb462 kb463 kb464 kb465 kb470 kb471 kb879a ///
kd200 kd201 kd202a kd202b kd203a kd203b kd204 kd204b kd205a kd205b kd206 kd990 ///
kf230 kf231 kf232 kf233 kf234 kf235 kf237 kf238 kf239 kf240 kf999 ///
kj205 kj206 kj207 kj207a kj208 kj208a kj209 kj210 kj999a ///
kl220 kl221 kl222a kl222b kl223a kl223b kl226 kl227 kl991a ///
kn2000 kn2010 kn2011a kn2011b kn2020a kn2020b kn2021 kn9991a ///
kq250 kq251 kq252 kq253 kq254 kq255 kq256 kq257 kq258 kq259 kq260 kq261 kq264 kq265 kq998a ///
kr800 kr801 kr802 kr803 kr810 kr811 kr812 kr813 kr815 kr820 kr821 kr822 kr823 kr824 kr825 kr826 kr827 kr830 kr831 kr832 kr991a ///
ku660 ku661 ku662 ku663 ku664 ku665 ku666 ku667 ku668 ku669 ku670 ku671 ku672 ku673a ku673b ku673c ku720 ku721 ku991a ///
kw4060a kw4060b kw4061a kw4061b kw4062a kw4062b kw4063a kw4063b kw6000 kw6001 kw6002 kw6003 kw6004 kw6005 kw6006 kw6007 kw6008 kw6009 kw6010 kw6011 kw6012 kw6100a kw6100b kw6100c kw9991a ///
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
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccp_r1c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccq_r1c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccr_r1b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccs_r1b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\cct_1c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccu_2b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccxa_r1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\ccxd_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPA_r1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPB_r1e.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPC_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPD_1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPE_4a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPH_3a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPJ_1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Quest\Child Completed\YPK_1a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\f07_5a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\f08_4d.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\f09_4c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\f10_6b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\F11_5d.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\tf1_3b.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\tf2_5a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\tf3_4c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\tf4_6a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Clinic\Child\F24_6a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Other\Samples\Child\Child_bloods_6c.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Other\Geodata\G1_IMD_2a.dta", nogen
merge 1:1 aln qlet using "\\path\to\alspac\filestore\Data\Current\Other\Longitudinal\cLon_1a.dta", nogen

keep aln qlet kz021 kz030 ///
ccp800 ccp805 ccp810 ccp815 ccp820 ccp825 ccp830 ccp991a ccp991b ccq320 ccq991a ccq991b ///
ccr760 ccr765 ccr840 ccr841 ccr842 ccr843 ccr844 ccr850 ccr851 ccr852 ccr853 ccr854 ccr855 ccr856 ccr857 ccr991a ccr991b ///
ccs2210 ccs3500 ccs3540 ccs4060 ccs4065 ccs4150 ccs4151 ccs4152 ccs4153 ccs4154 ccs4160 ccs4161 ccs4162 ccs4163 ccs4164 ccs4165 ccs4166 ccs4167 ccs4168 ccs4169 ccs4170 ccs5510 ccs7500 ccs7510 ccs7520 ccs7521 ccs7522 ccs7523 ccs7524 ccs9991a ccs9991b  ///
cct2900 cct2901 cct2902 cct2903 cct2904 cct2905 cct2906 cct2907 cct2908 cct2909 cct2910 cct2911 cct2912 cct2913 cct2914 cct2915 cct2916 cct2917 cct2918 cct2919 cct2950 cct2955 cct2956 cct2957 cct2958 cct2959 cct2960 cct2961 cct2962 cct2963 cct2964 cct2965 cct2966 cct2967 cct2968 cct2969 cct2970 cct2971 cct2972 cct2973 cct2974 cct2980 cct2990 cct2992 cct2993 cct3100 cct3101 cct3102 cct4105 cct5020 cct5030 cct5050 cct5055 cct5100 cct5101 cct5102 cct5110 cct5111 cct5112 cct5120 cct5121 cct5122 cct5130 cct5131 cct5132 cct5140 cct5141 cct5142 cct5150 cct5151 cct5152 cct5160 cct5161 cct5162 cct5170 cct5171 cct5172 cct5180 cct5181 cct5182 cct9991a cct9991c cct5031 cct5032 ///
CCU1005 CCU3050 CCU3100 CCU3200 CCU3201 CCU3202 CCU3203 CCU3204 CCU3207 CCU3208 CCU3209 CCU3210 CCU3300 CCU3302 CCU3400 CCU3401 CCU3402 CCU3410 CCU3411 CCU3412 CCU3420 CCU3421 CCU3422 CCU3430 CCU3431 CCU3432 CCU3440 CCU3441 CCU3442 CCU3450 CCU3451 CCU3452 CCU3460 CCU3461 CCU3462 CCU4000 CCU4001 CCU4002 CCU4003 CCU4004 CCU4005 CCU4006 CCU4007 CCU4008 CCU4009 CCU4010 CCU4011 CCU4012 CCU4013 CCU4014 CCU4015 CCU4016 CCU4017 CCU4018 CCU4019 CCU4020 CCU4021 CCU4022 CCU4023 CCU4024 CCU4050 CCU4055 CCU4060 CCU4070 CCU4071 CCU4072 CCU4073 CCU4074 CCU9991 CCU1000 CCU3101 CCU3102 ///
ccxa290 ccxa300 ccxa310 ccxa320 ccxa330 ccxa340 ccxa341 ccxa342 ccxa343 ccxa344 ccxa345 ccxa346 ccxa991a ccxa991b ///
CCXD800 CCXD801 CCXD802 CCXD803 CCXD804 CCXD805 CCXD806 CCXD807 CCXD808 CCXD809 CCXD810 CCXD811 CCXD812 CCXD813 CCXD814 ///
YPA1020 YPA3310 YPA3311 YPA3312 YPA8000 YPA8010 YPA8020 YPA9020 YPA1000 YPA2000 YPA2010 YPA2020 YPA2030 YPA2040 YPA2050 YPA2060 YPA2070 YPA2080 YPA2090 YPA2100 YPA2110 YPA2120 YPA2130 YPA2131 YPA2132 YPA2133 YPA2134 YPA2140 YPA2141 YPA2142 YPA2143 YPA2144 YPA2150 YPA2151 YPA2152 YPA2160 YPA2170 YPA2180 YPA2190 YPA2200 YPA2210 YPA2220 YPA2230 YPA2240 YPA2250 YPA2260 YPA2270 YPA2280 YPA2290 YPA2300 YPA2310 YPA2320 YPA2330 YPA2340 YPA2350 YPA2360 YPA2370 YPA2380 ///
YPB1231 YPB1232 YPB1233 YPB2040 YPB4130 YPB4150 YPB4390 YPB4400 YPB4440 YPB4441 YPB4442 YPB4443 YPB4444 YPB4445 YPB4446 YPB4447 YPB4448 YPB4449 YPB4450 YPB4451 YPB4452 YPB4453 YPB4454 YPB4455 YPB4456 YPB4457 YPB4458 YPB4459 YPB4460 YPB4461 YPB4462 YPB4463 YPB4464 YPB4465 YPB4466 YPB4467 YPB4468 YPB4469 YPB4477 YPB4478 YPB4479 YPB5180 YPB6020 YPB6030 YPB9000 YPB9001 YPB9002 YPB9003 YPB9004 YPB9005 YPB9006 YPB9007 YPB9008 YPB9009 YPB9080 YPB9090 YPB9091 YPB9092 YPB9093 YPB9094 YPB9095 YPB9096 YPB9097 YPB9098 YPB9099 YPB9100 YPB9992 YPB7000 ///
YPC0420 YPC1050 YPC1070 YPC2450 YPC2451 YPC2452 YPC2453 YPC2454 YPC2455 YPC2456 YPC2457 YPC2458 YPC2459 YPC2460 YPC2491 YPC2492 YPC2493 YPC2650 YPC0490 YPC0491 YPC0492 YPC0493 YPC0494 YPC0495 YPC0496 YPC0497 YPC0498 YPC0499 YPC0500 YPC0501 YPC0502 YPC0503 YPC0600 ///
YPD1020 YPD1030 YPD9650 ///
YPE7470 YPE7471 YPE7472 YPE9650 YPE9660 YPE0101 YPE7440 ///
YPH9510 YPH9520 YPH3010 YPH5650 YPH5660 YPH7000 ///
YPJ4500 YPJ4501 YPJ4502 YPJ4503 YPJ4504 YPJ4505 YPJ4506 YPJ4507 YPJ4508 YPJ4509 YPJ4510 YPJ4511 YPJ4512 YPJ6000 ///
f7003c f7dd400 f7dd401 f7dd402 f7dd403 f7dd404 f7dd500 f7dd501 f7dd502 f7dd503 f7dd504 f7dd505 f7dd506 f7dd507 f7dd600 f7dd601 f7ms026a ///
f8003c f8aa107 f8aa108 ///
f9003c f9ms026a ///
fd003c fdaa480 fdaa481 fdaa482 fdaa490 fdaa491 fdaa492 fdaa500 fdaa510 fdaa520 fdaa521 fdaa522 fddd400 fddd401 fddd402 fddd403 fddd404 fddd405 fddd500 fddd501 fddd502 fddd503 fddd504 fddd505 fddd506 fddd507 fddp110 fddp111 fddp112 fddp113 fddp114 fddp115 fddp116 fddp117 fddp118 fddp119 fddp120 fddp121 fddp122 fddp123 fddp124 fddp125 fddp126 fddp130 fdms026a ///
fe003c feag100 feag101 feag102 feag103 feag104 feag105 feag106 feag107 feag108 feag109 feag110 feag111 feag112 feag113 feag114 feag115 feag116 feag117 feag118 feag119 feag120 feag121 feag157 feag158 feag159 feag160 feag161 feag162 feag163 feag164 feag165 feag166 feag167 feag168 feag200 feag201 feag202 feag203 feag204 feag205 feag206 feag207 feag208 feag209 feag210 feag211 feag212 feag213 feag214 feag215 feag216 feag217 feag218 feag219 feag220 feag221 feag222 feag223 feag224 feag225 feag226 feag227 feag228 feag229 feag230 feag231 feag232 feag233 feag234 feag235 feag236 feag237 feag238 febp191 febp193 fefs026 fefs029 fefs047 fefs049 fefs050 fems026a ///
FKAC1000 FKAC1010 FKAC1020 FKAC1030 FKAC1040 FKAC1050 FKAC1060 FKAC1070 FKAC1080 FKAC1090 FKAC1100 FKAC1110 FKAC1200 FKAC1210 FKAC1220 FKAC1230 FKAC1240 FKAC1250 FKAC1260 FKAC1270 FKAC1280 FKAC1290 FKAC1300 FKAC1400 FKAC1410 FKAC1420 FKAC1430 FKAC1440 FKAC1450 FKAC1460 FKAC1470 FKAC1480 FKAC1490 FKAC1500 FKAL1010 FKAL1020 FKAR0010 FKAR0011 FKCA1010 FKCA1013 FKCA1016 FKCO1101 FKCO1102 FKCO1103 FKCO1104 FKCO1105 FKCO1106 FKCO1107 FKDR1010 FKDR1015 FKDR1020 FKDR1025 FKDR1030 FKDR1035 FKDR1040 FKDR1045 FKDR1050 FKDR1055 FKDR1060 FKDR1065 FKDR1070 FKDR1075 FKDR1080 FKDR1085 FKDR1090 FKDR1095 FKFB1010 FKFB1011 FKFB1012 FKMS1040 FKPL1100 FKPL1101 FKPL1102 FKPL1103 FKPL1104 FKPL1200 FKPL1201 FKPL1202 FKPL1203 FKPL1204 FKPL1205 FKPL1206 FKPL1207 FKPL1208 FKPL1209 FKPL1210 FKPL1211 FKPL1212 FKPL1213 FKPL1214 FKPL1215 FKPL1216 FKPL1217 FKPL1218 FKPL1219 FKPL1300 FKPL1301 FKPL1302 FKPL1303 FKPL1304 FKPL1305 FKPL1306 FKPL1307 FKPL1308 FKPL1309 FKPL1310 FKPL1311 FKPL1312 FKPL1313 FKPL1314 FKPL1315 FKPL1316 FKPL1317 FKPL1318 FKPL1319 FKPL1320 FKPL1321 FKPL1322 FKPL1400 FKPL1401 FKPL1402 FKPL1403 FKPL1404 FKPL1405 FKPL1406 FKPL1407 FKPL1408 FKPL1409 FKPL1410 FKPL1411 FKPL1412 FKPL1413 FKPL1414 FKPL1500 FKPL1501 FKPL1502 FKPL1503 FKPL1504 FKPL1505 FKPL1506 FKPL1507 FKPL1508 FKPL1509 FKPL1510 FKPL1511 FKPL1512 FKPL1513 FKPL1514 FKPL1600 FKPL1601 FKPL1602 FKPL1603 FKPL1604 FKPL1605 FKPL1606 FKPL1607 FKPL1608 FKPL1609 FKPL1610 FKPL1611 FKPL1612 FKPL1613 FKPL1614 FKPL1615 FKPL1700 FKPL1701 FKPL1702 FKPL1703 FKPL1704 FKPL1705 FKPL1706 FKPL1707 FKPL1708 FKPL1709 FKPL1710 FKPL1711 FKPL1712 FKPL1713 FKPL1714 FKPL1715 FKPL1716 FKPL1717 FKPL1718 FKPL1719 FKPL1720 FKPL1721 FKPL1722 FKPL1723 FKPL1724 FKPL1725 FKPL1726 FKPL1727 FKPL1728 FKPL1729 FKPL1730 FKPL1731 FKPL1732 FKPL1733 FKPL1800 FKPL1801 FKPL1802 FKPL1803 FKPL1804 FKPL1805 FKPL1806 FKPL1807 FKPL1808 FKPL1809 FKPL1810 FKPL1811 FKPL1812 FKPL1813 FKPL1814 FKPL1815 FKPL1816 FKPL1817 FKPL1818 FKPL1819 FKPL1820 FKPL1821 FKPL1822 FKPL1823 FKPL1824 FKPL1900 FKPL1901 FKPL1902 FKPL1903 FKPL1904 FKPL1905 FKPL1906 FKPL1907 FKPL1908 FKPL1909 FKPL1910 FKPL1911 FKPL1912 FKPL1913 FKPL1914 FKPL1915 FKPL1916 FKPL1917 FKPL1918 FKPL1919 FKPL1920 FKPL1921 FKPL1922 FKPL1923 FKPL2000 FKPL2010 FKPL2020 FKPL2030 FKPL2100 FKPL2110 FKPL2120 FKPL2200 FKPL2210 FKPL2220 FKPL2230 FKPL2240 FKPL2250 FKPL2300 FKPL2310 FKPL2320 FKPL2330 FKPL2400 FKPL2410 FKPL2420 FKPL2430 FKPL2500 FKPL2510 FKPL2520 FKPL2600 FKPL2610 FKPL2620 FKPL2700 FKPL2710 FKPL2720 FKDQ1000 FKDQ1010 FKDQ1020 FKDQ1030 FKDQ1050 FKDQ1060 FKDQ1070 FKDQ1080 FKDQ1110 ///
ff0011a ff2039 ff5429 ff5447 ff5449 ff5450 ff6500 ff6501 ff6502 ff6503 ff6504 ff6505 ff6506 ff6507 ff6508 ff6509 ff6510 ff6511 ff6512 ff6513 ff6514 ff6515 ff7000 ff7001 ff7010 ff7011 ff7012 ff7015 ff7021 ff7700 ff7720 ff7721 ff7722 ff7730 ff7750 ff7751 ff7752 ff7753 ff8000 ff8100 ff8170 ///
fg0011a fg1203 fg1204 fg1205 fg1206 fg1207 fg1211 fg1212 fg1213 fg1214 fg1215 fg1216 fg1217 fg1218 fg1219 fg1220 fg1221 fg1222 fg1223 fg1224 fg1225 fg1226 fg1227 fg1228 fg1229 fg1230 fg1231 fg1232 fg1233 fg1241 fg1242 fg1243 fg1259 fg1260 fg1261 fg1271 fg1272 fg1273 fg1289 fg1290 fg1291 fg1299 fg1300 fg1301 fg1302 fg1303 fg1304 fg1305 fg1306 fg1307 fg1308 fg1309 fg1310 fg1311 fg1312 fg1313 fg1314 fg1315 fg1316 fg1317 fg1318 fg1319 fg1320 fg1321 fg1322 fg1323 fg1324 fg1325 fg1326 fg1327 fg1328 fg1329 fg1330 fg1331 fg1332 fg1333 fg1334 fg1335 fg1336 fg1337 fg1560 fg1570 fg1580 fg1581 fg1582 fg1583 fg1584 fg1585 fg1586 fg1590 fg1591 fg1592 fg1600 fg1601 fg3139 fg4147 fg4149 fg4150 fg4820 fg4821 fg4870 fg4871 fg4872 fg4873 fg4877 fg4892 fg5420 fg5421 fg5422 fg5423 fg5424 fg5425 fg5426 fg5427 fg5480 fg5500 fg5520 fg7210 fg7211 fg7212 fg7213 fg7214 fg7215 fg7216 fg7217 fg7218 fg7219 fg7220 fg7221 fg7222 fg7223 fg7224 fg7225 fg7226 ///
fh0011a fh3019 fh5010 fh5011 fh5012 fh5013 fh5014 fh5015 fh5016 fh5017 fh5018 fh5019 fh5020 fh5021 fh5067 fh5068 fh5069 fh5070 fh5071 fh5072 fh5073 fh5074 fh5075 fh5076 fh5077 fh5078 fh5100 fh5101 fh5102 fh5103 fh5104 fh5105 fh5106 fh5107 fh5108 fh5109 fh5110 fh5111 fh5112 fh5113 fh5114 fh5115 fh5116 fh5117 fh5118 fh5119 fh5120 fh5121 fh5122 fh5123 fh5124 fh5125 fh5126 fh5127 fh5128 fh5129 fh5130 fh5131 fh5132 fh5133 fh5134 fh5135 fh5136 fh5137 fh5314 fh5405 fh5406 fh5410 fh5411 fh5415 fh5416 fh5420 fh5421 fh5425 fh5426 fh6320 fh6330 fh6331 fh6332 fh6333 fh6334 fh6335 fh6340 fh6341 fh6342 fh6343 fh6344 fh6345 fh6346 fh6350 fh6351 fh6352 fh6360 fh6361 fh6370 fh6371 fh6372 fh6373 fh6374 fh6400 fh6401 fh6402 fh6403 fh6404 fh6405 fh6410 fh6411 fh6412 fh6413 fh6414 fh6420 fh6421 fh6422 fh6423 fh6424 fh6425 fh6430 fh6431 fh6432 fh6433 fh6435 fh6440 fh6441 fh6442 fh6443 fh6444 fh6445 fh6446 fh6447 fh6448 fh6449 fh6450 fh6451 fh6452 fh6453 fh6454 fh6455 fh6456 fh6457 fh6470 fh6471 fh6472 fh6473 fh6474 fh6475 fh6480 fh6481 fh6482 fh6483 fh6484 fh6500 fh6501 fh6502 fh6510 fh6511 fh6512 fh6513 fh6514 fh6515 fh6516 fh6517 fh6518 fh6519 fh6520 fh6530 fh6531 fh6540 fh6541 fh6542 fh6543 fh6544 fh6545 fh6546 fh6547 fh6548 fh6549 fh6550 fh6551 fh6570 fh6571 fh6572 fh6573 fh6574 fh6575 fh6600 fh6610 fh6611 fh6620 fh6621 fh6622 fh6623 fh6624 fh6625 fh6626 fh6627 fh6628 fh6629 fh6630 fh6631 fh6632 fh6633 fh6634 fh6640 fh6641 fh6670 fh6671 fh6672 fh6673 fh6674 fh6675 fh6700 fh6710 fh6711 fh6712 fh6720 fh6721 fh6722 fh6723 fh6724 fh6725 fh6726 fh6727 fh6728 fh6729 fh6730 fh6731 fh6732 fh6733 fh6740 fh6741 fh6742 fh6743 fh6750 fh6770 fh6771 fh6772 fh6773 fh6774 fh6775 fh6800 fh6801 fh6802 fh6803 fh6804 fh6805 fh6806 fh6807 fh6808 fh6809 fh6810 fh6811 fh6812 fh6820 fh6821 fh6822 fh6823 fh6824 fh6830 fh6831 fh6832 fh6833 fh6834 fh6881 fh6894 fh6897 fh8150 fh8151 fh8325 fh8340 fh8341 fh8342 fh8510 fh8511 fh8610 fh8611 fh8635 fh8700 fh8701 fh8702 fh8703 fh8704 fh8705 fh8706 fh8707 fh8708 fh8709 fh8710 ///
FJ003a FJ003b FJAA3300 FJAA3350 FJAA3400 FJAL050 FJAL1000 FJCI050 FJCI600 FJCI601 FJCI602 FJCI603 FJCI604 FJCI605 FJCI606 FJCI607 FJCI608 FJCI609 FJCI610 FJCI1000 FJCI1001 FJCI1002 FJCI1003 FJCI101 FJCI102 FJCI103 FJCI104 FJCI105 FJCI106 FJDR050 FJDR250 FJDR5000 FJDR5050 FJDR5100 FJDR5150 FJDR5200 FJDR5250 FJDR5300 FJDR5350 FJDR5400 FJDR5450 FJDR5500 FJDR5550 FJDR5600 FJDR5650 FJDR5700 FJDR5750 FJDR5800 FJDR5850 FJDR5900 FJDR5950 FJDR600 FJDR6000 FJDR6050 FJMR022a FJPL013 FJPL014 FJPL015 FJPL016 FJPL017 FJPL018 FJPL019 FJPL020 FJPL021 FJPL022 FJPL023 FJPL024 FJPL025 FJPL026 FJPL027 FJPL028 FJPL029 FJPL030 FJPL031 FJPL032 FJPL033 FJPL034 FJPL035 FJPL036 FJPL037 FJPL038 FJPL039 FJPL040 FJPL041 FJPL042 FJPL043 FJPL044 FJPL045 FJPL046 FJPL047 FJPL048 FJPL049 FJPL050 FJPL051 FJPL052 FJPL053 FJPL054 FJPL055 FJPL056 FJPL057 FJPL058 FJPL059 FJPL060 FJPL061 FJPL062 FJPL063 FJPL064 FJPL065 FJPL066 FJPL067 FJPL068 FJPL069 FJPL070 FJPL071 FJPL072 FJPL073 FJPL074 FJPL075 FJPL076 FJPL077 FJPL078 FJPL079 FJPL080 FJPL081 FJPL082 FJPL083 FJPL084 FJPL085 FJPL086 FJPL087 FJPL088 FJPL089 FJPL090 FJPL091 FJPL092 FJPL093 FJPL094 FJPL095 FJPL096 FJPL097 FJPL098 FJPL099 FJPL100 FJPL101 FJPL102 FJPL103 FJPL104 FJPL105 FJPL106 FJPL107 FJPL108 FJPL109 FJPL110 FJPL111 FJPL112 FJPL113 FJPL160 FJPL161 FJPL162 FJPL163 FJPL164 FJPL165 FJPL166 FJPL167 FJPL167_new FJPL168 FJPL169 FJPL170 FJPL171 FJPL172 FJAL1050 FJAL1100 ///
Cotinine_F7 Cotinine_TF3 cotinine_TF4_s1 cotinine_TF4_s2 cotinine_TF4_avg ///
clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109 clon111 clon112 clon113 clon114 clon115 clon116 clon117 clon118 clon119 clon120 clon121 clon122 clon123 clon140 clon141 clon142 clon143 clon144 clon145 clon146 clon147 clon148 clon149 clon150 clon151 clon152 clon153 clon154 clon155 clon156 clon157 clon158 clon159 clon160 clon161 clon162 clon163 clon164 clon165 clon166 clon167 clon168 clon169 clon170 ///
ccbimd2000q5 ccbTownsendq5 ccbur01ind cccimd2000q5 cccTownsendq5 cccur01ind ccdimd2000q5 ccdTownsendq5 ccdur01ind cceimd2000q5 cceTownsendq5 cceur01ind ccfimd2000q5 ccfTownsendq5 ccfur01ind ccgimd2000q5 ccgTownsendq5 ccgur01ind cchimd2000q5 cchTownsendq5 cchur01ind ccjimd2000q5 ccjTownsendq5 ccjur01ind cckimd2000q5 cckTownsendq5 cckur01ind cclimd2000q5 cclTownsendq5 cclur01ind ccmimd2000q5 ccmTownsendq5 ccmur01ind ccnimd2000q5 ccnTownsendq5 ccnur01ind ccpimd2000q5 ccpTownsendq5 ccpur01ind ccqimd2000q5 ccqTownsendq5 ccqur01ind ccrimd2000q5 ccrTownsendq5 ccrur01ind ccsimd2000q5 ccsTownsendq5 ccsur01ind cctimd2000q5 cctTownsendq5 cctur01ind ccuimd2000q5 ccuTownsendq5 ccuur01ind YPAur01ind YPATownsendq5 YPAimd2000q5 ///
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
merge m:1 aln using "\\path\to\ieu\project\folder\working\data\vaping_transitions\partner.dta", nogen

* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1

drop in_alsp tripquad
save "\\path\to\ieu\project\folder\working\data\vaping_transitions\vaping_exp_vars_ALSPAC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* QC checks*
use "\\path\to\ieu\project\folder\working\data\vaping_transitions\vaping_exp_vars_ALSPAC.dta", clear

* Check that there are 15645 records.
count
