##LOAD
library(haven)
exp <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_vars_ALSPAC.dta')
expdf <- data.frame(exp[,c('aln','qlet')])

###MODIFIABLE FACTORS

##Familial smoking
#Mother ever smoked
expdf$m_eversmk_gest <- factor(exp$b650, levels=c(2:1), labels=c('No','Yes'))
expdf$m_eversmk_8 <- factor(exp$n5000, levels=c(2:1), labels=c('No','Yes'))
expdf$m_eversmk_11 <- factor(exp$r6010, levels=c(2:1), labels=c('No','Yes'))
expdf$m_eversmk_18 <- factor(exp$t5560, levels=c(2:1), labels=c('No','Yes')) #(ever smoked in past)
expdf$m_eversmk_22 <- factor(exp$V5560, levels=c(2:1), labels=c('No','Yes')) #(ever smoked in past)

#Mother smoked regularly
expdf$m_regsmk_gest <- ifelse(exp$b653 %in% 1 | exp$b654 %in% 1 | exp$b655 %in% 1, 1, NA) #(ever reg. smoke)
expdf$m_regsmk_gest <- ifelse((exp$b653 %in% 2 & exp$b654 %in% 2 & exp$b655 %in% 2), 2, expdf$m_regsmk_gest)
expdf$m_regsmk_gest <- factor(expdf$m_regsmk_gest, levels=c(2:1), labels=c('No','Yes'))

expdf$m_regsmk_8w <- ifelse(exp$e171 %in% 1 | exp$e173 %in% 1 | exp$e175 %in% 1, 1, NA) #(since birth of child)
expdf$m_regsmk_8w <- ifelse(exp$e171 %in% 2 & exp$e173 %in% 2 & exp$e175 %in% 2, 2, expdf$m_regsmk_8w)
expdf$m_regsmk_8w <- factor(expdf$m_regsmk_8w, levels=c(2:1), labels=c('No','Yes'))

expdf$m_regsmk_8 <- ifelse(exp$n5002 %in% 1 | exp$n5003 %in% 1 | exp$n5004 %in% 1, 1, NA) #(ever reg. smoke)
expdf$m_regsmk_8 <- ifelse((exp$n5002 %in% 2 & exp$n5003 %in% 2 & exp$n5004 %in% 2) | (is.na(expdf$m_regsmk_8) & exp$n5000 %in% 2), 2, expdf$m_regsmk_8)
expdf$m_regsmk_8 <- factor(expdf$m_regsmk_8, levels=c(2:1), labels=c('No','Yes'))

expdf$m_regsmk_11 <- ifelse(exp$r6012 %in% 1 | exp$r6013 %in% 1 | exp$r6014 %in% 1, 1, NA) #(ever reg. smoke)
expdf$m_regsmk_11 <- ifelse((is.na(expdf$m_regsmk_11) & exp$r6010 %in% 2), 2, expdf$m_regsmk_11)
expdf$m_regsmk_11 <- factor(expdf$m_regsmk_11, levels=c(2:1), labels=c('No','Yes'))

#Mother smoked in pregnancy
expdf$m_pre_pregsmk_gest <- ifelse(exp$b663 %in% c(2:4), 1, NA)
expdf$m_pre_pregsmk_gest <- ifelse(is.na(expdf$m_pre_pregsmk_gest) & (exp$b663 %in% 1), 2, expdf$m_pre_pregsmk_gest)
expdf$m_pre_pregsmk_gest <- factor(expdf$m_pre_pregsmk_gest, levels=c(2:1), labels=c('No','Yes'))

expdf$m_f3m_pregsmk_gest <- ifelse(exp$b665 %in% c(2:4), 1, NA)
expdf$m_f3m_pregsmk_gest <- ifelse(is.na(expdf$m_f3m_pregsmk_gest) & (exp$b665 %in% 1), 2, expdf$m_f3m_pregsmk_gest)
expdf$m_f3m_pregsmk_gest <- factor(expdf$m_f3m_pregsmk_gest, levels=c(2:1), labels=c('No','Yes'))

expdf$m_l2w_pregsmk_gest <- ifelse(exp$b667 %in% c(2:4), 1, NA)
expdf$m_l2w_pregsmk_gest <- ifelse(is.na(expdf$m_l2w_pregsmk_gest) & (exp$b667 %in% 1), 2, expdf$m_l2w_pregsmk_gest)
expdf$m_l2w_pregsmk_gest <- factor(expdf$m_l2w_pregsmk_gest, levels=c(2:1), labels=c('No','Yes'))

expdf$m_l2m_pregsmk_8w <- ifelse(exp$e170 %in% 1 | exp$e172 %in% 1 | exp$e174 %in% 1, 1, NA)
expdf$m_l2m_pregsmk_8w <- ifelse(is.na(expdf$m_l2m_pregsmk_8w) & (exp$e170 %in% 2 | exp$e172 %in% 2 | exp$e174 %in% 2), 2, expdf$m_l2m_pregsmk_8w)
expdf$m_l2m_pregsmk_8w <- factor(expdf$m_l2m_pregsmk_8w, levels=c(2:1), labels=c('No','Yes'))

#Mother smokes daily
expdf$m_dailsmk_8w <- ifelse(exp$e179 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_8w <- ifelse(is.na(expdf$m_dailsmk_8w) & exp$e179 %in% 0, 2, expdf$m_dailsmk_8w)
expdf$m_dailsmk_8w <- factor(expdf$m_dailsmk_8w, levels=c(2:1), labels=c('No','Yes')) #(number smoked daily past week)

expdf$m_dailsmk_1 <- ifelse(exp$f620 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_1 <- ifelse(is.na(expdf$m_dailsmk_1) & exp$f620 %in% 0, 2, expdf$m_dailsmk_1)
expdf$m_dailsmk_1 <- factor(expdf$m_dailsmk_1, levels=c(2:1), labels=c('No','Yes')) #(number smoked daily at present)

expdf$m_dailsmk_2 <- ifelse(exp$g820 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_2 <- ifelse(is.na(expdf$m_dailsmk_2) & exp$g820 %in% 0, 2, expdf$m_dailsmk_2)
expdf$m_dailsmk_2 <- factor(expdf$m_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day)

expdf$m_dailsmk_3 <- ifelse(exp$h720 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_3 <- ifelse(is.na(expdf$m_dailsmk_3) & exp$h720 %in% 0, 2, expdf$m_dailsmk_3)
expdf$m_dailsmk_3 <- factor(expdf$m_dailsmk_3, levels=c(2:1), labels=c('No','Yes'))

expdf$m_dailsmk_4 <- ifelse(exp$j735 %in% c(1:18,20,22,24:25,30,35,40,50), 1, NA)
expdf$m_dailsmk_4 <- ifelse(is.na(expdf$m_dailsmk_4) & exp$j735 %in% 0, 2, expdf$m_dailsmk_4)
expdf$m_dailsmk_4 <- factor(expdf$m_dailsmk_4, levels=c(2:1), labels=c('No','Yes'))

expdf$m_dailsmk_5 <- ifelse(exp$k6180 %in% c(1,5,9,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_5 <- ifelse(is.na(expdf$m_dailsmk_5) & exp$k6180 %in% 0, 2, expdf$m_dailsmk_5)
expdf$m_dailsmk_5 <- factor(expdf$m_dailsmk_5, levels=c(2:1), labels=c('No','Yes'))

expdf$m_dailsmk_6 <- ifelse(exp$l5050 %in% c(1:20,22,24,25,30,35,40,45,60) | exp$l5051 %in% c(1:21,24:25,30,32,35,40,50), 1, NA)
expdf$m_dailsmk_6 <- ifelse(exp$l5050 %in% 0 & exp$l5051 %in% 0, 2, expdf$m_dailsmk_6)
expdf$m_dailsmk_6 <- factor(expdf$m_dailsmk_6, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes nowadays per day weekday | weekend day)

expdf$m_dailsmk_7 <- ifelse(exp$m5160 %in% c(1,5,9,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_7 <- ifelse(is.na(expdf$m_dailsmk_7) & exp$m5160 %in% 0, 2, expdf$m_dailsmk_7)
expdf$m_dailsmk_7 <- factor(expdf$m_dailsmk_7, levels=c(2:1), labels=c('No','Yes'))

expdf$m_dailsmk_12 <- ifelse(exp$s1300 %in% c(1:18,20,23,25,27,30,40,50,60) | exp$s1301 %in% c(1:10,12:18,20,25,27:28,30,35,40,50,60), 1, NA)
expdf$m_dailsmk_12 <- ifelse(exp$s1300 %in% 0 & exp$s1301 %in% 0, 2, expdf$m_dailsmk_12)
expdf$m_dailsmk_12 <- factor(expdf$m_dailsmk_12, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day weekdays | weekends)

expdf$m_dailsmk_18 <- ifelse(exp$t5521 %in% 1, 1, NA)
expdf$m_dailsmk_18 <- ifelse(exp$t5521 %in% 2 | exp$t5520 %in% 2, 2, expdf$m_dailsmk_18)
expdf$m_dailsmk_18 <- factor(expdf$m_dailsmk_18, levels=c(2:1), labels=c('No','Yes')) #(smokes every day)

expdf$m_dailsmk_22 <- ifelse(exp$V5521 %in% 1, 1, NA)
expdf$m_dailsmk_22 <- ifelse(exp$V5521 %in% 2 | exp$V5520 %in% 2, 2, expdf$m_dailsmk_22)
expdf$m_dailsmk_22 <- factor(expdf$m_dailsmk_22, levels=c(2:1), labels=c('No','Yes'))

#Mother currently smokes
expdf$m_currsmk_18 <- factor(exp$t5520, levels=c(2:1), labels=c('No','Yes'))
expdf$m_currsmk_22 <- factor(exp$V5520, levels=c(2:1), labels=c('No','Yes')) #(cigarettes or tobacco specified)

#Mother stopped smoking/length of time (yrs) since quitting
expdf$m_stopsmk_8 <- factor(exp$n5006, levels=c(2:1), labels=c('No','Yes'))
expdf$m_stopsmk_11 <- factor(exp$r6016, levels=c(2:1), labels=c('No','Yes'))

exp[,c('n5007a','r6017a','t5580','V5580','n5007b','r6017b','t5581','V5581')] <- sapply(exp[,c('n5007a','r6017a','t5580','V5580','n5007b','r6017b','t5581','V5581')], function(x) replace(x, x < 0, NA))
exp[,c('n5007b','r6017b','t5581','V5581')] <- exp[,c('n5007b','r6017b','t5581','V5581')]/12
expdf$m_quitsmk_8 <- exp$n5007a + exp$n5007b
expdf$m_quitsmk_8 <- factor(ifelse(expdf$m_quitsmk_8 > 5, 0, 1), levels=c(0:1), labels=c('>5','<=5'))

expdf$m_quitsmk_11 <- exp$r6017a + exp$r6017b
expdf$m_quitsmk_11 <- factor(ifelse(expdf$m_quitsmk_11 > 5, 0, 1), levels=c(0:1), labels=c('>5','<=5'))

expdf$m_quitsmk_18 <- exp$t5580 + exp$t5581
expdf$m_quitsmk_18 <- factor(ifelse(expdf$m_quitsmk_18 > 5, 0, 1), levels=c(0:1), labels=c('>5','<=5'))

expdf$m_quitsmk_22 <- exp$V5580 + exp$V5581
expdf$m_quitsmk_22 <- factor(ifelse(expdf$m_quitsmk_22 > 5, 0, 1), levels=c(0:1), labels=c('>5','<=5'))

#Partner smokes
expdf$p_smk_gest <- ifelse(exp$b683 %in% c(2:4), 1, NA)
expdf$p_smk_gest <- ifelse(exp$b683 %in% 1, 2, expdf$p_smk_gest)
expdf$p_smk_gest <- factor(expdf$p_smk_gest, levels=c(2:1), labels=c('No','Yes')) #partner smokes

expdf$p_smk_8 <- ifelse(exp$n5040 %in% c(3:5), 1, NA)
expdf$p_smk_8 <- ifelse(exp$n5040 %in% 2, 2, expdf$p_smk_8)
expdf$p_smk_8 <- factor(expdf$p_smk_8, levels=c(2:1), labels=c('No','Yes'))

expdf$p_smk_11 <- ifelse(exp$r6040 %in% c(3:5), 1, NA)
expdf$p_smk_11 <- ifelse(exp$r6040 %in% 2, 2, expdf$p_smk_11)
expdf$p_smk_11 <- factor(expdf$p_smk_11, levels=c(2:1), labels=c('No','Yes')) #live-in husband

#Partner smokes daily
expdf$p_dailsmk_l2m_8w <- ifelse(exp$e185 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$p_dailsmk_l2m_8w <- ifelse(is.na(expdf$p_dailsmk_l2m_8w) & exp$e185 %in% 0, 2, expdf$p_dailsmk_l2m_8w)
expdf$p_dailsmk_l2m_8w <- factor(expdf$p_dailsmk_l2m_8w, levels=c(2:1), labels=c('No','Yes'))

expdf$p_dailsmk_8w <- ifelse(exp$e186 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$p_dailsmk_8w <- ifelse(is.na(expdf$p_dailsmk_8w) & exp$e186 %in% 0, 2, expdf$p_dailsmk_8w)
expdf$p_dailsmk_8w <- factor(expdf$p_dailsmk_8w, levels=c(2:1), labels=c('No','Yes')) #(last 2 weeks)

expdf$p_dailsmk_2 <- ifelse(exp$g648 %in% c(1:20,22,24:25,28,30,35,40,50,60), 1, NA)
expdf$p_dailsmk_2 <- ifelse(is.na(expdf$p_dailsmk_2) & exp$g648 %in% 0, 2, expdf$p_dailsmk_2)
expdf$p_dailsmk_2 <- factor(expdf$p_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(n cigs partner smokers every day)

expdf$p_dailsmk_3 <- ifelse(exp$h525 %in% c(1:18,20,25:26,30,35,40,45,50,60), 1, NA)
expdf$p_dailsmk_3 <- ifelse(is.na(expdf$p_dailsmk_3) & exp$h525 %in% 0, 2, expdf$p_dailsmk_3)
expdf$p_dailsmk_3 <- factor(expdf$p_dailsmk_3, levels=c(2:1), labels=c('No','Yes'))

expdf$p_dailsmk_4 <- ifelse(exp$j630 %in% c(1:18,20,22,24,25,28,30,35,40,50), 1, NA)
expdf$p_dailsmk_4 <- ifelse(is.na(expdf$p_dailsmk_4) & exp$j630 %in% 0, 2, expdf$p_dailsmk_4)
expdf$p_dailsmk_4 <- factor(expdf$p_dailsmk_4, levels=c(2:1), labels=c('No','Yes'))

expdf$p_dailsmk_6 <- ifelse(exp$l6070 %in% c(1:18,20,24,25,30,35,40,50,60) | exp$l6071 %in% c(1:18,20,22,25,30,35,40,48,50,60), 1, NA)
expdf$p_dailsmk_6 <- ifelse(exp$l6070 %in% 0 & exp$l6071 %in% 0, 2, expdf$p_dailsmk_6)
expdf$p_dailsmk_6 <- factor(expdf$p_dailsmk_6, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day weekdays | weekends)

expdf$p_dailsmk_8 <- ifelse(exp$n5042 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$p_dailsmk_8 <- ifelse(exp$n5042 %in% 0 | exp$n5040 %in% 2, 2, expdf$p_dailsmk_8)
expdf$p_dailsmk_8 <- factor(expdf$p_dailsmk_8, levels=c(2:1), labels=c('No','Yes'))

expdf$p_dailsmk_9 <- ifelse(exp$p3070 %in% c(1:10,12:18,20,22,25,30,35,40,50,60,80) | exp$p3071 %in% c(1:18,20,22,24,25,26,30,35,40,45,50,60,80), 1, NA)
expdf$p_dailsmk_9 <- ifelse(exp$p3070 %in% 0 & exp$p3071 %in% 0, 2, expdf$p_dailsmk_9)
expdf$p_dailsmk_9 <- factor(expdf$p_dailsmk_9, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day weekdays | weekends)

expdf$p_dailsmk_12 <- ifelse(exp$s3070 %in% c(1:10,12:18,20,24,25,28,30,35,40,50,60) | exp$s3071 %in% c(1:10,12,14:16,18,20,24,25,28,30,35,40,50,60), 1, NA)
expdf$p_dailsmk_12 <- ifelse(exp$s3070 %in% 0 & exp$s3071 %in% 0, 2, expdf$p_dailsmk_12)
expdf$p_dailsmk_12 <- factor(expdf$p_dailsmk_12, levels=c(2:1), labels=c('No','Yes'))

#Maternal grandmother smoked while pregnant
expdf$mgm_pregsmk_gest <- factor(exp$b681, levels=c(2:1), labels=c('No','Yes')) #mother smoked when expecting mum
expdf$mgm_pregsmk_8 <- factor(exp$n5031, levels=c(2:1), labels=c('No','Yes')) #mother's mother smoked when pregnant with mother

expdf$mgm_pregsmk_25 <- factor(exp$W2222, levels=c(0:1), labels=c('No','Yes')) #mgm smoked when pregnant with mother
expdf$mgmm_pregsmk_25 <- factor(exp$W4222, levels=c(0:1), labels=c('No','Yes')) #mgmm smoked when pregnant with mgm
expdf$mgfm_pregsmk_25 <- factor(exp$W6222, levels=c(0:1), labels=c('No','Yes'))#mgfm smoked when pregnant with mgf

#Maternal grandparents smoked
expdf$mgm_eversmk_gest <- factor(exp$b680, levels=c(2:1), labels=c('No','Yes')) #mother ever smoked
expdf$mgf_eversmk_gest <- factor(exp$b682, levels=c(2:1), labels=c('No','Yes')) #father ever smoked
expdf$mgm_eversmk_8 <- factor(exp$n5030, levels=c(2:1), labels=c('No','Yes')) #mother's mother is/was smoker
expdf$mgf_eversmk_8 <- factor(exp$n5032, levels=c(2:1), labels=c('No','Yes')) #mother's father is/was smoker

expdf$mgm_eversmk_25 <- ifelse(exp$W2220 %in% c(1:2), 1, NA)
expdf$mgm_eversmk_25 <- ifelse(exp$W2220 %in% 0, 2, expdf$mgm_eversmk_25) 
expdf$mgm_eversmk_25 <- factor(expdf$mgm_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgm smoked regularly during childhood

expdf$mgf_eversmk_25 <- ifelse(exp$W3220 %in% c(1:2), 1, NA)
expdf$mgf_eversmk_25 <- ifelse(exp$W3220 %in% 0, 2, expdf$mgf_eversmk_25)
expdf$mgf_eversmk_25 <- factor(expdf$mgf_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgf smoked regularly during childhood

#Maternal great grandparents smoked
expdf$mgmm_eversmk_25 <- ifelse(exp$W4220 %in% c(1:2), 1, NA)
expdf$mgmm_eversmk_25 <- ifelse(exp$W4220 %in% 0, 2, expdf$mgmm_eversmk_25)
expdf$mgmm_eversmk_25 <- factor(expdf$mgmm_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgmm smoked regularly during childhood

expdf$mgmf_eversmk_25 <- ifelse(exp$W5220 %in% c(1:2), 1, NA)
expdf$mgmf_eversmk_25 <- ifelse(exp$W5220 %in% 0, 2, expdf$mgmf_eversmk_25)
expdf$mgmf_eversmk_25 <- factor(expdf$mgmf_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgmf smoked regularly during childhood

expdf$mgfm_eversmk_25 <- ifelse(exp$W6220 %in% c(1:2), 1, NA)
expdf$mgfm_eversmk_25 <- ifelse(exp$W6220 %in% 0, 2, expdf$mgfm_eversmk_25)
expdf$mgfm_eversmk_25 <- factor(expdf$mgfm_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgfm smoked regularly during childhood

expdf$mgff_eversmk_25 <- ifelse(exp$W7220 %in% c(1:2), 1, NA)
expdf$mgff_eversmk_25 <- ifelse(exp$W7220 %in% 0, 2, expdf$mgff_eversmk_25)
expdf$mgff_eversmk_25 <- factor(expdf$mgff_eversmk_25, levels=c(2:1), labels=c('No','Yes')) #mgff smoked regularly during childhood

#Household members smoke
expdf$hh_smk_gest <- factor(exp$b695, levels=c(2:1), labels=c('No','Yes'))

expdf$hh_smk_2 <- ifelse(exp$g515 > 0, 1, NA)
expdf$hh_smk_2 <- ifelse(exp$g515 %in% 0, 2, expdf$hh_smk_2)
expdf$hh_smk_2 <- factor(expdf$hh_smk_2, levels=c(2:1), labels=c('No','Yes')) #(other household members smoker)

expdf$hh_smk_3 <- ifelse(exp$h385 > 0, 1, NA)
expdf$hh_smk_3 <- ifelse(exp$h385 %in% 0, 2, expdf$hh_smk_3)
expdf$hh_smk_3 <- factor(expdf$hh_smk_3, levels=c(2:1), labels=c('No','Yes')) #(no of smokers in household)

expdf$hh_smk_4 <- ifelse(exp$j369 > 0, 1, NA)
expdf$hh_smk_4 <- ifelse(exp$j369 %in% 0, 2, expdf$hh_smk_4)
expdf$hh_smk_4 <- factor(expdf$hh_smk_4, levels=c(2:1), labels=c('No','Yes'))

expdf$hh_smk_7 <- ifelse(exp$m3030 > 0, 1, NA)
expdf$hh_smk_7 <- ifelse(exp$m3030 %in% 0, 2, expdf$hh_smk_7)
expdf$hh_smk_7 <- factor(expdf$hh_smk_7, levels=c(2:1), labels=c('No','Yes')) #(inc mother)

expdf$hh_smk_8 <- factor(exp$n5060, levels=c(2:1), labels=c('No','Yes')) #(other members of the household smoke)

expdf$hh_smk_10 <- ifelse(exp$q3031 > 0, 1, NA)
expdf$hh_smk_10 <- ifelse(exp$q3031 %in% 0, 2, expdf$hh_smk_10)
expdf$hh_smk_10 <- factor(expdf$hh_smk_10, levels=c(2:1), labels=c('No','Yes')) #(number of smokers in household)

expdf$hh_smk_11 <- factor(exp$r6060, levels=c(2:1), labels=c('No','Yes')) #(apart from respondent/husband/partner, other member of household smokes)


##Familial substance use
#Mother alcohol consumption in pregnancy
expdf$m_pre_pregalc_gest <- ifelse(exp$b720 %in% c(1:2), 1, NA) 
expdf$m_pre_pregalc_gest <- ifelse(exp$b720 %in% c(3:6), 2, expdf$m_pre_pregalc_gest)
expdf$m_pre_pregalc_gest <- factor(expdf$m_pre_pregalc_gest, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(alcohol consumption before this preg)

expdf$m_f3m_pregalc_gest <- ifelse(exp$b721 %in% c(1:2), 1, NA) 
expdf$m_f3m_pregalc_gest <- ifelse(exp$b721 %in% c(3:6), 2, expdf$m_f3m_pregalc_gest)
expdf$m_f3m_pregalc_gest <- factor(expdf$m_f3m_pregalc_gest, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(alcohol consumption in 1-3mths this preg)

expdf$m_mov_pregalc_gest <- ifelse(exp$b722 %in% c(1:2), 1, NA) 
expdf$m_mov_pregalc_gest <- ifelse(exp$b722 %in% c(3:6), 2, expdf$m_mov_pregalc_gest)
expdf$m_mov_pregalc_gest <- factor(expdf$m_mov_pregalc_gest, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(alcohol consumption since baby 1st moved)

expdf$m_l2m_pregalc_8w <- ifelse(exp$e220 %in% c(1:2), 1, NA) 
expdf$m_l2m_pregalc_8w <- ifelse(exp$e220 %in% c(3:6), 2, expdf$m_l2m_pregalc_8w)
expdf$m_l2m_pregalc_8w <- factor(expdf$m_l2m_pregalc_8w, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(freq of alcohol use in last 2 months of this preg)

#Mother alcohol consumption
expdf$m_alc_8w <- ifelse(exp$e221 %in% c(1:2), 1, NA) 
expdf$m_alc_8w <- ifelse(exp$e221 %in% c(3:6), 2, expdf$m_alc_8w)
expdf$m_alc_8w <- factor(expdf$m_alc_8w, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(freq alcohol use since birth)

expdf$m_alc_1 <- ifelse(exp$f625 %in% c(1:2), 1, NA) 
expdf$m_alc_1 <- ifelse(exp$f625 %in% c(3:6), 2, expdf$m_alc_1)
expdf$m_alc_1 <- factor(expdf$m_alc_1, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #(alcohol consumption)

expdf$m_alc_2 <- ifelse(exp$g822 %in% c(1:2), 1, NA) 
expdf$m_alc_2 <- ifelse(exp$g822 %in% c(3:6), 2, expdf$m_alc_2)
expdf$m_alc_2 <- factor(expdf$m_alc_2, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #quantity of alcohol mum drinks

expdf$m_alc_3 <- ifelse(exp$h723 %in% c(1:2), 1, NA) 
expdf$m_alc_3 <- ifelse(exp$h723 %in% c(3:6), 2, expdf$m_alc_3)
expdf$m_alc_3 <- factor(expdf$m_alc_3, levels=c(1:2), labels=c('Never/<1 per week','1+ per week'))

expdf$m_alc_4 <- ifelse(exp$j292 %in% 0, 1, NA)
expdf$m_alc_4 <- ifelse(exp$j292 > 0, 2, expdf$m_alc_4)
expdf$m_alc_4 <- factor(expdf$m_alc_4, levels=c(1:2), labels=c('Never/<1 per week','1+ per week')) #total alcohol drinks per week

expdf$m_alc_5 <- ifelse(exp$k6190 %in% c(1:2), 1, NA)
expdf$m_alc_5 <- ifelse(exp$k6190 %in% c(3:6), 2, expdf$m_alc_5)
expdf$m_alc_5 <- factor(expdf$m_alc_5, levels=c(1:2), labels=c('None/<1 per week','1+ per week')) #amount of alcohol mother drinks

expdf$m_alc_7 <- factor(exp$m6100, levels=c(2:1), labels=c('No','Yes')) #drank past week

expdf$m_alc_18 <- ifelse(exp$t5500 %in% c(1:2), 1, NA)
expdf$m_alc_18 <- ifelse(exp$t5500 %in% c(3:6), 2, expdf$m_alc_18)
expdf$m_alc_18 <- factor(expdf$m_alc_18, levels=c(1:2), labels=c('Never/Monthly or less','2+ per month'))

expdf$m_alc_22 <- ifelse(exp$V5500 %in% c(1:2), 1, NA)
expdf$m_alc_22 <- ifelse(exp$V5500 %in% c(3:6), 2, expdf$m_alc_22)
expdf$m_alc_22 <- factor(expdf$m_alc_22, levels=c(1:2), labels=c('Never/Monthly or less','2+ per month'))

#Partner alcohol consumption
expdf$p_alc_gest <- ifelse(exp$b730 %in% c(1:2), 1, NA)
expdf$p_alc_gest <- ifelse(exp$b730 %in% c(3:6), 2, expdf$p_alc_gest)
expdf$p_alc_gest <- factor(expdf$p_alc_gest, levels=c(1:2), labels=c('Never/Very occasionally','Occasionally/1+ per week')) #(partner's alcohol consumption)

expdf$p_alc_1 <- ifelse(exp$f600 %in% c(1:2), 1, NA)
expdf$p_alc_1 <- ifelse(exp$f600 %in% c(3:6), 2, expdf$p_alc_1)
expdf$p_alc_1 <- factor(expdf$p_alc_1, levels=c(1:2), labels=c('Never/<1 per week','1+ per week'))

expdf$p_alc_2 <- ifelse(exp$g750 %in% c(1:2), 1, NA)
expdf$p_alc_2 <- ifelse(exp$g750 %in% c(3:6), 2, expdf$p_alc_2)
expdf$p_alc_2 <- factor(expdf$p_alc_2, levels=c(1:2), labels=c('Never/Very occasionally','Occasionally/1+ per day'))

expdf$p_alc_3 <- ifelse(exp$h602 %in% c(1:2), 1, NA)
expdf$p_alc_3 <- ifelse(exp$h602 %in% c(3:6), 2, expdf$p_alc_3)
expdf$p_alc_3 <- factor(expdf$p_alc_3, levels=c(1:2), labels=c('Never/<1 per week','1+ per week'))

expdf$p_alc_4 <- ifelse(exp$j650 %in% c(1:2), 1, NA)
expdf$p_alc_4 <- ifelse(exp$j650 %in% c(3:6), 2, expdf$p_alc_4)
expdf$p_alc_4 <- factor(expdf$p_alc_4, levels=c(1:2), labels=c('Never/Very occasionally','Occasionally/1+ per day'))

expdf$p_alc_6 <- ifelse(exp$l6190 %in% c(1:2), 1, NA)
expdf$p_alc_6 <- ifelse(exp$l6190 %in% c(3:6), 2, expdf$p_alc_6)
expdf$p_alc_6 <- factor(expdf$p_alc_6, levels=c(1:2), labels=c('Never/Occasionally','1+ per week'))

expdf$p_alc_9 <- ifelse(exp$p3190 %in% c(1:2), 1, NA)
expdf$p_alc_9 <- ifelse(exp$p3190 %in% c(3:6), 2, expdf$p_alc_9)
expdf$p_alc_9 <- factor(expdf$p_alc_9, levels=c(1:2), labels=c('Never/<1 per week','1+ per week'))

#Mother cannabis consumption in pregnancy
expdf$m_pre_pregcana_gest <- ifelse(exp$b700 %in% c(1:4), 1, NA)
expdf$m_pre_pregcana_gest <- ifelse(exp$b700 %in% 5, 2, expdf$m_pre_pregcana_gest)
expdf$m_pre_pregcana_gest <- factor(expdf$m_pre_pregcana_gest, levels=c(2:1), labels=c('No','Yes')) #(smoked cannabis in 6 months pre pregnancy)

expdf$m_f3m_pregcana_gest <- ifelse(exp$b701 %in% c(1:4), 1, NA)
expdf$m_f3m_pregcana_gest <- ifelse(exp$b701 %in% 5, 2, expdf$m_f3m_pregcana_gest)
expdf$m_f3m_pregcana_gest <- factor(expdf$m_f3m_pregcana_gest, levels=c(2:1), labels=c('No','Yes')) #(smoked cannabis in 1-3mths of preg)

expdf$m_3m_pregcana_gest <- ifelse(exp$b702 %in% c(1:4), 1, NA)
expdf$m_3m_pregcana_gest <- ifelse(exp$b702 %in% 5, 2, expdf$m_3m_pregcana_gest)
expdf$m_3m_pregcana_gest <- factor(expdf$m_3m_pregcana_gest, levels=c(2:1), labels=c('No','Yes')) #(smoked cannabis after 3 months into pregnancy)

expdf$m_l2m_pregcana_8w <- ifelse(exp$e190 %in% c(1:4), 1, NA)
expdf$m_l2m_pregcana_8w <- ifelse(exp$e190 %in% 5, 2, expdf$m_l2m_pregcana_8w)
expdf$m_l2m_pregcana_8w <- factor(expdf$m_l2m_pregcana_8w, levels=c(2:1), labels=c('No','Yes')) #(freq of gangja use last 2 months of preg)

#Mother cannabis consumption
expdf$m_cana_8w <- ifelse(exp$e192 %in% c(1:4), 1, NA)
expdf$m_cana_8w <- ifelse(exp$e192 %in% 5, 2, expdf$m_cana_8w)
expdf$m_cana_8w <- factor(expdf$m_cana_8w, levels=c(2:1), labels=c('No','Yes')) #(since birth)

expdf$m_cana_2 <- ifelse(exp$g047 %in% c(1:3), 1, NA)
expdf$m_cana_2 <- ifelse(exp$g047 %in% 4, 2, expdf$m_cana_2)
expdf$m_cana_2 <- factor(expdf$m_cana_2, levels=c(2:1), labels=c('No','Yes')) #(since child 8 months old)

expdf$m_cana_3 <- ifelse(exp$h037 %in% c(1:3), 1, NA)
expdf$m_cana_3 <- ifelse(exp$h037 %in% 4, 2, expdf$m_cana_3)
expdf$m_cana_3 <- factor(expdf$m_cana_3, levels=c(2:1), labels=c('No','Yes')) #(since child 18 months old)

expdf$m_cana_4 <- ifelse(exp$j042 %in% c(1:3), 1, NA)
expdf$m_cana_4 <- ifelse(exp$j042 %in% 4, 2, expdf$m_cana_4)
expdf$m_cana_4 <- factor(expdf$m_cana_4, levels=c(2:1), labels=c('No','Yes')) #(past year)

expdf$m_cana_5 <- ifelse(exp$k1042 %in% c(1:3), 1, NA)
expdf$m_cana_5 <- ifelse(exp$k1042 %in% 4, 2, expdf$m_cana_5)
expdf$m_cana_5 <- factor(expdf$m_cana_5, levels=c(2:1), labels=c('No','Yes')) #(past year)

expdf$m_cana_6 <- ifelse(exp$l3042 %in% c(1:3), 1, NA)
expdf$m_cana_6 <- ifelse(exp$l3042 %in% 4, 2, expdf$m_cana_6)
expdf$m_cana_6 <- factor(expdf$m_cana_6, levels=c(2:1), labels=c('No','Yes')) #(since child 5 years old)

expdf$m_cana_9 <- ifelse(exp$p1052 %in% c(1:3), 1, NA)
expdf$m_cana_9 <- ifelse(exp$p1052 %in% 4, 2, expdf$m_cana_9)
expdf$m_cana_9 <- factor(expdf$m_cana_9, levels=c(2:1), labels=c('No','Yes')) #(last 2 years)

expdf$m_cana_18 <- ifelse(exp$t5402 %in% c(1:3), 1, NA)
expdf$m_cana_18 <- ifelse(exp$t5402 %in% 4, 2, expdf$m_cana_18)
expdf$m_cana_18 <- factor(expdf$m_cana_18, levels=c(2:1), labels=c('No','Yes')) #(last 2 years)

#Mother other drugs in pregnancy
expdf$m_pregdrug_gest <- factor(exp$b714, levels=c(2:1), labels=c('No','Yes')) #(derived drugs during pregnancy b706-b713)
expdf$m_l2m_pregdrug_8w <- factor(exp$e203, levels=c(2:1), labels=c('No','Yes')) #(derived drugs last 2 months pregnancy e195-e202)

#Mother other drugs
expdf$m_drug_8w <- factor(exp$e213, levels=c(2:1), labels=c('No','Yes')) #(derived drugs since birth e205-e212)

expdf$m_drug_2 <- ifelse(exp$g053 %in% c(1:3) | exp$g056 %in% c(1:3), 1, NA)
expdf$m_drug_2 <- ifelse(is.na(expdf$m_drug_2) & (exp$g053 %in% 4 | exp$g056 %in% 4), 2, expdf$m_drug_2)
expdf$m_drug_2 <- factor(expdf$m_drug_2, levels=c(2:1), labels=c('No','Yes')) #(g053 = amphetamines, g056 = heroin meth cocaine, since child 8 months old used )

expdf$m_drug_3 <- ifelse(exp$h043 %in% c(1:3) | exp$h046 %in% c(1:3), 1, NA)
expdf$m_drug_3 <- ifelse(is.na(expdf$m_drug_3) & (exp$h043 %in% 4 | exp$h046 %in% 4), 2, expdf$m_drug_3)
expdf$m_drug_3 <- factor(expdf$m_drug_3, levels=c(2:1), labels=c('No','Yes')) #(h043 = amphetamines, h046 = herion, methadone, crack or cocaine, since child 18 months old)

expdf$m_drug_4 <- ifelse(exp$j048 %in% c(1:3) | exp$j051 %in% c(1:3), 1, NA)
expdf$m_drug_4 <- ifelse(is.na(expdf$m_drug_4) & (exp$j048 %in% 4 | exp$j051 %in% 4), 2, expdf$m_drug_4)
expdf$m_drug_4 <- factor(expdf$m_drug_4, levels=c(2:1), labels=c('No','Yes')) #(j048 = amphetamines, j051 = herion, past year)

expdf$m_drug_5 <- ifelse(exp$k1050 %in% c(1:3) | exp$k1053 %in% c(1:3), 1, NA)
expdf$m_drug_5 <- ifelse(is.na(expdf$m_drug_5) & (exp$k1050 %in% 4 | exp$k1053 %in% 4), 2, expdf$m_drug_5)
expdf$m_drug_5 <- factor(expdf$m_drug_5, levels=c(2:1), labels=c('No','Yes')) #(k1050 = amphetamines, k1053 = heroin/methadone/cocaine, past year)

expdf$m_drug_6 <- ifelse(exp$l3050 %in% c(1:3) | exp$l3053 %in% c(1:3), 1, NA)
expdf$m_drug_6 <- ifelse(is.na(expdf$m_drug_6) & (exp$l3050 %in% 4 | exp$l3053 %in% 4), 2, expdf$m_drug_6)
expdf$m_drug_6 <- factor(expdf$m_drug_6, levels=c(2:1), labels=c('No','Yes')) #(l3050 = amphetamines/other stimulants, l3053 = heroin, methadone, crack, cocaine, since child 5 years old)

expdf$m_drug_9 <- ifelse(exp$p1060 %in% c(1:3) | exp$p1063 %in% c(1:3), 1, NA)
expdf$m_drug_9 <- ifelse(is.na(expdf$m_drug_9) & (exp$p1060 %in% 4 | exp$p1063 %in% 4), 2, expdf$m_drug_9)
expdf$m_drug_9 <- factor(expdf$m_drug_9, levels=c(2:1), labels=c('No','Yes')) #(p1060 = amphetamines or other stimulants, p1063 = heroin, methadone, crack, cocaine, past 2 years)

expdf$m_drug_18 <- ifelse(exp$t5406 %in% c(1:3) | exp$t5410 %in% c(1:3) | exp$t5412 %in% c(1:3), 1, NA)
expdf$m_drug_18 <- ifelse(is.na(expdf$m_drug_18) & (exp$t5406 %in% 4 | exp$t5410 %in% 4 | exp$t5412 %in% 4), 2, expdf$m_drug_18)
expdf$m_drug_18 <- factor(expdf$m_drug_18, levels=c(2:1), labels=c('No','Yes')) #(t5406 = cocaine, t5410 = amphetamines, ecstasy or other stimulants, t5412 = heroin, methadone, crack or other hard drug, past 2 years)

#Mother alcoholism
expdf$m_alcprob_gest <- factor(exp$d168, levels=c(3:1), labels=c('No','Yes','Yes')) #ever
expdf$m_alcprob_5 <- factor(exp$k1022, levels=c(3:1), labels=c('No','Yes','Yes')) #past year
expdf$m_alcprob_6 <- factor(exp$l3022, levels=c(3:1), labels=c('No','Yes','Yes')) #since child 5 years old
expdf$m_alcprob_8 <- factor(exp$n1058, levels=c(3:1), labels=c('No','Yes','Yes')) #ever
expdf$m_alcprob_9 <- factor(exp$p1022, levels=c(3:1), labels=c('No','Yes','Yes')) #last 3 years
expdf$m_alcprob_12 <- factor(exp$s1022, levels=c(3:1), labels=c('No','Yes','Yes')) #last 2 years

#Partner alcoholism
expdf$p_alcprob_1 <- factor(exp$f527, levels=c(3:1), labels=c('No','Yes','Yes')) #since child born
expdf$p_alcprob_2 <- factor(exp$g613, levels=c(3:1), labels=c('No','Yes','Yes')) #and accessed doctor
expdf$p_alcprob_3 <- factor(exp$h498, levels=c(3:1), labels=c('No','Yes','Yes')) #since child 18 months old
expdf$p_alcprob_4 <- factor(exp$j616, levels=c(3:1), labels=c('No','Yes','Yes')) #past year
expdf$p_alcprob_6 <- factor(exp$l6032, levels=c(3:1), labels=c('No','Yes','Yes')) #since child 5 years old
expdf$p_alcprob_9 <- factor(exp$p3032, levels=c(3:1), labels=c('No','Yes','Yes')) #last 2 years
expdf$p_alcprob_12 <- factor(exp$s3032, levels=c(3:1), labels=c('No','Yes','Yes')) #since child 10 years old

#Mother drug addiction
expdf$m_drugprob_gest <- factor(exp$d167, levels=c(3:1), labels=c('No','Yes','Yes')) #ever
expdf$m_drugprob_8 <- factor(exp$n1057, levels=c(3:1), labels=c('No','Yes','Yes')) #ever

#Maternal grandparents alcohol problem
expdf$mgm_alcprob_gest <- ifelse(exp$d528 %in% 1 | exp$d529 %in% 1, 1, NA)
expdf$mgm_alcprob_gest <- ifelse(is.na(expdf$mgm_alcprob_gest) & (exp$d528 %in% 2 | exp$d529 %in% 2), 2, expdf$mgm_alcprob_gest)
expdf$mgm_alcprob_gest <- factor(expdf$mgm_alcprob_gest, levels=c(2:1), labels=c('No','Yes')) #natural mother or mother figure

expdf$mgf_alcprob_gest <- ifelse(exp$d578 %in% 1 | exp$d579 %in% 1, 1, NA)
expdf$mgf_alcprob_gest <- ifelse(is.na(expdf$mgf_alcprob_gest) & (exp$d578 %in% 2 | exp$d579 %in% 2), 2, expdf$mgf_alcprob_gest)
expdf$mgf_alcprob_gest <- factor(expdf$mgf_alcprob_gest, levels=c(2:1), labels=c('No','Yes')) #natural father or father figure

expdf$mgm_alcprob_8 <- factor(exp$n2009, levels=c(2:1), labels=c('No','Yes')) #natural mother
expdf$mgf_alcprob_8 <- factor(exp$n2029, levels=c(2:1), labels=c('No','Yes')) #natural father

#Maternal grandparents death addiction to alcohol or drugs
expdf$mgm_alcprob_25 <- factor(exp$W2279, levels=c(0:1), labels=c('No','Yes')) #mgm cause of death said to be addiction to alcohol or drugs
expdf$mgf_alcprob_25 <- factor(exp$W3279, levels=c(0:1), labels=c('No','Yes')) #mgf cause of death said to be addiction to alcohol or drugs
expdf$mgmm_alcprob_25 <- factor(exp$W4279, levels=c(0:1), labels=c('No','Yes'))
expdf$mgmf_alcprob_25 <- factor(exp$W5279, levels=c(0:1), labels=c('No','Yes'))
expdf$mgfm_alcprob_25 <- factor(exp$W6279, levels=c(0:1), labels=c('No','Yes'))
expdf$mgff_alcprob_25 <- factor(exp$W7279, levels=c(0:1), labels=c('No','Yes'))


##Familial mental health
#Edinburgh Post-natal Depression scores (EPDS) (Mothers who score above 12/13 are likely to be suffering from a depressive illness)
expdf$m_edps_gest <- as.numeric(exp$b370)
expdf$m_edps_gest[expdf$m_edps_gest<0] <- NA
expdf$m_edps_gest_gr <- factor(ifelse(expdf$m_edps_gest > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

expdf$m_edps_8w <- as.numeric(exp$e391)
expdf$m_edps_8w[expdf$m_edps_8w<0] <- NA
expdf$m_edps_8w_gr <- factor(ifelse(expdf$m_edps_8w > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

expdf$m_edps_1 <- as.numeric(exp$f200)
expdf$m_edps_1[expdf$m_edps_1<0] <- NA
expdf$m_edps_1_gr <- factor(ifelse(expdf$m_edps_1 > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

expdf$m_edps_2 <- as.numeric(exp$g290)
expdf$m_edps_2[expdf$m_edps_2<0] <- NA
expdf$m_edps_2_gr <- factor(ifelse(expdf$m_edps_2 > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

expdf$m_edps_3 <- as.numeric(exp$h200a) #complete cases
expdf$m_edps_3[expdf$m_edps_3<0] <- NA
expdf$m_edps_3_gr <- factor(ifelse(expdf$m_edps_3 > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

expdf$m_edps_28 <- as.numeric(exp$Y4095)
expdf$m_edps_28[expdf$m_edps_28<0] <- NA
expdf$m_edps_28_gr <- factor(ifelse(expdf$m_edps_28 > 12, 1, 0), levels=c(0:1), labels=c('<=12','>12'))

#Mother experienced a mental health problem
expdf$m_mhp_gest <- ifelse(exp$d169 %in% c(1:2) | exp$d170 %in% c(1:2) | exp$d171 %in% c(1:2) | exp$d172 %in% c(1:2), 1, NA)
expdf$m_mhp_gest <- ifelse(is.na(expdf$m_mhp_gest) & exp$d169 %in% 3 & exp$d170 %in% 3 & exp$d171 %in% 3 & exp$d172 %in% 3, 2, expdf$m_mhp_gest) 
expdf$m_mhp_gest <- factor(expdf$m_mhp_gest, levels=c(2:1), labels=c('No','Yes')) #ever had schizophrenia, anorexia nervosa, severe depression or other psychiatric problem

expdf$m_mhp_1 <- ifelse(exp$f020 %in% c(1:2) | exp$f021 %in% c(1:2), 1, NA)
expdf$m_mhp_1 <- ifelse(is.na(expdf$m_mhp_1) & exp$f020 %in% 3 & exp$f021 %in% 3, 2, expdf$m_mhp_1) 
expdf$m_mhp_1 <- factor(expdf$m_mhp_1, levels=c(2:1), labels=c('No','Yes')) #since birth had anxiety or depression

expdf$m_mhp_2 <- ifelse(exp$g020 %in% c(1:2) | exp$g021 %in% c(1:2), 1, NA)
expdf$m_mhp_2 <- ifelse(is.na(expdf$m_mhp_2) & exp$g020 %in% 3 & exp$g021 %in% 3, 2, expdf$m_mhp_2) 
expdf$m_mhp_2 <- factor(expdf$m_mhp_2, levels=c(2:1), labels=c('No','Yes')) #since child 8 months old had anxiety (or nerves) or depression

expdf$m_mhp_3 <- ifelse(exp$h012 %in% c(1:2) | exp$h013 %in% c(1:2), 1, NA)
expdf$m_mhp_3 <- ifelse(is.na(expdf$m_mhp_3) & exp$h012 %in% 3 & exp$h013 %in% 3, 2, expdf$m_mhp_3) 
expdf$m_mhp_3 <- factor(expdf$m_mhp_3, levels=c(2:1), labels=c('No','Yes')) #since child 18 months old had anxiety (or nerves) or depression

expdf$m_mhp_4 <- ifelse(exp$j011 %in% c(1:2) | exp$j012 %in% c(1:2), 1, NA)
expdf$m_mhp_4 <- ifelse(is.na(expdf$m_mhp_4) & exp$j011 %in% 3 & exp$j012 %in% 3, 2, expdf$m_mhp_4) 
expdf$m_mhp_4 <- factor(expdf$m_mhp_4, levels=c(2:1), labels=c('No','Yes')) #past year had anxiety or depression

expdf$m_mhp_5 <- ifelse(exp$k1010 %in% c(1:2) | exp$k1011 %in% c(1:2) | exp$k1020 %in% c(1:2), 1, NA)
expdf$m_mhp_5 <- ifelse(is.na(expdf$m_mhp_5) & exp$k1010 %in% 3 & exp$k1011 %in% 3 & exp$k1020 %in% 3, 2, expdf$m_mhp_5) 
expdf$m_mhp_5 <- factor(expdf$m_mhp_5, levels=c(2:1), labels=c('No','Yes')) #past year had anxiety (or nerves), depression or schizophrenia

expdf$m_mhp_6 <- ifelse(exp$l3010 %in% c(1:2) | exp$l3011 %in% c(1:2) | exp$l3020 %in% c(1:2), 1, NA)
expdf$m_mhp_6 <- ifelse(is.na(expdf$m_mhp_6) & exp$l3010 %in% 3 & exp$l3011 %in% 3 & exp$l3020 %in% 3, 2, expdf$m_mhp_6) 
expdf$m_mhp_6 <- factor(expdf$m_mhp_6, levels=c(2:1), labels=c('No','Yes')) #since child 5 years old had/continued to have anxiety/nerves, depression or schizophrenia

expdf$m_mhp_8 <- ifelse(exp$n1059 %in% c(1:2) | exp$n1060 %in% c(1:2) | exp$n1061 %in% c(1:2) | exp$n1062 %in% c(1:2), 1, NA)
expdf$m_mhp_8 <- ifelse(is.na(expdf$m_mhp_8) & exp$n1059 %in% 3 & exp$n1060 %in% 3 & exp$n1061 %in% 3 & exp$n1062 %in% 3, 2, expdf$m_mhp_8) 
expdf$m_mhp_8 <- factor(expdf$m_mhp_8, levels=c(2:1), labels=c('No','Yes')) #ever had schizophrenia, anorexia nervosa, severe depression or other psychiatric problem

expdf$m_mhp_9 <- ifelse(exp$p1010 %in% c(1:2) | exp$p1011 %in% c(1:2) | exp$p1020 %in% c(1:2), 1, NA)
expdf$m_mhp_9 <- ifelse(is.na(expdf$m_mhp_9) & exp$p1010 %in% 3 & exp$p1011 %in% 3 & exp$p1020 %in% 3, 2, expdf$m_mhp_9) 
expdf$m_mhp_9 <- factor(expdf$m_mhp_9, levels=c(2:1), labels=c('No','Yes')) #last 3 years had anxiety (or nerves), depression or schizophrenia

expdf$m_mhp_11 <- ifelse(exp$r2019 %in% c(1:2) | exp$r2020 %in% c(1:2) | exp$r2021 %in% c(1:2) | exp$r2022 %in% c(1:2), 1, NA)
expdf$m_mhp_11 <- ifelse(is.na(expdf$m_mhp_11) & exp$r2019 %in% 3 & exp$r2020 %in% 3 & exp$r2021 %in% 3 & exp$r2022 %in% 3, 2, expdf$m_mhp_11) 
expdf$m_mhp_11 <- factor(expdf$m_mhp_11, levels=c(2:1), labels=c('No','Yes')) #ever schizophrenia, anorexia nervosa, severe depression or other psychiatric problem

expdf$m_mhp_12 <- ifelse(exp$s1010 %in% c(1:2) | exp$s1011 %in% c(1:2) | exp$s1020 %in% c(1:2), 1, NA)
expdf$m_mhp_12 <- ifelse(is.na(expdf$m_mhp_12) & exp$s1010 %in% 3 & exp$s1011 %in% 3 & exp$s1020 %in% 3, 2, expdf$m_mhp_12) 
expdf$m_mhp_12 <- factor(expdf$m_mhp_12, levels=c(2:1), labels=c('No','Yes')) #last 2 years had anxiety (or nerves), depression or schizophrenia

#Mother felt depressed and if more good days than bad
expdf$m_feltdepr_8w <- ifelse(exp$e296 %in% c(1:2), 1, NA)
expdf$m_feltdepr_8w <- ifelse(exp$e296 %in% 3, 2, expdf$m_feltdepr_8w) 
expdf$m_feltdepr_8w <- factor(expdf$m_feltdepr_8w, levels=c(2:1), labels=c('No','Yes')) #since birth

expdf$m_pwk_feltdepr_8w <- ifelse(exp$e393 %in% c(2:4), 1, NA)
expdf$m_pwk_feltdepr_8w <- ifelse(exp$e393 %in% 1, 2, expdf$m_pwk_feltdepr_8w) 
expdf$m_pwk_feltdepr_8w <- factor(expdf$m_pwk_feltdepr_8w, levels=c(2:1), labels=c('No','Yes')) #past week

expdf$m_feltdepr_2 <- ifelse(exp$g120 %in% c(1:3), 1, NA)
expdf$m_feltdepr_2 <- ifelse(exp$g120 %in% 4, 2, expdf$m_feltdepr_2) 
expdf$m_feltdepr_2 <- factor(expdf$m_feltdepr_2, levels=c(2:1), labels=c('No','Yes')) #past month freq felt depressed

expdf$m_pwk_gooddays_8w <- ifelse(exp$e394 %in% 1, 1, NA)
expdf$m_pwk_gooddays_8w <- ifelse(exp$e394 %in% c(2:3), 2, expdf$m_pwk_gooddays_8w) 
expdf$m_pwk_gooddays_8w <- factor(expdf$m_pwk_gooddays_8w, levels=c(1:2), labels=c('More good days','Half & half or more bad days')) #past week

expdf$m_pwk_gooddays_1 <- ifelse(exp$f205 %in% 1, 1, NA)
expdf$m_pwk_gooddays_1 <- ifelse(exp$f205 %in% c(2:3), 2, expdf$m_pwk_gooddays_1) 
expdf$m_pwk_gooddays_1 <- factor(expdf$m_pwk_gooddays_1, levels=c(1:2), labels=c('More good days','Half & half or more bad days')) #past week

#Mother takes medication for mental health problem
expdf$m_pregmhmeds_gest <- ifelse(exp$b106 %in% c(1:3) | exp$b122 %in% c(1:3), 1, NA)
expdf$m_pregmhmeds_gest <- ifelse(is.na(expdf$m_pregmhmeds_gest) & exp$b106 %in% 4 & exp$b122 %in% 4, 2, expdf$m_pregmhmeds_gest) 
expdf$m_pregmhmeds_gest <- factor(expdf$m_pregmhmeds_gest, levels=c(2:1), labels=c('No','Yes')) #meds for anxiety or depression this pregnancy

expdf$m_mhmeds_8w <- factor(exp$e327, levels=c(2:1), labels=c('No','Yes')) #anti-depressant since birth

expdf$m_mhmeds_1 <- ifelse(exp$f063 %in% c(1:3), 1, NA)
expdf$m_mhmeds_1 <- ifelse(exp$f063 %in% 4, 2, expdf$m_mhmeds_1) 
expdf$m_mhmeds_1 <- factor(expdf$m_mhmeds_1, levels=c(2:1), labels=c('No','Yes')) #anti-depressant since birth

expdf$m_mhmeds_2 <- ifelse(exp$g049 %in% c(1:3), 1, NA)
expdf$m_mhmeds_2 <- ifelse(exp$g049 %in% 4, 2, expdf$m_mhmeds_2) 
expdf$m_mhmeds_2 <- factor(expdf$m_mhmeds_2, levels=c(2:1), labels=c('No','Yes')) #depression pills since child 8 months old

expdf$m_mhmeds_3 <- ifelse(exp$h039 %in% c(1:3), 1, NA)
expdf$m_mhmeds_3 <- ifelse(exp$h039 %in% 4, 2, expdf$m_mhmeds_3) 
expdf$m_mhmeds_3 <- factor(expdf$m_mhmeds_3, levels=c(2:1), labels=c('No','Yes')) #taken pills for depression since child 18 months old

expdf$m_mhmeds_4 <- ifelse(exp$j044 %in% c(1:3), 1, NA)
expdf$m_mhmeds_4 <- ifelse(exp$j044 %in% 4, 2, expdf$m_mhmeds_4) 
expdf$m_mhmeds_4 <- factor(expdf$m_mhmeds_4, levels=c(2:1), labels=c('No','Yes')) #took depression pills past year

expdf$m_mhmeds_5 <- ifelse(exp$k1044 %in% c(1:3), 1, NA)
expdf$m_mhmeds_5 <- ifelse(exp$k1044 %in% 4, 2, expdf$m_mhmeds_5) 
expdf$m_mhmeds_5 <- factor(expdf$m_mhmeds_5, levels=c(2:1), labels=c('No','Yes')) #took depression pills past year

expdf$m_mhmeds_6 <- ifelse(exp$l3044 %in% c(1:3), 1, NA)
expdf$m_mhmeds_6 <- ifelse(exp$l3044 %in% 4, 2, expdf$m_mhmeds_6) 
expdf$m_mhmeds_6 <- factor(expdf$m_mhmeds_6, levels=c(2:1), labels=c('No','Yes')) #taken pills for depression since child 5 years old

expdf$m_mhmeds_9 <- ifelse(exp$p1054 %in% c(1:3), 1, NA)
expdf$m_mhmeds_9 <- ifelse(exp$p1054 %in% 4, 2, expdf$m_mhmeds_9) 
expdf$m_mhmeds_9 <- factor(expdf$m_mhmeds_9, levels=c(2:1), labels=c('No','Yes')) #freq taken pills for depression last two years

expdf$m_mhmeds_10 <- ifelse(exp$q4100 %in% 1 | exp$q4110 %in% 1, 1, NA)
expdf$m_mhmeds_10 <- ifelse(is.na(expdf$m_mhmeds_10) & (exp$q4100 %in% -1 & exp$q4110 %in% -1), 2, expdf$m_mhmeds_10) 
expdf$m_mhmeds_10 <- factor(expdf$m_mhmeds_10, levels=c(2:1), labels=c('No','Yes')) #past year used medicine for depression or anxiety/nerves

expdf$m_mhmeds_12 <- ifelse(exp$s4100 %in% 1 | exp$s4110 %in% 1, 1, NA)
expdf$m_mhmeds_12 <- ifelse(is.na(expdf$m_mhmeds_12) & (exp$s4100 %in% -1 & exp$s4110 %in% -1), 2, expdf$m_mhmeds_12) 
expdf$m_mhmeds_12 <- factor(expdf$m_mhmeds_12, levels=c(2:1), labels=c('No','Yes')) #past year taken medication for depression or anxiety/nerves

expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% c(1:3), 1, NA)
expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% 4, 2, expdf$m_mhmeds_18) 
expdf$m_mhmeds_18 <- factor(expdf$m_mhmeds_18, levels=c(2:1), labels=c('No','Yes')) #taken pills for depression last two years

#Partner has mental health problem
expdf$p_mhp_1 <- ifelse(exp$f518 %in% c(1:2) | exp$f519 %in% c(1:2) | exp$f526 %in% c(1:2), 1, NA)
expdf$p_mhp_1 <- ifelse(is.na(expdf$p_mhp_1) & exp$f518 %in% 3 & exp$f519 %in% 3 & exp$f526 %in% 3, 2, expdf$p_mhp_1) 
expdf$p_mhp_1 <- factor(expdf$p_mhp_1, levels=c(2:1), labels=c('No','Yes')) #since birth partner had depression, anxiety or schizophrenia

expdf$p_mhp_3 <- ifelse(exp$h489 %in% c(1:2) | exp$h490 %in% c(1:2), 1, NA)
expdf$p_mhp_3 <- ifelse(is.na(expdf$p_mhp_3) & exp$h489 %in% 3 & exp$h490 %in% 3, 2, expdf$p_mhp_3) 
expdf$p_mhp_3 <- factor(expdf$p_mhp_3, levels=c(2:1), labels=c('No','Yes')) #since child 18 months old partner had depression or anxiety

expdf$p_mhp_4 <- ifelse(exp$j607 %in% c(1:2) | exp$j608 %in% c(1:2), 1, NA)
expdf$p_mhp_4 <- ifelse(is.na(expdf$p_mhp_4) & exp$j607 %in% 3 & exp$j608 %in% 3, 2, expdf$p_mhp_4) 
expdf$p_mhp_4 <- factor(expdf$p_mhp_4, levels=c(2:1), labels=c('No','Yes')) #past year partner had depression or anxiety

expdf$p_mhp_6 <- ifelse(exp$l6023 %in% c(1:2) | exp$l6024 %in% c(1:2) | exp$l6031 %in% c(1:2), 1, NA)
expdf$p_mhp_6 <- ifelse(is.na(expdf$p_mhp_6) & exp$l6023 %in% 3 & exp$l6024 %in% 3 & exp$l6031 %in% 3, 2, expdf$p_mhp_6) 
expdf$p_mhp_6 <- factor(expdf$p_mhp_6, levels=c(2:1), labels=c('No','Yes')) #since child 5 years old partner had depression, anxiety/nerves or schizophrenia

expdf$p_mhp_9 <- ifelse(exp$p3023 %in% c(1:2) | exp$p3024 %in% c(1:2), 1, NA)
expdf$p_mhp_9 <- ifelse(is.na(expdf$p_mhp_9) & exp$p3023 %in% 3 & exp$p3024 %in% 3, 2, expdf$p_mhp_9) 
expdf$p_mhp_9 <- factor(expdf$p_mhp_9, levels=c(2:1), labels=c('No','Yes')) #last 2 years partner has had depression or anxiety

expdf$p_mhp_12 <- ifelse(exp$s3023 %in% c(1:2) | exp$s3024 %in% c(1:2) | exp$s3031 %in% c(1:2), 1, NA)
expdf$p_mhp_12 <- ifelse(is.na(expdf$p_mhp_12) & exp$s3023 %in% 3 & exp$s3024 %in% 3 & exp$s3031 %in% 3, 2, expdf$p_mhp_12) 
expdf$p_mhp_12 <- factor(expdf$p_mhp_12, levels=c(2:1), labels=c('No','Yes')) #since child 10 years old depression, anxiety/nerves or schizophrenia

#Maternal grandparents had mental health problem
expdf$mgm_mhp_gest <- ifelse(exp$d530 %in% 1 | exp$d531 %in% 1 | exp$d536 %in% 1 | exp$d537 %in% 1, 1, NA)
expdf$mgm_mhp_gest <- ifelse(is.na(expdf$mgm_mhp_gest) & (exp$d530 %in% 2 | exp$d531 %in% 2) & (exp$d536 %in% 2 | exp$d537 %in% 2), 2, expdf$mgm_mhp_gest)
expdf$mgm_mhp_gest <- factor(expdf$mgm_mhp_gest, levels=c(2:1), labels=c('No','Yes')) #natural mother or mother figure had schizophrenia or depression or nerves

expdf$mgf_mhp_gest <- ifelse(exp$d580 %in% 1 | exp$d581 %in% 1 | exp$d586 %in% 1 | exp$d587 %in% 1, 1, NA)
expdf$mgf_mhp_gest <- ifelse(is.na(expdf$mgf_mhp_gest) & (exp$d580 %in% 2 | exp$d581 %in% 2) & (exp$d586 %in% 2 | exp$d587 %in% 2), 2, expdf$mgf_mhp_gest)
expdf$mgf_mhp_gest <- factor(expdf$mgf_mhp_gest, levels=c(2:1), labels=c('No','Yes')) #natural father or father figure had schizophrenia or depression or nerves

expdf$mgm_mhp_8 <- ifelse(exp$n2010 %in% 1 | exp$n2013 %in% 1, 1, NA)
expdf$mgm_mhp_8 <- ifelse(is.na(expdf$mgm_mhp_8) & (exp$n2010 %in% 2 | exp$n2013 %in% 2), 2, expdf$mgm_mhp_8)
expdf$mgm_mhp_8 <- factor(expdf$mgm_mhp_8, levels=c(2:1), labels=c('No','Yes')) #natural mother had schizophrenia or depression or nerves

expdf$mgf_mhp_8 <- ifelse(exp$n2030 %in% 1 | exp$n2033 %in% 1, 1, NA)
expdf$mgf_mhp_8 <- ifelse(is.na(expdf$mgf_mhp_8) & (exp$n2030 %in% 2 | exp$n2033 %in% 2), 2, expdf$mgf_mhp_8)
expdf$mgf_mhp_8 <- factor(expdf$mgf_mhp_8, levels=c(2:1), labels=c('No','Yes')) #natural father had schizophrenia or depression or nerves

#Maternal grandparents death cause mental health or suicide
expdf$mgm_mhpdeath_25 <- ifelse(exp$W2278 %in% 1 | exp$W2295 %in% 1, 1, NA)
expdf$mgm_mhpdeath_25 <- ifelse(is.na(expdf$mgm_mhpdeath_25) & (exp$W2278 %in% 0 & exp$W2295 %in% 0), 2, expdf$mgm_mhpdeath_25)
expdf$mgm_mhpdeath_25 <- factor(expdf$mgm_mhpdeath_25, levels=c(2:1), labels=c('No','Yes')) #mgm cause of death said to be suicide or mental health problem

expdf$mgf_mhpdeath_25 <- ifelse(exp$W3278 %in% 1 | exp$W3294 %in% 1, 1, NA)
expdf$mgf_mhpdeath_25 <- ifelse(is.na(expdf$mgf_mhpdeath_25) & (exp$W3278 %in% 0 & exp$W3294 %in% 0), 2, expdf$mgf_mhpdeath_25)
expdf$mgf_mhpdeath_25 <- factor(expdf$mgf_mhpdeath_25, levels=c(2:1), labels=c('No','Yes')) #mgf cause of death said to be suicide or mental health problem

expdf$mgmm_mhpdeath_25 <- ifelse(exp$W4278 %in% 1 | exp$W4295 %in% 1, 1, NA)
expdf$mgmm_mhpdeath_25 <- ifelse(is.na(expdf$mgmm_mhpdeath_25) & (exp$W4278 %in% 0 & exp$W4295 %in% 0), 2, expdf$mgmm_mhpdeath_25)
expdf$mgmm_mhpdeath_25 <- factor(expdf$mgmm_mhpdeath_25, levels=c(2:1), labels=c('No','Yes'))

expdf$mgmf_mhpdeath_25 <- ifelse(exp$W5278 %in% 1 | exp$W5294 %in% 1, 1, NA)
expdf$mgmf_mhpdeath_25 <- ifelse(is.na(expdf$mgmf_mhpdeath_25) & (exp$W5278 %in% 0 & exp$W5294 %in% 0), 2, expdf$mgmf_mhpdeath_25)
expdf$mgmf_mhpdeath_25 <- factor(expdf$mgmf_mhpdeath_25, levels=c(2:1), labels=c('No','Yes'))

expdf$mgfm_mhpdeath_25 <- ifelse(exp$W6278 %in% 1 | exp$W6295 %in% 1, 1, NA)
expdf$mgfm_mhpdeath_25 <- ifelse(is.na(expdf$mgfm_mhpdeath_25) & (exp$W6278 %in% 0 & exp$W6295 %in% 0), 2, expdf$mgfm_mhpdeath_25)
expdf$mgfm_mhpdeath_25 <- factor(expdf$mgfm_mhpdeath_25, levels=c(2:1), labels=c('No','Yes'))

expdf$mgff_mhpdeath_25 <- ifelse(exp$W7278 %in% 1 | exp$W7294 %in% 1, 1, NA)
expdf$mgff_mhpdeath_25 <- ifelse(is.na(expdf$mgff_mhpdeath_25) & (exp$W7278 %in% 0 & exp$W7294 %in% 0), 2, expdf$mgff_mhpdeath_25)
expdf$mgff_mhpdeath_25 <- factor(expdf$mgff_mhpdeath_25, levels=c(2:1), labels=c('No','Yes'))


##Peer substance use
#Friends smoke, drink, offered drugs
expdf$friends_smk_10 <- factor(exp$fdaa480, levels=c(2:1), labels=c('No','Yes')) #friends have smoked cigarettes (F10 clinic)
expdf$friends_smk_14 <- factor(exp$fg4820, levels=c(2:1), labels=c('No','Yes')) #(TF2 clinic)
expdf$friends_smk_16 <- factor(exp$fh8340, levels=c(1:3), labels=c('None/some','None/some','Most/all')) #past year (TF3 clinic)
expdf$friends_smk_18 <- factor(exp$FJAA3300, levels=c(1:3), labels=c('None/some','None/some','Most/all')) #past year number of YPs friends that smoked cigarettes (TF4)
expdf$friends_smk_20 <- factor(exp$CCU3201, levels=c(1:5), labels=c('None/some','None/some','None/some','Most/all','Most/all')) #between ages 18 and 21 number of friends who have smoked

expdf$friends_alc_10 <- factor(exp$fdaa490, levels=c(2:1), labels=c('No','Yes')) #friends drunk alcohol
expdf$friends_alc_13 <- factor(exp$ff7000, levels=c(2:1), labels=c('No','Yes')) #without permission (TF1 clinic)
expdf$friends_alc_14 <- factor(exp$fg4870, levels=c(2:1), labels=c('No','Yes')) #without permission
expdf$friends_alc_16 <- factor(exp$fh8341, levels=c(1:3), labels=c('None/some','None/some','Most/all')) #past year number of YPs friends that drank alcohol
expdf$friends_alc_18 <- factor(exp$FJAA3350, levels=c(1:3), labels=c('None/some','None/some','Most/all')) #past year
expdf$friends_alc_20 <- factor(exp$CCU3204, levels=c(1:5), labels=c('None/some','None/some','None/some','Most/all','Most/all')) #between ages of 18 and 21, number of friends who would have drunk alcohol
expdf$friends_drunk_20 <- factor(exp$CCU3202, levels=c(1:5), labels=c('None/some','None/some','None/some','Most/all','Most/all')) #between ages of 18 and 21, number of friends who would have got drunk
expdf$friends_alcprob_20 <- factor(exp$CCU3203, levels=c(1:5), labels=c('None/some','None/some','None/some','Most/all','Most/all')) #between ages of 18 and 21, number of friends who would have had problems with alcohol

expdf$friends_cana_10 <- factor(exp$fdaa520, levels=c(2:1), labels=c('No','Yes')) #friends smoked cannabis
expdf$friends_cana_13 <- factor(exp$ff7720, levels=c(2:1), labels=c('No','Yes'))
expdf$friends_cana_14 <- factor(exp$fg5420, levels=c(2:1), labels=c('No','Yes'))
expdf$friends_cana_20 <- factor(exp$CCU3207, levels=c(1:5), labels=c('None/some','None/some','None/some','Most/all','Most/all')) #between ages of 18 and 21, number of friends who would have had problems with alcohol

expdf$friends_offdrug_10 <- factor(exp$fdaa500, levels=c(2:1), labels=c('No','Yes')) #friends offered drugs
expdf$friends_offdrug_13 <- factor(exp$ff8000, levels=c(2:1), labels=c('No','Yes'))
expdf$friends_offdrug_14 <- factor(exp$fg5480, levels=c(2:1), labels=c('No','Yes'))
expdf$friends_drug_16 <- factor(exp$fh8342, levels=c(1:3), labels=c('No','Yes','Yes')) #past year number of friends that took illegal drugs
expdf$friends_drug_18 <- factor(exp$FJAA3400, levels=c(1:3), labels=c('No','Yes','Yes'))#past year number of friends that took illegal drugs
expdf$friends_drug_20 <- ifelse(exp$CCU3208 %in% c(5:2) | exp$CCU3209 %in% c(5:2), 1, NA)
expdf$friends_drug_20 <- ifelse(is.na(expdf$friends_drug_20) & (exp$CCU3208 %in% 1 & exp$CCU3209 %in% 1), 2, expdf$friends_drug_20)
expdf$friends_drug_20 <- factor(expdf$friends_drug_20, levels=c(2:1), labels=c('No','Yes')) #between ages of 18 and 21, number of friends who would have used inhalants or other drugs like cocaine, downers, ecstasy or LSD


##Other substance use
expdf$eversmk_8 <- factor(exp$f8aa108, levels=c(2:1), labels=c('No','Yes')) #ever tried cigs
expdf$eversmk_10 <- factor(exp$fdaa482, levels=c(2:1), labels=c('No','Yes')) #smoked cigs

expdf$everalc_8 <- factor(exp$f8aa107, levels=c(2:1), labels=c('No','Yes')) #drunk alcohol without permission
expdf$everalc_10 <- factor(exp$fdaa492, levels=c(2:1), labels=c('No','Yes')) #drunk alcohol
expdf$everalc_13 <- factor(exp$ff7011, levels=c(2:1), labels=c('No','Yes')) #without permission
expdf$everalc_14 <- factor(exp$fg4872, levels=c(2:1), labels=c('No','Yes')) #without permission

expdf$alc_16 <- ifelse(exp$fh8510 %in% 2 | exp$fh8511 %in% c(1:3), 1, NA)
expdf$alc_16 <- ifelse(exp$fh8511 %in% c(4:6), 2, expdf$alc_16)
expdf$alc_16 <- factor(expdf$alc_16, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption past year

expdf$alc_17 <- ifelse(exp$ccs3500 %in% 2 | exp$ccs3540 %in% c(1:3), 1, NA)
expdf$alc_17 <- ifelse(exp$ccs3540 %in% c(4:5), 2, expdf$alc_17)
expdf$alc_17 <- factor(expdf$alc_17, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption past year

expdf$alc_C_18 <- ifelse(exp$FJAL050 %in% 2 | exp$FJAL1000 %in% c(1:3), 1, NA)
expdf$alc_C_18 <- ifelse(exp$FJAL1000 %in% c(4:5), 2, expdf$alc_C_18)
expdf$alc_C_18 <- factor(expdf$alc_C_18, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption

expdf$alc_Q_18 <- ifelse(exp$cct5020 %in% 2 | exp$cct5030 %in% c(1:3), 1, NA)
expdf$alc_Q_18 <- ifelse(exp$cct5030 %in% c(4:5), 2, expdf$alc_Q_18)
expdf$alc_Q_18 <- factor(expdf$alc_Q_18, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption

expdf$alc_20 <- ifelse(exp$CCU3050 %in% 2 | exp$CCU3100 %in% c(1:3), 1, NA)
expdf$alc_20 <- ifelse(exp$CCU3100 %in% c(4:5), 2, expdf$alc_20)
expdf$alc_20 <- factor(expdf$alc_20, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption past year

expdf$alc_22 <- ifelse(exp$YPB4130 %in% 2 | exp$YPB4150 %in% c(1:3), 1, NA)
expdf$alc_22 <- ifelse(exp$YPB4150 %in% c(4:5), 2, expdf$alc_22)
expdf$alc_22 <- factor(expdf$alc_22, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption past year

expdf$alc_24 <- ifelse(exp$FKAL1010 %in% 0 | exp$FKAL1020 %in% c(0:2), 1, NA)
expdf$alc_24 <- ifelse(exp$FKAL1020 %in% c(3:5), 2, expdf$alc_24)
expdf$alc_24 <- factor(expdf$alc_24, levels=c(1:2), labels=c('Less than weekly','More than weekly')) ## whole drink/Frequency of alcohol consumption past year

expdf$alc_28 <- ifelse(exp$YPH5660 %in% c(1:3), 1, NA)
expdf$alc_28 <- ifelse(exp$YPH5660 %in% c(4:5), 2, expdf$alc_28)
expdf$alc_28 <- factor(expdf$alc_28, levels=c(1:2), labels=c('Less than weekly','More than weekly')) #past 12 months


expdf$evercana_10 <- factor(exp$fdaa522, levels=c(2:1), labels=c('No','Yes')) #child smoked cannabis
expdf$evercana_13 <- factor(exp$ff7750, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_C_14 <- factor(exp$fg5423, levels=c(2:1), labels=c('No','Yes')) #past 6 months
expdf$evercana_Q_14 <- factor(exp$ccr760, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_16 <- factor(exp$fh8610, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_17 <- factor(exp$ccs4060, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_C_18 <- factor(exp$FJDR050, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_Q_18 <- factor(exp$cct5050, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_20 <- factor(exp$CCU3300, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_22 <- factor(exp$YPB4390, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_24 <- factor(exp$FKCA1010, levels=c(0:1), labels=c('No','Yes')) #ever
expdf$evercana_28 <- factor(exp$YPH5650, levels=c(6:1), labels=c('No',rep('Yes',5))) #past 12 months


expdf$everdrunk_11 <- factor(exp$febp191, levels=c(2:1), labels=c('No','Yes')) #got really drunk on alcohol
expdf$everhigh_11 <- factor(exp$febp193, levels=c(2:1), labels=c('No','Yes')) #used drugs to get high
expdf$everhigh_14 <- factor(exp$fg5520, levels=c(2:1), labels=c('No','Yes')) #used drugs (other than cannabis) to get high

expdf$everoffdrug_10 <- factor(exp$fdaa510, levels=c(2:1), labels=c('No','Yes'))
expdf$everoffdrug_13 <- factor(exp$ff8100, levels=c(2:1), labels=c('No','Yes'))
expdf$everoffdrug_14 <- factor(exp$fg5500, levels=c(2:1), labels=c('No','Yes')) 

expdf$everdrug_13 <- factor(exp$ff8170, levels=c(2:1), labels=c('No','Yes')) #used drugs other than cannabis

expdf$everdrug_14 <- ifelse(exp$ccr840 %in% c(1:2) | exp$ccr841 %in% c(1:2) | exp$ccr842 %in% c(1:2) | exp$ccr843 %in% c(1:2) | exp$ccr844 %in% c(1:2) | exp$ccr850 %in% c(1:2) | exp$ccr851 %in% c(1:2) | exp$ccr852 %in% c(1:2) | exp$ccr853 %in% c(1:2) | exp$ccr855 %in% c(1:2) | exp$ccr856 %in% c(1:2) | exp$ccr857 %in% c(1:2), 1, NA) #respondent has tried aerosols, gas, glue, solvents, poppers, amphetamines, ecstacy, LSD, magic mushrooms, cocaine, crack, heroin
expdf$everdrug_14 <- ifelse(is.na(expdf$everdrug_14) & (exp$ccr840 %in% 3 & exp$ccr841 %in% 3  & exp$ccr842 %in% 3 & exp$ccr843 %in% 3 & exp$ccr844 %in% 3 & exp$ccr850 %in% 3 & exp$ccr851 %in% 3 & exp$ccr852 %in% 3 & exp$ccr853 %in% 3 & exp$ccr855 %in% 3 & exp$ccr856 %in% 3 & exp$ccr857 %in% 3), 2, expdf$everdrug_14)
expdf$everdrug_14 <- factor(expdf$everdrug_14, levels=c(2:1), labels=c('No','Yes')) #respondent has tried aerosols, gas, glue, solvents, poppers, amphetamines, ecstacy, LSD, magic mushrooms, cocaine, crack, heroin

expdf$drug_16 <- ifelse(exp$fh8700 %in% 1 | exp$fh8701 %in% 1 | exp$fh8702 %in% 1 | exp$fh8703 %in% 1 | exp$fh8704 %in% 1 | exp$fh8705 %in% 1 | exp$fh8706 %in% 1 | exp$fh8707 %in% 1 | exp$fh8709 %in% 1 | exp$fh8710 %in% 1, 1, NA)
expdf$drug_16 <- ifelse(is.na(expdf$drug_16) & (exp$fh8701 %in% 2 & exp$fh8702 %in% 2 & exp$fh8703 %in% 2 & exp$fh8704 %in% 2 & exp$fh8705 %in% 2 & exp$fh8706 %in% 2 & exp$fh8707 %in% 2 & exp$fh8709 %in% 2 & exp$fh8710 %in% 2), 2, expdf$drug_16)
expdf$drug_16 <- factor(expdf$drug_16, levels=c(2:1), labels=c('No','Yes')) #since 15th birthday used amphetamines, barbiturates, ecstasy, cocaine, crack, LSD, heroin, ketamine, benzodiazepine, ritalin

expdf$drug_17 <- ifelse(exp$ccs4150 %in% c(2:3) | exp$ccs4151 %in% c(2:3) | exp$ccs4152 %in% c(2:3) | exp$ccs4153 %in% c(2:3) | exp$ccs4154 %in% c(2:3) | exp$ccs4160 %in% c(2:3) | exp$ccs4161 %in% c(2:3) | exp$ccs4162 %in% c(2:3) | exp$ccs4163 %in% c(2:3) | exp$ccs4165 %in% c(2:3) | exp$ccs4166 %in% c(2:3) | exp$ccs4167 %in% c(2:3) | exp$ccs4168 %in% c(2:3) | exp$ccs4169 %in% c(2:3) | exp$ccs4170 %in% c(2:3), 1, NA)
expdf$drug_17 <- ifelse(is.na(expdf$drug_17) & (exp$ccs4150 %in% 1 & exp$ccs4151 %in% 1 & exp$ccs4152 %in% 1 & exp$ccs4153 %in% 1 & exp$ccs4154 %in% 1 & exp$ccs4160 %in% 1 & exp$ccs4161 %in% 1 & exp$ccs4162 %in% 1 & exp$ccs4163 %in% 1 & exp$ccs4165 %in% 1 & exp$ccs4166 %in% 1 & exp$ccs4167 %in% 1 & exp$ccs4168 %in% 1 & exp$ccs4169 %in% 1 & exp$ccs4170 %in% 1), 2, expdf$drug_17)
expdf$drug_17 <- factor(expdf$drug_17, levels=c(2:1), labels=c('No','Yes')) #since 15th birthday used aerosols, gas, glue, solvents, poppers, amphetamines, ecstasy, LSD, magic mushrooms, cocaine, crack, heroin, ketamine, steroids, white widows

expdf$drug_C_18 <- ifelse(exp$FJDR5000 %in% 1 | exp$FJDR5150 %in% 1 | exp$FJDR5300 %in% 1 | exp$FJDR5450 %in% 1 | exp$FJDR5600 %in% 1 | exp$FJDR5750 %in% 1, 1, NA)
expdf$drug_C_18 <- ifelse(is.na(expdf$drug_C_18) & (exp$FJDR5000 %in% 2  & exp$FJDR5150 %in% 2  & exp$FJDR5300 %in% 2 & exp$FJDR5450 %in% 2 & exp$FJDR5600 %in% 2 & exp$FJDR5750 %in% 2), 2, expdf$drug_C_18)
expdf$drug_C_18 <- factor(expdf$drug_C_18, levels=c(2:1), labels=c('No','Yes')) #ever used ecstasy, gas, cocaine, amphetamines, inhalants, sedatives, hallucinogens, opioids

expdf$drug_Q_18 <- ifelse(exp$cct5100 %in% 1 | exp$cct5110 %in% 1 | exp$cct5120 %in% 1 | exp$cct5130 %in% 1 | exp$cct5140 %in% 1 | exp$cct5150 %in% 1 | exp$cct5160 %in% 1 | exp$cct5170 %in% 11, 1, NA)
expdf$drug_Q_18 <- ifelse(is.na(expdf$drug_Q_18) & (exp$cct5100 %in% 2 & exp$cct5110 %in% 2 & exp$cct5120 %in% 2 & exp$cct5130 %in% 2 & exp$cct5140 %in% 2 & exp$cct5150 %in% 2 & exp$cct5160 %in% 2 & exp$cct5170 %in% 2), 2, expdf$drug_Q_18)
expdf$drug_Q_18 <- factor(expdf$drug_Q_18, levels=c(2:1), labels=c('No','Yes')) #ever used cocaince, crack, amphetamine, inhalants, sedatives, hallucinogens, other stimulants

expdf$drug_20 <- ifelse(exp$CCU3401 %in% 1 | exp$CCU3411 %in% 1 | exp$CCU3421 %in% 1 | exp$CCU3431 %in% 1 | exp$CCU3441 %in% 1 | exp$CCU3451 %in% 1 | exp$CCU3461 %in% 1, 1, NA)
expdf$drug_20 <- ifelse(is.na(expdf$drug_20) & (exp$CCU3401 %in% 2 & exp$CCU3411 %in% 2 & exp$CCU3421 %in% 2 & exp$CCU3431 %in% 2 & exp$CCU3441 %in% 2 & exp$CCU3451 %in% 2 & exp$CCU3461 %in% 2), 2, expdf$drug_20)
expdf$drug_20 <- factor(expdf$drug_20, levels=c(2:1), labels=c('No','Yes')) #last year used cocaine, amphetamine, inhalants, sedatives, hallucinogens, heroin, injected illicit drugs

expdf$drug_22 <- ifelse(exp$YPB4441 %in% 1 | exp$YPB4444 %in% 1 | exp$YPB4447 %in% 1 | exp$YPB4450 %in% 1 | exp$YPB4453 %in% 1 | exp$YPB4456 %in% 1 | exp$YPB4459 %in% 1 | exp$YPB4462 %in% 1 | exp$YPB4465 %in% 1, 1, NA)
expdf$drug_22 <- ifelse(is.na(expdf$drug_22) & (exp$YPB4441 %in% 2 & exp$YPB4444 %in% 2 & exp$YPB4447 %in% 2 & exp$YPB4450 %in% 2 & exp$YPB4453 %in% 2 & exp$YPB4456 %in% 2 & exp$YPB4459 %in% 2 & exp$YPB4462 %in% 2 & exp$YPB4465 %in% 2), 2, expdf$drug_22)
expdf$drug_22 <- factor(expdf$drug_22, levels=c(2:1), labels=c('No','Yes')) #last year used cocaine, crack, amphetamine, nitrous oxide, inhalants, sedatives, hallucinogens, opioids, injected illicit drugs

expdf$drug_24 <- ifelse(exp$FKDR1010 %in% 1 | exp$FKDR1020 %in% 1 | exp$FKDR1030 %in% 1 | exp$FKDR1040 %in% 1 | exp$FKDR1050 %in% 1 | exp$FKDR1060 %in% 1 | exp$FKDR1070 %in% 1 | exp$FKDR1080 %in% 1 | exp$FKDR1090 %in% 1, 1, NA)
expdf$drug_24 <- ifelse(is.na(expdf$drug_24) & (exp$FKDR1010 %in% 0  & exp$FKDR1020 %in% 0 & exp$FKDR1030 %in% 0 & exp$FKDR1040 %in% 0 & exp$FKDR1050 %in% 0 & exp$FKDR1060 %in% 0 & exp$FKDR1070 %in% 0 & exp$FKDR1080 %in% 0 & exp$FKDR1090 %in% 0), 0, expdf$drug_24)
expdf$drug_24 <- factor(expdf$drug_24, levels=c(0:1), labels=c('No','Yes')) #ever used cocaine, crack, amphetamine, nitrous oxide, inhalants, sedatives, hallucinogens, opioids, injected illicit drugs


##Mental health and wellbeing
#Development and well-being assessment (DAWBA DSM-IV clinical diagnosis)
expdf$dawba_ADHD_8 <- ifelse(exp$kr803 %in% c(1:3), 1, NA)
expdf$dawba_ADHD_8 <- ifelse(exp$kr803 %in% 4, 2, expdf$dawba_ADHD_8)
expdf$dawba_ADHD_8 <- factor(expdf$dawba_ADHD_8, levels=c(2:1), labels=c('No','Yes'))

expdf$dawba_oppconductdis_8 <- ifelse(exp$kr813 %in% c(1:3), 1, NA)
expdf$dawba_oppconductdis_8 <- ifelse(exp$kr813 %in% 4, 2, expdf$dawba_oppconductdis_8)
expdf$dawba_oppconductdis_8 <- factor(expdf$dawba_oppconductdis_8, levels=c(2:1), labels=c('No','Yes'))

expdf$dawba_perdevdis_8 <- ifelse(exp$kr815 %in% 1, 1, NA)
expdf$dawba_perdevdis_8 <- ifelse(exp$kr815 %in% 2, 2, expdf$dawba_perdevdis_8)
expdf$dawba_perdevdis_8 <- factor(expdf$dawba_perdevdis_8, levels=c(2:1), labels=c('No','Yes'))

expdf$dawba_anxietydis_8 <- ifelse(exp$kr827 %in% c(1:3), 1, NA)
expdf$dawba_anxietydis_8 <- ifelse(exp$kr827 %in% 0, 2, expdf$dawba_anxietydis_8)
expdf$dawba_anxietydis_8 <- factor(expdf$dawba_anxietydis_8, levels=c(2:1), labels=c('No','Yes'))

expdf$dawba_depressdis_8 <- ifelse(exp$kr832 %in% c(1:2), 1, NA)
expdf$dawba_depressdis_8 <- ifelse(exp$kr832 %in% 3, 2, expdf$dawba_depressdis_8)
expdf$dawba_depressdis_8 <- factor(expdf$dawba_depressdis_8, levels=c(2:1), labels=c('No','Yes'))

expdf$dawba_mis_16 <- factor(exp$fh6405, levels=c(2:1), labels=c('No','Yes'))
expdf$dawba_irit_16 <- factor(exp$fh6425, levels=c(2:1), labels=c('No','Yes'))
expdf$dawba_losint_16 <- factor(exp$fh6435, levels=c(2:1), labels=c('No','Yes'))

#Mood and Feelings Questionnaire (SMFQ) (Scores range from 0 to 26, scoring 12 or higher may indicate presence of depression. Scoring 27 or higher in long version)
expdf$cb_mfq_10 <- as.numeric(exp$ku673a) #complete cases
expdf$cb_mfq_10[expdf$cb_mfq_10<0] <- NA
expdf$cb_mfq_10_gr <- factor(ifelse(expdf$cb_mfq_10 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

expdf$mfq_10 <- as.numeric(exp$fddp130)
expdf$mfq_10[expdf$mfq_10<0] <- NA
expdf$mfq_10_gr <- factor(ifelse(expdf$mfq_10 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

expdf$mfq_11 <- as.numeric(exp$kw6100a) #complete cases
expdf$mfq_11[expdf$mfq_11<0] <- NA
expdf$mfq_11_gr <- factor(ifelse(expdf$mfq_11 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

exp[,c('ff6500','ff6502','ff6503','ff6504','ff6505','ff6506','ff6508','ff6509','ff6511','ff6512','ff6513','ff6514','ff6515')] <- sapply(exp[,c('ff6500','ff6502','ff6503','ff6504','ff6505','ff6506','ff6508','ff6509','ff6511','ff6512','ff6513','ff6514','ff6515')], function(x) replace(x, x < 0, NA))
exp[,c('ff6500','ff6502','ff6503','ff6504','ff6505','ff6506','ff6508','ff6509','ff6511','ff6512','ff6513','ff6514','ff6515')] <- sapply(exp[,c('ff6500','ff6502','ff6503','ff6504','ff6505','ff6506','ff6508','ff6509','ff6511','ff6512','ff6513','ff6514','ff6515')], function(x) replace(x, c(3:1), c(0:2))[x])
expdf$mfq_13 <- rowSums(exp[,c('ff6500','ff6502','ff6503','ff6504','ff6505','ff6506','ff6508','ff6509','ff6511','ff6512','ff6513','ff6514','ff6515')])
expdf$mfq_13_gr <- factor(ifelse(expdf$mfq_13 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

expdf$mfq_14 <- as.numeric(exp$fg7226)
expdf$mfq_14[expdf$mfq_14<0] <- NA
expdf$mfq_14_gr <- factor(ifelse(expdf$mfq_14 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

#Clinical Interview Schedule-Revised (CIS-R) (A score of 12 or above on the CIS-R indicates caseness)
expdf$cisr_18 <- as.numeric(exp$FJCI050)
expdf$cisr_18[expdf$cisr_18<0] <- NA
expdf$cisr_18_gr <- factor(ifelse(expdf$cisr_18 >= 12, 1, 0), levels=c(0:1), labels=c('<12','>=12'))

expdf$cisr_gad_18 <- factor(exp$FJCI602, levels=c(0:1), labels=c('No','Yes')) #generalised anxiety disorder
expdf$cisr_mildep_18 <- factor(exp$FJCI603, levels=c(0:1), labels=c('No','Yes')) #mild depressive episode
expdf$cisr_panicdis_18 <- factor(exp$FJCI604, levels=c(0:1), labels=c('No','Yes')) #panic disorder syndrome
expdf$cisr_agoraphob_18 <- factor(exp$FJCI605, levels=c(0:1), labels=c('No','Yes')) #agoraphobia
expdf$cisr_socphob_18 <- factor(exp$FJCI606, levels=c(0:1), labels=c('No','Yes')) #social phobia
expdf$cisr_specphob_18 <- factor(exp$FJCI607, levels=c(0:1), labels=c('No','Yes')) #specific phobias
expdf$cisr_moddep_18 <- factor(exp$FJCI608, levels=c(0:1), labels=c('No','Yes')) #moderate depressive episode
expdf$cisr_sevdep_18 <- factor(exp$FJCI609, levels=c(0:1), labels=c('No','Yes')) #severe depressive episode
expdf$cisr_chrofat_18 <- factor(exp$FJCI610, levels=c(0:1), labels=c('No','Yes')) #chronic fatigue indicator

expdf$cisr_gad_24 <- factor(exp$FKDQ1030, levels=c(0:1), labels=c('No','Yes')) #generalised anxiety disorder
expdf$cisr_mildep_24 <- factor(exp$FKDQ1000, levels=c(0:1), labels=c('No','Yes'))
expdf$cisr_panicdis_24 <- factor(exp$FKDQ1070, levels=c(0:1), labels=c('No','Yes')) #panic disorder syndrome
expdf$cisr_socphob_24 <- factor(exp$FKDQ1050, levels=c(0:1), labels=c('No','Yes')) #social phobia
expdf$cisr_specphob_24 <- factor(exp$FKDQ1060, levels=c(0:1), labels=c('No','Yes')) #specific phobias
expdf$cisr_moddep_24 <- factor(exp$FKDQ1010, levels=c(0:1), labels=c('No','Yes')) #moderate depressive episode
expdf$cisr_sevdep_24 <- factor(exp$FKDQ1020, levels=c(0:1), labels=c('No','Yes')) #severe depressive episode
expdf$cisr_chrofat_24 <- factor(exp$FKDQ1110, levels=c(0:1), labels=c('No','Yes')) #chronic fatigue indicator

#Pychosis-like symptoms (PLIKS)
expdf$pliks_18 <- factor(exp$FJPL172, levels=c(0:1), labels=c('No','Yes'))
expdf$pliks_24 <- factor(exp$FKPL2240, levels=c(0:1), labels=c('No','Yes'))

#Warwick-Edinburgh Mental Wellbeing Scale (WEMWBS) (a score below 42 cutoff for low wellbeing)
expdf$wemwbs_18 <- as.numeric(exp$CCXD814)
expdf$wemwbs_18[expdf$wemwbs_18<0] <- NA
expdf$wemwbs_18_gr <- factor(ifelse(expdf$wemwbs_18 <= 42, 1, 0), levels=c(0:1), labels=c('>42','<=42')) 

expdf$wemwbs_23 <- as.numeric(exp$YPC0600)
expdf$wemwbs_23[expdf$wemwbs_23<0] <- NA
expdf$wemwbs_23_gr <- factor(ifelse(expdf$wemwbs_23 <= 42, 1, 0), levels=c(0:1), labels=c('>42','<=42'))

#Ever diagnosed with mental health problem
expdf$mhp_22 <- ifelse(exp$YPB1231 %in% c(1,2) | exp$YPB1232 %in% c(1,2) | exp$YPB1233 %in% c(1,2), 1, NA)
expdf$mhp_22 <- ifelse(is.na(expdf$mhp_22) & (exp$YPB1231 %in% 3 & exp$YPB1232 %in% 3 & exp$YPB1233 %in% 3), 2, expdf$mhp_22)
expdf$mhp_22 <- factor(expdf$mhp_22, levels=c(2:1), labels=c('No','Yes')) #ever diagnosed with schizophrenia, bipolar disorder, depression

#Taking medication for mental health
expdf$mhmeds_24 <- ifelse(exp$FKCO1101 %in% 1 | exp$FKCO1102 %in% 1 | exp$FKCO1103 %in% 1 | exp$FKCO1104 %in% 1 | exp$FKCO1105 %in% 1, 1, NA)
expdf$mhmeds_24 <- ifelse(is.na(expdf$mhmeds_24) & (exp$FKCO1101 %in% 0  & exp$FKCO1102 %in% 0 & exp$FKCO1103 %in% 0 & exp$FKCO1104 %in% 0 & exp$FKCO1105 %in% 0), 0, expdf$mhmeds_24)
expdf$mhmeds_24 <- factor(expdf$mhmeds_24, levels=c(0:1), labels=c('No','Yes')) #participant taking antipsychotics, antidepressants, mood stabilisers or anxiolytics

expdf$mhmeds_28 <- factor(exp$YPH7000, levels=c(2:1), labels=c('No','Yes')) #participant prescribed meds for depression or anxiety


##BMI
#5-85th percentiles is considered the normal, healthy range in children and teens by CDC
exp$bmi_7 <- as.numeric(exp$f7ms026a)
exp$bmi_7[exp$bmi_7<0] <- NA
expdf$bmi_7_gr <- ifelse(exp$bmi_7 <= median(exp$bmi_7, na.rm=T), 1, NA)
expdf$bmi_7_gr <- ifelse(exp$bmi_7 > median(exp$bmi_7, na.rm=T), 2, expdf$bmi_7_gr)
expdf$bmi_7_gr <- factor(expdf$bmi_7_gr, levels=c(1:2), labels = c('<=15.8','>15.8'))

exp$bmi_9 <- as.numeric(exp$f9ms026a)
exp$bmi_9[exp$bmi_9<0] <- NA
expdf$bmi_9_gr <- ifelse(exp$bmi_9 <= median(exp$bmi_9, na.rm=T), 1, NA)
expdf$bmi_9_gr <- ifelse(exp$bmi_9 > median(exp$bmi_9, na.rm=T), 2, expdf$bmi_9_gr)
expdf$bmi_9_gr <- factor(expdf$bmi_9_gr, levels=c(1:2), labels = c('<=17.0','>17.0'))

exp$bmi_10 <- as.numeric(exp$fdms026a)
exp$bmi_10[exp$bmi_10<0] <- NA
expdf$bmi_10_gr <- ifelse(exp$bmi_10 <= median(exp$bmi_10, na.rm=T), 1, NA)
expdf$bmi_10_gr <- ifelse(exp$bmi_10 > median(exp$bmi_10, na.rm=T), 2, expdf$bmi_10_gr)
expdf$bmi_10_gr <- factor(expdf$bmi_10_gr, levels=c(1:2), labels = c('<=17.5','>17.5'))

exp$bmi_11 <- as.numeric(exp$fems026a)
exp$bmi_11[exp$bmi_11<0] <- NA
expdf$bmi_11_gr <- ifelse(exp$bmi_11 <= median(exp$bmi_11, na.rm=T), 1, NA)
expdf$bmi_11_gr <- ifelse(exp$bmi_11 > median(exp$bmi_11, na.rm=T), 2, expdf$bmi_11_gr)
expdf$bmi_11_gr <- factor(expdf$bmi_11_gr, levels=c(1:2), labels = c('<=18.3','>18.3'))

exp$bmi_13 <- as.numeric(exp$ff2039)
exp$bmi_13[exp$bmi_13<0] <- NA
expdf$bmi_13_gr <- ifelse(exp$bmi_13 <= median(exp$bmi_13, na.rm=T), 1, NA)
expdf$bmi_13_gr <- ifelse(exp$bmi_13 > median(exp$bmi_13, na.rm=T), 2, expdf$bmi_13_gr)
expdf$bmi_13_gr <- factor(expdf$bmi_13_gr, levels=c(1:2), labels = c('<=19.1','>19.1'))

exp$bmi_14 <- as.numeric(exp$fg3139)
exp$bmi_14[exp$bmi_14<0] <- NA
expdf$bmi_14_gr <- ifelse(exp$bmi_14 <= median(exp$bmi_14, na.rm=T), 1, NA)
expdf$bmi_14_gr <- ifelse(exp$bmi_14 > median(exp$bmi_14, na.rm=T), 2, expdf$bmi_14_gr)
expdf$bmi_14_gr <- factor(expdf$bmi_14_gr, levels=c(1:2), labels = c('<=19.6','>19.6'))

exp$bmi_16 <- as.numeric(exp$fh3019)
exp$bmi_16[exp$bmi_16<0] <- NA
expdf$bmi_16_gr <- ifelse(exp$bmi_16 <= median(exp$bmi_16, na.rm=T), 1, NA)
expdf$bmi_16_gr <- ifelse(exp$bmi_16 > median(exp$bmi_16, na.rm=T), 2, expdf$bmi_16_gr)
expdf$bmi_16_gr <- factor(expdf$bmi_16_gr, levels=c(1:2), labels = c('<=20.7','>20.7'))

exp$bmi_18 <- as.numeric(exp$FJMR022a)
exp$bmi_18[exp$bmi_18<0] <- NA
expdf$bmi_18_gr <- ifelse(exp$bmi_18 <= median(exp$bmi_18, na.rm=T), 1, NA)
expdf$bmi_18_gr <- ifelse(exp$bmi_18 > median(exp$bmi_18, na.rm=T), 2, expdf$bmi_18_gr)
expdf$bmi_18_gr <- factor(expdf$bmi_18_gr, levels=c(1:2), labels = c('<=21.9','>21.9'))

exp$bmi_24 <- as.numeric(exp$FKMS1040)
exp$bmi_24[exp$bmi_24<0] <- NA
expdf$bmi_24_gr <- ifelse(exp$bmi_24 <= median(exp$bmi_24, na.rm=T), 1, NA)
expdf$bmi_24_gr <- ifelse(exp$bmi_24 > median(exp$bmi_24, na.rm=T), 2, expdf$bmi_24_gr)
expdf$bmi_24_gr <- factor(expdf$bmi_24_gr, levels=c(1:2), labels = c('<=23.8','>23.8'))

##Diet
exp$totkcal_7 <- as.numeric(exp$f7dd400)
exp$totkcal_7[exp$totkcal_7<0] <- NA
expdf$totkcal_7_gr <- ifelse(exp$totkcal_7 < median(exp$totkcal_7, na.rm=T), 1, NA)
expdf$totkcal_7_gr <- ifelse(exp$totkcal_7 >= median(exp$totkcal_7, na.rm=T), 2, expdf$totkcal_7_gr)
expdf$totkcal_7_gr <- factor(expdf$totkcal_7_gr, levels=c(1:2), labels = c('<=1686','>1686'))

exp$totkcal_14 <- as.numeric(exp$fg1560)
exp$totkcal_14[exp$totkcal_14<0] <- NA
expdf$totkcal_14_gr <- ifelse(exp$totkcal_14 < median(exp$totkcal_14, na.rm=T), 1, NA)
expdf$totkcal_14_gr <- ifelse(exp$totkcal_14 >= median(exp$totkcal_14, na.rm=T), 2, expdf$totkcal_14_gr)
expdf$totkcal_14_gr <- factor(expdf$totkcal_14_gr, levels=c(1:2), labels = c('<=1903','>1903'))

expdf$foodgr_7 <- ifelse(exp$f7dd500 %in% c(1,3,4), 1, NA)
expdf$foodgr_7 <- ifelse(exp$f7dd500 %in% 2, 2, expdf$foodgr_7)
expdf$foodgr_7 <- factor(expdf$foodgr_7, levels=c(2:1), labels = c('Healthy','Processed/Traditional/Lunch'))

expdf$foodgr_14 <- ifelse(exp$fg1570 %in% c(1,3,4), 1, NA)
expdf$foodgr_14 <- ifelse(exp$fg1570 %in% 2, 2, expdf$foodgr_14)
expdf$foodgr_14 <- factor(expdf$foodgr_14, levels=c(2:1), labels = c('Healthy','Processed/Traditional/Lunch'))

##Physical activity
#Accelerometer data e.g. moderate-to-vigorous physical activity
expdf$mvpa3600_11 <- ifelse(exp$feag100 %in% c(0:4), 1, NA)
expdf$mvpa3600_11 <- ifelse(exp$feag100 %in% c(5:7), 2, expdf$mvpa3600_11)
expdf$mvpa3600_11 <- factor(expdf$mvpa3600_11, levels=c(2:1), labels = c('5+','0-4'))

expdf$mvpa3600_14 <- ifelse(exp$fg1205 %in% c(0:4), 1, NA)
expdf$mvpa3600_14 <- ifelse(exp$fg1205 %in% c(5:7), 2, expdf$mvpa3600_14)
expdf$mvpa3600_14 <- factor(expdf$mvpa3600_14, levels=c(2:1), labels = c('5+','0-4'))

expdf$mvpa3600_16 <- ifelse(exp$fh5010 %in% c(0:4), 1, NA)
expdf$mvpa3600_16 <- ifelse(exp$fh5010 %in% c(5:7), 2, expdf$mvpa3600_16)
expdf$mvpa3600_16 <- factor(expdf$mvpa3600_16, levels=c(2:1), labels = c('5+','0-4'))

expdf$mvpa_24 <- ifelse(exp$FKAC1010 %in% c(0:1), 1, NA)
expdf$mvpa_24 <- ifelse(exp$FKAC1010 %in% c(2:3), 2, expdf$mvpa_24)
expdf$mvpa_24 <- factor(expdf$mvpa_24, levels=c(2:1), labels = c('2-3','0-1'))

#Plays sports, goes to sports clubs, etc
expdf$sportclubs_11 <- ifelse(exp$fefs029 %in% 1 | exp$fefs047 %in% 1 | exp$fefs049 %in% 1, 1, NA)
expdf$sportclubs_11 <- ifelse(is.na(expdf$sportclubs_11) & (exp$fefs029 %in% 2 & exp$fefs047 %in% 2 & exp$fefs049 %in% 2), 2, expdf$sportclubs_11)
expdf$sportclubs_11 <- factor(expdf$sportclubs_11, levels=c(1:2), labels=c('Yes','No')) #goes to sports centre, plays outdoor sports, plays organised sports, with friends

expdf$sportclubs_13 <- ifelse(exp$ff5429 %in% 1 | exp$ff5447 %in% 1 | exp$ff5449 %in% 1, 1, NA)
expdf$sportclubs_13 <- ifelse(is.na(expdf$sportclubs_13) & (exp$ff5429 %in% 2 & exp$fefs047 %in% 2 & exp$ff5449 %in% 2), 2, expdf$sportclubs_13)
expdf$sportclubs_13 <- factor(expdf$sportclubs_13, levels=c(1:2), labels=c('Yes','No'))

expdf$sportclubs_14 <- ifelse(exp$fg4147 %in% 1 | exp$fg4149 %in% 1, 1, NA)
expdf$sportclubs_14 <- ifelse(is.na(expdf$sportclubs_14) & (exp$fg4147 %in% 2 & exp$fg4149 %in% 2), 2, expdf$sportclubs_14)
expdf$sportclubs_14 <- factor(expdf$sportclubs_14, levels=c(1:2), labels=c('Yes','No')) #plays outside or orgnaised sports with other children

#Frequency exercised past year
expdf$exerc_14 <- ifelse(exp$ccq320 %in% c(1:2), 1, NA)
expdf$exerc_14 <- ifelse(exp$ccq320 %in% c(3:5), 2, expdf$exerc_14)
expdf$exerc_14 <- factor(expdf$exerc_14, levels=c(1:2), labels = c('> Weekly','< Weekly'))

expdf$exerc_17 <- ifelse(exp$ccs5510 %in% c(1:2), 1, NA)
expdf$exerc_17 <- ifelse(exp$ccs5510 %in% c(3:5), 2, expdf$exerc_17)
expdf$exerc_17 <- factor(expdf$exerc_17, levels=c(1:2), labels = c('> Weekly','< Weekly'))

expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(1:2), 1, NA)
expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(3:5), 2, expdf$exerc_18)
expdf$exerc_18 <- factor(expdf$exerc_18, levels=c(1:2), labels = c('> Weekly','< Weekly'))

expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(1:2), 1, NA)
expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(3:5), 2, expdf$exerc_22)
expdf$exerc_22 <- factor(expdf$exerc_22, levels=c(1:2), labels = c('> Weekly','< Weekly'))

##Sleep
#Has regular sleep routine
expdf$regslp_1 <- factor(exp$kb450, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_3 <- factor(exp$kf230, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_4 <- factor(exp$kj205, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_5 <- factor(exp$kl220, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_6 <- factor(exp$kn2000, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_7 <- factor(exp$kq250, levels=c(1:2), labels=c('Yes','No'))
expdf$regslp_10 <- factor(exp$ku720, levels=c(1:2), labels=c('Yes','No'))

#Time asleep
exp$kw4060a[exp$kw4060a<0] <- NA 
exp$kw4060b[exp$kw4060b<0] <- NA
exp$kw4061a[exp$kw4061a<0] <- NA
exp$kw4061b[exp$kw4061b<0] <- NA
exp$kw4061a[exp$kw4061a>12] <- NA
exp$kw4061a <- exp$kw4061a+12
exp$kw4060b_h <- exp$kw4060b/60
exp$kw4061b_h <- exp$kw4061b/60
exp$kw4060 <- (exp$kw4060a+exp$kw4060b_h) 
exp$kw4061 <- (exp$kw4061a+exp$kw4061b_h)
expdf$slp_wkdays_11 <- (24-exp$kw4061)+exp$kw4060
expdf$slp_wkdays_11_gr <- ifelse(expdf$slp_wkdays_11 <= 10, 1, NA)
expdf$slp_wkdays_11_gr <- ifelse(expdf$slp_wkdays_11 > 10, 2, expdf$slp_wkdays_11_gr)
expdf$slp_wkdays_11_gr <- factor(expdf$slp_wkdays_11_gr, levels=c(2:1), labels=c('>=10','<=10')) #school days (time between going to sleep and waking up)

exp$fh5420[exp$fh5420<0] <- NA 
exp$fh5421[exp$fh5421<0] <- NA
exp$fh5425[exp$fh5425<0] <- NA
exp$fh5426[exp$fh5426<0] <- NA
exp$fh5420[exp$fh5420 %in% 0] <- 24
exp$fh5420[exp$fh5420 %in% 1] <- 25
exp$fh5420[exp$fh5420 < 8] <- NA
exp$fh5421_h <- exp$fh5421/60
exp$fh5426_h <- exp$fh5426/60
exp$fh54201 <- (exp$fh5420+exp$fh5421_h) 
exp$fh54256 <- (exp$fh5425+exp$fh5426_h)
expdf$slp_wkdays_16 <- (24-exp$fh54201)+exp$fh54256
expdf$slp_wkdays_16_gr <- ifelse(expdf$slp_wkdays_16 <= 8, 1, NA)
expdf$slp_wkdays_16_gr <- ifelse(expdf$slp_wkdays_16 > 8, 2, expdf$slp_wkdays_16_gr)
expdf$slp_wkdays_16_gr <- factor(expdf$slp_wkdays_16_gr, levels=c(2:1), labels=c('>=8','<=8')) #school days (time between starts trying to go to sleep and waking up)

exp$YPE7440[exp$YPE7440<0] <- NA 
expdf$slp_25 <- exp$YPE7440
expdf$slp_25_gr <- ifelse(expdf$slp_25 <= 8, 1, NA)
expdf$slp_25_gr <- ifelse(expdf$slp_25 > 8, 2, expdf$slp_25_gr)
expdf$slp_25_gr <- factor(expdf$slp_25_gr, levels=c(2:1), labels=c('>=8','<=8')) #number of hours sleeps in 24 hours


##Respondent or partner pregnant
expdf$everpreg_17 <- factor(exp$ccs2210, levels=c(2:1), labels=c('No','Yes'))
expdf$everpreg_21 <- factor(exp$YPA3310, levels=c(2:1), labels=c('No','Yes'))
expdf$everpreg_22 <- factor(exp$YPB6020, levels=c(5:1), labels=c('No',rep('Yes',4))) #since 21
expdf$everpreg_24 <- factor(exp$YPD1020, levels=c(0:4), labels=c('No',rep('Yes',4))) #since 23

expdf$currpreg_20 <- factor(exp$CCU1005, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$currpreg_21 <- factor(exp$YPA1020, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$currpreg_23 <- factor(exp$YPC1070, levels=c(0:2), labels=c('No','Yes','Yes'))

expdf$npregs_21 <- ifelse(exp$YPA3311 %in% c(0:1), 1, NA)
expdf$npregs_21 <- ifelse(exp$YPA3311 > 1, 2, expdf$npregs_21)
expdf$npregs_21 <- factor(expdf$npregs_21, levels=c(1:2), labels=c('0-1','>1'))

expdf$baby_22 <- factor(exp$YPB6030, levels=c(5:1), labels=c('No',rep('Yes',4))) #since 21
expdf$baby_24 <- factor(exp$YPD1030, levels=c(0:4), labels=c('No',rep('Yes',4))) #since 23

expdf$parent_20 <- factor(exp$CCU1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_21 <- factor(exp$YPA1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_22 <- factor(exp$YPB7000, levels=c(2:1), labels=c('No','Yes'))
expdf$parent_23 <- factor(exp$YPC1050, levels=c(0:1), labels=c('No','Yes'))
expdf$parent_25 <- factor(exp$YPE0101, levels=c(0:1), labels=c('No','Yes'))
expdf$parent_28 <- factor(exp$YPH3010, levels=c(0:1), labels=c('No','Yes'))


##Cotinine (cutoffs from lower ends of this paper: https://pubmed.ncbi.nlm.nih.gov/27983665/)
#Child bloods (serum)
exp$Cotinine_F7[exp$Cotinine_F7<0] <- NA
exp$Cotinine_TF3[exp$Cotinine_TF3<0] <- NA
exp$cotinine_TF4_avg[exp$cotinine_TF4_avg<0] <- NA
expdf$cotinine_7 <- as.numeric(exp$Cotinine_F7)
expdf$cotinine_16 <- as.numeric(exp$Cotinine_TF3)
expdf$cotinine_18 <- as.numeric(exp$cotinine_TF4_avg)
expdf$cotinine_7_gr <- factor(ifelse(expdf$cotinine_7 >= 10, 1, 0), levels=c(0:1), labels=c('<10 ng/ml','>=10 ng/ml'))
expdf$cotinine_16_gr <- factor(ifelse(expdf$cotinine_16 >= 10, 1, 0), levels=c(0:1), labels=c('<10 ng/ml','>=10 ng/ml'))
expdf$cotinine_18_gr <- factor(ifelse(expdf$cotinine_18 >= 10, 1, 0), levels=c(0:1), labels=c('<10 ng/ml','>=10 ng/ml'))

#Mother bloods (urine)
exp$cotinine_Preg_Trim1_avg[exp$cotinine_Preg_Trim1_avg<0] <- NA
exp$cotinine_Preg_Trim3_avg[exp$cotinine_Preg_Trim3_avg<0] <- NA
expdf$m_cotinine_trim1_gest <- as.numeric(exp$cotinine_Preg_Trim1_avg)
expdf$m_cotinine_trim3_gest <- as.numeric(exp$cotinine_Preg_Trim3_avg)
expdf$m_cotinine_trim1_gest_gr <- factor(ifelse(expdf$m_cotinine_trim1_gest >= 50, 1, 0), levels=c(0:1), labels=c('<50 ng/ml','>=50 ng/ml'))
expdf$m_cotinine_trim3_gest_gr <- factor(ifelse(expdf$m_cotinine_trim3_gest >= 50, 1, 0), levels=c(0:1), labels=c('<50 ng/ml','>=50 ng/ml'))



###SOCIODEMOGRAPHIC FACTORS
##Sex and Ethnicity
#Sex
expdf$sex <- factor(exp$kz021, levels=c(1:2), labels=c('Male','Female'))
#Ethnicity
expdf$m_ethnic <- ifelse(exp$c800 %in% 1, 1, NA)
expdf$m_ethnic <- ifelse(exp$c800 %in% c(2:9), 2, expdf$m_ethnic)
expdf$m_ethnic <- factor(expdf$m_ethnic, levels=c(1:2), labels=c('White','Non-white'))

expdf$p_ethnic <- ifelse(exp$c801 %in% 1, 1, NA)
expdf$p_ethnic <- ifelse(exp$c801 %in% c(2:9), 2, expdf$p_ethnic)
expdf$p_ethnic <- factor(expdf$p_ethnic, levels=c(1:2), labels=c('White','Non-white'))

expdf$ethnic <- factor(exp$c804, levels=c(1:2), labels=c('White','Non-white'))

##Sexual orientation
expdf$sexor_23 <- factor(exp$YPC0420, levels=c(1:6), labels=c(rep('Straight',2),'Bisexual',rep('Homosexual',2),'Asexual'))

                      
##Parental SEP
#Mother marital status
expdf$m_marital_gest <- ifelse(exp$a525 %in% c(5:6), 1, NA)
expdf$m_marital_gest <- ifelse(exp$a525 %in% c(1:4), 2, expdf$m_marital_gest)
expdf$m_marital_gest <- factor(expdf$m_marital_gest, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_1 <- ifelse(exp$f460 %in% c(5:6), 1, NA)
expdf$m_marital_1 <- ifelse(exp$f460 %in% c(1:4), 2, expdf$m_marital_1)
expdf$m_marital_1 <- factor(expdf$m_marital_1, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_2 <- ifelse(exp$g517 %in% c(5:6), 1, NA)
expdf$m_marital_2 <- ifelse(exp$g517 %in% c(1:4), 2, expdf$m_marital_2)
expdf$m_marital_2 <- factor(expdf$m_marital_2, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_3 <- ifelse(exp$h386 %in% c(5:6), 1, NA)
expdf$m_marital_3 <- ifelse(exp$h386 %in% c(1:4), 2, expdf$m_marital_3)
expdf$m_marital_3 <- factor(expdf$m_marital_3, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_4 <- ifelse(exp$j370 %in% c(5:6), 1, NA)
expdf$m_marital_4 <- ifelse(exp$j370 %in% c(1:4), 2, expdf$m_marital_4)
expdf$m_marital_4 <- factor(expdf$m_marital_4, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_7 <- ifelse(exp$m3040 %in% c(5:7), 1, NA)
expdf$m_marital_7 <- ifelse(exp$m3040 %in% c(1:4), 2, expdf$m_marital_7)
expdf$m_marital_7 <- factor(expdf$m_marital_7, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_8 <- ifelse(exp$n8040 %in% c(5:6), 1, NA)
expdf$m_marital_8 <- ifelse(exp$n8040 %in% c(1:4), 2, expdf$m_marital_8)
expdf$m_marital_8 <- factor(expdf$m_marital_8, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_10 <- ifelse(exp$q3040 %in% c(5:6), 1, NA)
expdf$m_marital_10 <- ifelse(exp$q3040 %in% c(1:4), 2, expdf$m_marital_10)
expdf$m_marital_10 <- factor(expdf$m_marital_10, levels=c(1:2), labels=c('Married','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_18 <- ifelse(exp$t1050 %in% c(5:9), 1, NA)
expdf$m_marital_18 <- ifelse(exp$t1050 %in% c(1:4), 2, expdf$m_marital_18)
expdf$m_marital_18 <- factor(expdf$m_marital_18, levels=c(1:2), labels=c('Married/Living as married/Civil partnership','Never married/Seperated/Divorced/Widowed'))

expdf$m_marital_22 <- ifelse(exp$V0301 %in% c(5:7), 1, NA)
expdf$m_marital_22 <- ifelse(exp$V0301 %in% c(1:4), 2, expdf$m_marital_22)
expdf$m_marital_22 <- factor(expdf$m_marital_22, levels=c(1:2), labels=c('Married/Living as married/Civil partnership','Never married/Seperated/Divorced/Widowed'))

#Mother home ownership status
expdf$m_homown_gest <- ifelse(exp$a006 %in% c(0:1), 1, NA)
expdf$m_homown_gest <- ifelse(exp$a006 %in% c(2:6), 2, expdf$m_homown_gest)
expdf$m_homown_gest <- factor(expdf$m_homown_gest, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_1 <- ifelse(exp$f304 %in% c(0:2), 1, NA)
expdf$m_homown_1 <- ifelse(exp$f304 %in% c(3:6), 2, expdf$m_homown_1)
expdf$m_homown_1 <- factor(expdf$m_homown_1, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_3 <- ifelse(exp$h262 %in% c(0:1), 1, NA)
expdf$m_homown_3 <- ifelse(exp$h262 %in% c(2:6), 2, expdf$m_homown_3)
expdf$m_homown_3 <- factor(expdf$m_homown_3, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_5 <- ifelse(exp$k5010 %in% c(0:2), 1, NA)
expdf$m_homown_5 <- ifelse(exp$k5010 %in% c(3:7), 2, expdf$m_homown_5)
expdf$m_homown_5 <- factor(expdf$m_homown_5, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_7 <- ifelse(exp$m2010 %in% c(0:2), 1, NA)
expdf$m_homown_7 <- ifelse(exp$m2010 %in% c(3:7), 2, expdf$m_homown_7)
expdf$m_homown_7 <- factor(expdf$m_homown_7, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_10 <- ifelse(exp$q2010 %in% c(0:2), 1, NA)
expdf$m_homown_10 <- ifelse(exp$q2010 %in% c(3:7), 2, expdf$m_homown_10)
expdf$m_homown_10 <- factor(expdf$m_homown_10, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_18 <- ifelse(exp$t1010 %in% c(1:3), 1, NA)
expdf$m_homown_18 <- ifelse(exp$t1010 %in% c(4:7), 2, expdf$m_homown_18)
expdf$m_homown_18 <- factor(expdf$m_homown_18, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

expdf$m_homown_22 <- ifelse(exp$V0310 %in% c(1:3), 1, NA)
expdf$m_homown_22 <- ifelse(exp$V0310 %in% c(4:7), 2, expdf$m_homown_22)
expdf$m_homown_22 <- factor(expdf$m_homown_22, levels=c(1:2), labels=c('Mortgaged/Owned','Rented/HA/Other'))

#Parental economic activity
expdf$m_econ_gest <- ifelse(exp$c710 %in% 1 | exp$c711 %in% 1 | exp$c712 %in% 1 | exp$c713 %in% 1 | exp$c714 %in% 1 | exp$c715 %in% 1 | exp$c717 %in% 1, 1, NA)
expdf$m_econ_gest <- ifelse(is.na(expdf$m_econ_gest) & (exp$c716 %in% 1 | exp$c718 %in% 1 | exp$c719 %in% 1 | exp$c720 %in% 1), 2, expdf$m_econ_gest)
expdf$m_econ_gest <- factor(expdf$m_econ_gest, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family'))

expdf$m_econ_8 <- ifelse(exp$n4100 %in% 1 | exp$n4101 %in% 1 | exp$n4102 %in% 1 | exp$n4103 %in% 1 | exp$n4104 %in% 1 | exp$n4105 %in% 1 | exp$n4107 %in% 1, 1, NA)
expdf$m_econ_8 <- ifelse(is.na(expdf$m_econ_8) & (exp$n4106 %in% 1 | exp$n4108 %in% 1 | exp$n4109 %in% 1 | exp$n4110 %in% 1 | exp$n4111 %in% 1), 2, expdf$m_econ_8)
expdf$m_econ_8 <- factor(expdf$m_econ_8, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

expdf$m_econ_18 <- ifelse(exp$t1150 %in% 1 | exp$t1164 %in% 1 | exp$t1158 %in% 1, 1, NA)
expdf$m_econ_18 <- ifelse(is.na(expdf$m_econ_18) & (exp$t1154 %in% 1 | exp$t1156 %in% 1 | exp$t1152 %in% 1 | exp$t1160 %in% 1 | exp$t1162 %in% 1), 2, expdf$m_econ_18)
expdf$m_econ_18 <- factor(expdf$m_econ_18, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Volunteer/Looks after family'))

expdf$m_econ_22 <- ifelse(exp$V1150 %in% 1 | exp$V1164 %in% 1 | exp$V1158 %in% 1, 1, NA)
expdf$m_econ_22 <- ifelse(is.na(expdf$m_econ_22) & (exp$V1154 %in% 1 | exp$V1156 %in% 1 | exp$V1152 %in% 1 | exp$V1160 %in% 1 | exp$V1166 %in% 1), 2, expdf$m_econ_22)
expdf$m_econ_22 <- factor(expdf$m_econ_22, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Volunteer/Looks after family/Carer'))

expdf$p_econ_gest <- ifelse(exp$c730 %in% 1 | exp$c731 %in% 1 | exp$c732 %in% 1 | exp$c733 %in% 1 | exp$c734 %in% 1 | exp$c735 %in% 1 | exp$c737 %in% 1, 1, NA)
expdf$p_econ_gest <- ifelse(is.na(expdf$p_econ_gest) & (exp$c736 %in% 1 | exp$c738 %in% 1 | exp$c739 %in% 1 | exp$c740 %in% 1), 2, expdf$p_econ_gest)
expdf$p_econ_gest <- factor(expdf$p_econ_gest, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family'))

expdf$p_econ_8 <- ifelse(exp$n4120 %in% 1 | exp$n4121 %in% 1 | exp$n4122 %in% 1 | exp$n4123 %in% 1 | exp$n4124 %in% 1 | exp$n4125 %in% 1 | exp$n4127 %in% 1, 1, NA)
expdf$p_econ_8 <- ifelse(is.na(expdf$p_econ_8) & (exp$n4126 %in% 1 | exp$n4128 %in% 1 | exp$n4129 %in% 1 | exp$n4130 %in% 1 | exp$n4131 %in% 1), 2, expdf$p_econ_8)
expdf$p_econ_8 <- factor(expdf$p_econ_8, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

expdf$p_econ_18 <- ifelse(exp$t1151 %in% 1 | exp$t1165 %in% 1 | exp$t1159 %in% 1, 1, NA)
expdf$p_econ_18 <- ifelse(is.na(expdf$p_econ_18) & (exp$t1155 %in% 1 | exp$t1157 %in% 1 | exp$t1153 %in% 1 | exp$t1161 %in% 1 | exp$t1163 %in% 1), 2, expdf$p_econ_18)
expdf$p_econ_18 <- factor(expdf$p_econ_18, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Volunteer/Looking after family'))

expdf$p_econ_22 <- ifelse(exp$V1151 %in% 1 | exp$V1165 %in% 1 | exp$V1159 %in% 1, 1, NA)
expdf$p_econ_22 <- ifelse(is.na(expdf$p_econ_22) & (exp$V1155 %in% 1 | exp$V1153 %in% 1 | exp$V1157 %in% 1 | exp$V1161 %in% 1 | exp$V1163 %in% 1 | exp$V1167 %in% 1), 2, expdf$p_econ_22)
expdf$p_econ_22 <- factor(expdf$p_econ_22, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Volunteer/Looking after family/Carer'))

#Parental social class
expdf$m_sc_gest <- ifelse(exp$c755 %in% c(1:2), 1, NA)
expdf$m_sc_gest <- ifelse(exp$c755 %in% c(3:6), 2, expdf$m_sc_gest)
expdf$m_sc_gest <- factor(expdf$m_sc_gest, levels=c(1:2), labels=c('I/II','III/IV/V'))

expdf$p_sc_gest <- ifelse(exp$c765 %in% c(1:2), 1, NA)
expdf$p_sc_gest <- ifelse(exp$c765 %in% c(3:6), 2, expdf$p_sc_gest)
expdf$p_sc_gest <- factor(expdf$p_sc_gest, levels=c(1:2), labels=c('I/II','III/IV/V'))

#Household income
expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(1:5), 1, NA)
expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(6:10), 2, expdf$hhincome_11)
expdf$hhincome_11 <- factor(expdf$hhincome_11, levels=c(2:1), labels=c('>=??360','<??360')) #each week

expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(1:5), 1, NA)
expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(6:10), 2, expdf$hhincome_18)
expdf$hhincome_18 <- factor(expdf$hhincome_18, levels=c(2:1), labels=c('>=??2100','<??2100')) #each month

#Parental education
expdf$m_hiqual_gest <- ifelse(exp$c645 %in% c(0:3), 1, NA)
expdf$m_hiqual_gest <- ifelse(exp$c645 %in% c(4:5), 2, expdf$m_hiqual_gest)
expdf$m_hiqual_gest <- factor(expdf$m_hiqual_gest, levels=c(2:1), labels=c('A level or above','O level or below'))

expdf$p_hiqual_gest <- ifelse(exp$c666 %in% c(0:3), 1, NA)
expdf$p_hiqual_gest <- ifelse(exp$c666 %in% c(4:5), 2, expdf$p_hiqual_gest)
expdf$p_hiqual_gest <- factor(expdf$p_hiqual_gest, levels=c(2:1), labels=c('A level or above','O level or below'))

expdf$m_qual_5 <- ifelse(exp$k6281 %in% 1 | exp$k6282 %in% 1 | exp$k6283 %in% 1 | exp$k6284 %in% 1 | exp$k6285 %in% 1 | exp$k6286 %in% 1 | exp$k6287 %in% 1 | exp$k6288 %in% 1 | exp$k6289 %in% 1 | exp$k6290 %in% 1 | exp$k6291 %in% 1 | exp$k6292 %in% 1, 1, NA)
expdf$m_qual_5 <- ifelse(is.na(expdf$m_qual_5) & (exp$k6280 %in% 1), 2, expdf$m_qual_5) #other/not known quals not included
expdf$m_qual_5 <- factor(expdf$m_qual_5, levels=c(1:2), labels=c('Any qualifications','No qualifications'))

expdf$p_qual_5 <- ifelse(exp$k6301 %in% 1 | exp$k6302 %in% 1 | exp$k6303 %in% 1 | exp$k6304 %in% 1 | exp$k6305 %in% 1 | exp$k6306 %in% 1 | exp$k6307 %in% 1 | exp$k6308 %in% 1 | exp$k6309 %in% 1 | exp$k6310 %in% 1 | exp$k6311 %in% 1 | exp$k6312 %in% 1, 1, NA)
expdf$p_qual_5 <- ifelse(is.na(expdf$p_qual_5) & (exp$k6300 %in% 1), 2, expdf$p_qual_5) #other/not known quals not included
expdf$p_qual_5 <- factor(expdf$p_qual_5, levels=c(1:2), labels=c('Any qualifications','No qualifications'))

expdf$m_qual_8 <- ifelse(exp$n4000 %in% 1 | exp$n4001 %in% 1 | exp$n4002 %in% 1 | exp$n4003 %in% 1 | exp$n4004 %in% 1 | exp$n4005 %in% 1 | exp$n4006 %in% 1 | exp$n4007 %in% 1 | exp$n4008 %in% 1 | exp$n4009 %in% 1 | exp$n4010 %in% 1 | exp$n4011 %in% 1, 1, NA)
expdf$m_qual_8 <- ifelse(is.na(expdf$m_qual_8) & (exp$n4012 %in% 1), 2, expdf$m_qual_8) #other/not known quals not included
expdf$m_qual_8 <- factor(expdf$m_qual_8, levels=c(1:2), labels=c('Any qualifications','No qualifications'))

expdf$p_qual_8 <- ifelse(exp$n4020 %in% 1 | exp$n4021 %in% 1 | exp$n4022 %in% 1 | exp$n4023 %in% 1 | exp$n4024 %in% 1 | exp$n4025 %in% 1 | exp$n4026 %in% 1 | exp$n4027 %in% 1 | exp$n4028 %in% 1 | exp$n4029 %in% 1 | exp$n4030 %in% 1 | exp$n4031 %in% 1, 1, NA)
expdf$p_qual_8 <- ifelse(is.na(expdf$p_qual_8) & (exp$n4032 %in% 1), 2, expdf$p_qual_8) #other/not known quals not included
expdf$p_qual_8 <- factor(expdf$p_qual_8, levels=c(1:2), labels=c('Any qualifications','No qualifications'))

#Maternal grandparents education
expdf$mgm_hiqual_gest <- ifelse(exp$c686 %in% c(0:3), 1, NA)
expdf$mgm_hiqual_gest <- ifelse(exp$c686 %in% c(4:5), 2, expdf$mgm_hiqual_gest)
expdf$mgm_hiqual_gest <- factor(expdf$mgm_hiqual_gest, levels=c(2:1), labels=c('A level or above','O level or below'))

expdf$mgf_hiqual_gest <- ifelse(exp$c706 %in% c(0:3), 1, NA)
expdf$mgf_hiqual_gest <- ifelse(exp$c706 %in% c(4:5), 2, expdf$mgf_hiqual_gest)
expdf$mgf_hiqual_gest <- factor(expdf$mgf_hiqual_gest, levels=c(2:1), labels=c('A level or above','O level or below'))

expdf$mgm_qual_8 <- ifelse(exp$n4040 %in% 1 | exp$n4041 %in% 1 | exp$n4042 %in% 1 | exp$n4043 %in% 1 | exp$n4044 %in% 1 | exp$n4045 %in% 1 | exp$n4046 %in% 1 | exp$n4047 %in% 1 | exp$n4048 %in% 1 | exp$n4049 %in% 1 | exp$n4050 %in% 1 | exp$n4051 %in% 1, 1, NA)
expdf$mgm_qual_8 <- ifelse(is.na(expdf$mgm_qual_8) & (exp$n4052 %in% 1), 2, expdf$mgm_qual_8) #other/not known quals not included
expdf$mgm_qual_8 <- factor(expdf$mgm_qual_8, levels=c(1:2), labels=c('Any qualifications','No qualifications'))

expdf$mgf_qual_8 <- ifelse(exp$n4060 %in% 1 | exp$n4061 %in% 1 | exp$n4062 %in% 1 | exp$n4063 %in% 1 | exp$n4064 %in% 1 | exp$n4065 %in% 1 | exp$n4066 %in% 1 | exp$n4067 %in% 1 | exp$n4068 %in% 1 | exp$n4069 %in% 1 | exp$n4070 %in% 1 | exp$n4071 %in% 1, 1, NA)
expdf$mgf_qual_8 <- ifelse(is.na(expdf$mgf_qual_8) & (exp$n4072 %in% 1), 2, expdf$mgf_qual_8) #other/not known quals not included
expdf$mgf_qual_8 <- factor(expdf$mgf_qual_8, levels=c(1:2), labels=c('Any qualifications','No qualifications'))


##Education
#Aspirations and plans
expdf$y11asp_14 <- factor(exp$ccp800, level=c(1:2), c('Stay in education','Leave education'))
expdf$y11perc_14 <- factor(exp$ccp815, level=c(1:2), c('Stay in education','Leave education'))
expdf$asp_16 <- factor(exp$ccxa290, level=c(1:2), c('Stay in education','Leave education'))

#Educational qualifications obtained
expdf$qual_18 <- ifelse((exp$cct2902 %in% 1 | exp$cct2904 %in% 1 | exp$cct2906 %in% 1), 1, NA) #other/not known quals not included
expdf$qual_18 <- ifelse(is.na(expdf$qual_18) & (exp$cct2900 %in% 1 | exp$cct2901 %in% 1 | exp$cct2908 %in% 1 | exp$cct2909 %in% 1 | exp$cct2910 %in% 1 | exp$cct2912 %in% 1 | exp$cct2914 %in% 1 | exp$cct2915 %in% 1 | exp$cct2916 %in% 1), 2, expdf$qual_18)
expdf$qual_18 <- factor(expdf$qual_18, levels=c(1:2), labels=c('A levels or equivalent','Other qualifications'))

expdf$qual_20 <- ifelse((exp$CCU4000 %in% 1 | exp$CCU4001 %in% 1 | exp$CCU4002 %in% 1 | exp$CCU4005 %in% 1 | exp$CCU4006 %in% 1 | exp$CCU4007 %in% 1 | exp$CCU4008 %in% 1 | exp$CCU4009 %in% 1 | exp$CCU4010 %in% 1 | exp$CCU4013 %in% 1 | exp$CCU4014 %in% 1 | exp$CCU4017 %in% 1), 1, NA) #other/not known quals not included
expdf$qual_20 <- ifelse(is.na(expdf$qual_20) & (exp$CCU4003 %in% 1 | exp$CCU4004 %in% 1 | exp$CCU4011 %in% 1 | exp$CCU4012 %in% 1 | exp$CCU4015 %in% 1 | exp$CCU4016 %in% 1 | exp$CCU4018 %in% 1 | exp$CCU4019 %in% 1 | exp$CCU4020 %in% 1 | exp$CCU4021 %in% 1 | exp$CCU4022 %in% 1 | exp$CCU4023 %in% 1), 2, expdf$qual_20)
expdf$qual_20 <- factor(expdf$qual_20, levels=c(1:2), labels=c('A levels/Higher education','Other qualifications'))

expdf$hiqual_26 <- ifelse((exp$YPF7970 %in% c(4:8)), 1, NA) 
expdf$hiqual_26 <- ifelse(is.na(expdf$hiqual_26) & (exp$YPF7970 %in% c(1:3)), 2, expdf$hiqual_26)
expdf$hiqual_26 <- factor(expdf$hiqual_26, levels=c(1:2), labels=c('Higher education','A levels or below'))

exp$YPF7980[exp$YPF7980 < 0] <- NA
expdf$eduyrs_26 <- ifelse((exp$YPF7980 >= 15), 1, NA) 
expdf$eduyrs_26 <- ifelse(is.na(expdf$eduyrs_26) & (exp$YPF7980 < 15), 2, expdf$eduyrs_26)
expdf$eduyrs_26 <- factor(expdf$eduyrs_26, levels=c(1:2), labels=c('>=15','<15'))

#Educational qualifications currently studying
expdf$uni_18 <- ifelse(exp$cct2993 %in% c(1:2), 1, NA)
expdf$uni_18 <- ifelse(exp$cct2993 %in% c(3:4), 2, expdf$uni_18)
expdf$uni_18 <- factor(expdf$uni_18, levels=c(1:2), labels=c('Likely','Not likely'))

expdf$studqual_18 <- ifelse(exp$cct2957 %in% 1 | exp$cct2959 %in% 1 | exp$cct2961 %in% 1, 1, NA)
expdf$studqual_18 <- ifelse(is.na(expdf$studqual_18) & (exp$cct2955 %in% 1 | exp$cct2956 %in% 1 | exp$cct2963 %in% 1 | exp$cct2964 %in% 1 | exp$cct2965 %in% 1 | exp$cct2967 %in% 1 | exp$cct2969 %in% 1 | exp$cct2970 %in% 1 | exp$cct2971 %in% 1), 2, expdf$studqual_18) #other/not known quals not included
expdf$studqual_18 <- factor(expdf$studqual_18, levels=c(2:1), labels=c('A levels or equivalent','Other qualifications'))

expdf$studqual_22 <- ifelse(exp$YPB9091 %in% 1 | exp$YPB9092 %in% 1 | exp$YPB9098 %in% 1 | exp$YPB9099 %in% 1, 1, NA)
expdf$studqual_22 <- ifelse(is.na(expdf$studqual_22) & (exp$YPB9090 %in% 1 | exp$YPB9093 %in% 1 | exp$YPB9094 %in% 1 | exp$YPB9095 %in% 1 | exp$YPB9096 %in% 1 | exp$YPB9097 %in% 1), 2, expdf$studqual_22) #other/not known quals not included
expdf$studqual_22 <- factor(expdf$studqual_22, levels=c(1:2), labels=c('A levels/Higher education','Other qualifications'))

expdf$inedu_22 <- ifelse(exp$YPB9080 %in% c(1,2), 1, NA)
expdf$inedu_22 <- ifelse(exp$YPB9080 %in% 3, 2, expdf$inedu_22)
expdf$inedu_22 <- factor(expdf$inedu_22, levels=c(1:2), labels=c('Yes','No'))


##Employment
#Economic activity
expdf$eduact_18 <- ifelse(exp$cct2950 %in% c(1:6), 1, NA)
expdf$eduact_18 <- ifelse(exp$cct2950 %in% 7, 2, expdf$eduact_18)
expdf$eduact_18 <- factor(expdf$eduact_18, levels=c(1:2), labels=c('In education or training','Not in education or training'))

expdf$eduact_20 <- ifelse(exp$CCU4055 %in% c(1:6), 1, NA)
expdf$eduact_20 <- ifelse(exp$CCU4055 %in% 7, 2, expdf$eduact_20)
expdf$eduact_20 <- factor(expdf$eduact_20, levels=c(1:2), labels=c('In education or training','Not in education or training'))

expdf$eduact_21 <- ifelse(exp$YPA8010 %in% c(1:6), 1, NA)
expdf$eduact_21 <- ifelse(exp$YPA8010 %in% 7, 2, expdf$eduact_21)
expdf$eduact_21 <- factor(expdf$eduact_21, levels=c(1:2), labels=c('In education or training','Not in education or training'))

expdf$econact_17 <- ifelse(exp$ccs7500 %in% 1 | exp$ccs7510 %in% 1 | exp$ccs7523 %in% 1 | exp$ccs7524 %in% 1, 1, NA)
expdf$econact_17 <- ifelse(is.na(expdf$econact_17) & (exp$ccs7520 %in% 1 | exp$ccs7521 %in% 1 | exp$ccs7522 %in% 1), 2, expdf$econact_17)
expdf$econact_17 <- factor(expdf$econact_17, levels=c(1:2), labels=c('Employed/Education','Unemployed/Disabled/Volunteer'))

expdf$econact_22 <- ifelse(exp$YPB9000 %in% 1 | exp$YPB9001 %in% 1 | exp$YPB9002 %in% 1 | exp$YPB9005 %in% 1 | exp$YPB9007 %in% 1, 1, NA)
expdf$econact_22 <- ifelse(is.na(expdf$econact_22) & (exp$YPB9003 %in% 1 | exp$YPB9004 %in% 1 | exp$YPB9006 %in% 1 | exp$YPB9008 %in% 1), 2, expdf$econact_22)
expdf$econact_22 <- factor(expdf$econact_22, levels=c(1:2), labels=c('Employed/Education','Unemployed/Disabled/Volunteer/Carer'))

expdf$econact_23 <- ifelse(exp$YPC2450 %in% 1 | exp$YPC2451 %in% 1 | exp$YPC2452 %in% 1 | exp$YPC2453 %in% 1 | exp$YPC2456 %in% 1 | exp$YPC2458 %in% 1, 1, NA)
expdf$econact_23 <- ifelse(is.na(expdf$econact_23) & (exp$YPC2454 %in% 1 | exp$YPC2455 %in% 1 | exp$YPC2457 %in% 1 | exp$YPC2459 %in% 1), 2, expdf$econact_23)
expdf$econact_23 <- factor(expdf$econact_23, levels=c(1:2), labels=c('Employed/Education','Unemployed/Disabled/Volunteer/Carer'))

expdf$econact_25 <- ifelse(exp$YPE6000 %in% 1 | exp$YPE6001 %in% 1 | exp$YPE6002 %in% 1 | exp$YPE6003 %in% 1 | exp$YPE6006 %in% 1 | exp$YPE6008 %in% 1, 1, NA)
expdf$econact_25 <- ifelse(is.na(expdf$econact_25) & (exp$YPE6004 %in% 1 | exp$YPE6005 %in% 1 | exp$YPE6007 %in% 1 | exp$YPE6009 %in% 1), 2, expdf$econact_25)
expdf$econact_25 <- factor(expdf$econact_25, levels=c(1:2), labels=c('Employed/Education','Unemployed/Disabled/Volunteer/Carer'))

expdf$workact_18 <- ifelse(exp$cct3100 %in% c(1:3,7), 1, NA)
expdf$workact_18 <- ifelse(exp$cct3100 %in% c(4:6), 2, expdf$workact_18)
expdf$workact_18 <- factor(expdf$workact_18, levels=c(1:2), labels=c('In work/Education','Unemployed/Disabled/Looking after family'))

expdf$workact_20 <- ifelse(exp$CCU4060 %in% c(1:3,5), 1, NA)
expdf$workact_20 <- ifelse(exp$CCU4060 %in% c(4), 2, expdf$workact_20)
expdf$workact_20 <- factor(expdf$workact_20, levels=c(1:2), labels=c('In work/Education','Unemployed'))

expdf$workact_21 <- ifelse(exp$YPA8020 %in% c(1:3,5), 1, NA)
expdf$workact_21 <- ifelse(exp$YPA8020 %in% c(4), 2, expdf$workact_21)
expdf$workact_21 <- factor(expdf$workact_21, levels=c(1:2), labels=c('In work/Education','Unemployed'))

expdf$employst_23 <- ifelse(exp$YPC2493 %in% c(1:2), 1, NA)
expdf$employst_23 <- ifelse(exp$YPC2493 %in% c(3:4), 2, expdf$employst_23)
expdf$employst_23 <- factor(expdf$employst_23, levels=c(1:2), labels=c('Employer/Manager','Employee'))

expdf$income_25 <- ifelse(exp$YPE6020 %in% c(1:3), 1, NA)
expdf$income_25 <- ifelse(exp$YPE6020 %in% c(4:7), 2, expdf$income_25)
expdf$income_25 <- factor(expdf$income_25, levels=c(1:2), labels=c('<??1500 per month','>=??1500 per month'))

#NS-SEC occupational class
expdf$nssec_all_23 <- ifelse(exp$YPC2491 %in% c(1:3), 1, NA)
expdf$nssec_all_23 <- ifelse(exp$YPC2491 %in% c(4:7), 2, expdf$nssec_all_23)
expdf$nssec_all_23 <- factor(expdf$nssec_all_23, levels=c(1:2), labels=c('Managerial/Admin/Professional/Intermediate/Employer','Lower supervisory/Technical/Semi-routine/Routine'))

expdf$nssec_5_23 <- ifelse(exp$YPC2492 %in% c(1:3), 1, NA)
expdf$nssec_5_23 <- ifelse(exp$YPC2492 %in% c(4:5), 2, expdf$nssec_5_23)
expdf$nssec_5_23 <- factor(expdf$nssec_5_23, levels=c(1:2), labels=c('Managerial/Admin/Professional/Intermediate/Employer','Lower supervisory/Technical/Semi-routine/Routine'))

#job
expdf$emply_25 <- factor(exp$YPE7470, levels=c(1:0), c('Yes','No'))

expdf$shiftwrk_25 <- ifelse(exp$YPE7471 %in% 0, 1, NA)
expdf$shiftwrk_25 <- ifelse(exp$YPE7471 %in% c(1:3), 2, expdf$shiftwrk_25)
expdf$shiftwrk_25 <- factor(expdf$shiftwrk_25, levels=c(1:2), labels=c('Never or rarely','Sometimes or more'))

expdf$nightwrk_25 <- ifelse(exp$YPE7472 %in% 0, 1, NA)
expdf$nightwrk_25 <- ifelse(exp$YPE7472 %in% c(1:3), 2, expdf$nightwrk_25)
expdf$nightwrk_25 <- factor(expdf$nightwrk_25, levels=c(1:2), labels=c('Never or rarely','Sometimes or more'))


##Neighbourhood deprivation
#Neighbourhood quality index
expdf$hoodqual_gest <- as.numeric(exp$a636)
expdf$hoodqual_gest[expdf$hoodqual_gest<0] <- NA
expdf$hoodqual_gest <- factor(ifelse(expdf$hoodqual_gest > 9, 1, 0), levels=c(1:0), labels=c('>9','<=9'))

#Neighbourhood stress score
expdf$hoodstress_2 <- as.numeric(exp$g496)
expdf$hoodstress_2[expdf$hoodstress_2<0] <- NA
expdf$hoodstress_2 <- factor(ifelse(expdf$hoodstress_2 > 3, 0, 1), levels=c(1:0), labels=c('<=3','>3'))

expdf$hoodstress_3 <- as.numeric(exp$h366)
expdf$hoodstress_3[expdf$hoodstress_3<0] <- NA
expdf$hoodstress_3 <- factor(ifelse(expdf$hoodstress_3 > 3, 0, 1), levels=c(1:0), labels=c('<=3','>3'))

#G0 Urban/rural (postcode)
expdf$m_urbrur_12w_gest <- ifelse(exp$aur01ind %in% 1, 1, NA)
expdf$m_urbrur_12w_gest <- ifelse(exp$aur01ind %in% c(2:4), 2, expdf$m_urbrur_12w_gest)
expdf$m_urbrur_12w_gest <- factor(expdf$m_urbrur_12w_gest, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_18w_gest <- ifelse(exp$bur01ind %in% 1, 1, NA)
expdf$m_urbrur_18w_gest <- ifelse(exp$bur01ind %in% c(2:4), 2, expdf$m_urbrur_18w_gest)
expdf$m_urbrur_18w_gest <- factor(expdf$m_urbrur_18w_gest, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_32w_gest <- ifelse(exp$cur01ind %in% 1, 1, NA)
expdf$m_urbrur_32w_gest <- ifelse(exp$cur01ind %in% c(2:4), 2, expdf$m_urbrur_32w_gest)
expdf$m_urbrur_32w_gest <- factor(expdf$m_urbrur_32w_gest, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_8to42w_gest <- ifelse(exp$dur01ind %in% 1, 1, NA)
expdf$m_urbrur_8to42w_gest <- ifelse(exp$dur01ind %in% c(2:4), 2, expdf$m_urbrur_8to42w_gest)
expdf$m_urbrur_8to42w_gest <- factor(expdf$m_urbrur_8to42w_gest, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_8w <- ifelse(exp$eur01ind %in% 1, 1, NA)
expdf$m_urbrur_8w <- ifelse(exp$eur01ind %in% c(2:4), 2, expdf$m_urbrur_8w)
expdf$m_urbrur_8w <- factor(expdf$m_urbrur_8w, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_1 <- ifelse(exp$fur01ind %in% 1, 1, NA)
expdf$m_urbrur_1 <- ifelse(exp$fur01ind %in% c(2:4), 2, expdf$m_urbrur_1)
expdf$m_urbrur_1 <- factor(expdf$m_urbrur_1, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_2 <- ifelse(exp$gur01ind %in% 1, 1, NA)
expdf$m_urbrur_2 <- ifelse(exp$gur01ind %in% c(2:4), 2, expdf$m_urbrur_2)
expdf$m_urbrur_2 <- factor(expdf$m_urbrur_2, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_3 <- ifelse(exp$hur01ind %in% 1, 1, NA)
expdf$m_urbrur_3 <- ifelse(exp$hur01ind %in% c(2:4), 2, expdf$m_urbrur_3)
expdf$m_urbrur_3 <- factor(expdf$m_urbrur_3, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_4 <- ifelse(exp$jur01ind %in% 1, 1, NA)
expdf$m_urbrur_4 <- ifelse(exp$jur01ind %in% c(2:4), 2, expdf$m_urbrur_4)
expdf$m_urbrur_4 <- factor(expdf$m_urbrur_4, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_7 <- ifelse(exp$mur01ind %in% 1, 1, NA)
expdf$m_urbrur_7 <- ifelse(exp$mur01ind %in% c(2:4), 2, expdf$m_urbrur_7)
expdf$m_urbrur_7 <- factor(expdf$m_urbrur_7, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_8 <- ifelse(exp$nur01ind %in% 1, 1, NA)
expdf$m_urbrur_8 <- ifelse(exp$nur01ind %in% c(2:4), 2, expdf$m_urbrur_8)
expdf$m_urbrur_8 <- factor(expdf$m_urbrur_8, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_9 <- ifelse(exp$pur01ind %in% 1, 1, NA)
expdf$m_urbrur_9 <- ifelse(exp$pur01ind %in% c(2:4), 2, expdf$m_urbrur_9)
expdf$m_urbrur_9 <- factor(expdf$m_urbrur_9, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_10 <- ifelse(exp$qur01ind %in% 1, 1, NA)
expdf$m_urbrur_10 <- ifelse(exp$qur01ind %in% c(2:4), 2, expdf$m_urbrur_10)
expdf$m_urbrur_10 <- factor(expdf$m_urbrur_10, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_11 <- ifelse(exp$rur01ind %in% 1, 1, NA)
expdf$m_urbrur_11 <- ifelse(exp$rur01ind %in% c(2:4), 2, expdf$m_urbrur_11)
expdf$m_urbrur_11 <- factor(expdf$m_urbrur_11, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_12 <- ifelse(exp$sur01ind %in% 1, 1, NA)
expdf$m_urbrur_12 <- ifelse(exp$sur01ind %in% c(2:4), 2, expdf$m_urbrur_12)
expdf$m_urbrur_12 <- factor(expdf$m_urbrur_12, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_18 <- ifelse(exp$tur01ind %in% 1, 1, NA)
expdf$m_urbrur_18 <- ifelse(exp$tur01ind %in% c(2:4), 2, expdf$m_urbrur_18)
expdf$m_urbrur_18 <- factor(expdf$m_urbrur_18, levels=c(1:2), labels=c('Urban','Town or Rural'))

#G1 Urban/rural (postcode)
expdf$urbrur_ccb_8 <- ifelse(exp$ccbur01ind %in% 1, 1, NA)
expdf$urbrur_ccb_8 <- ifelse(exp$ccbur01ind %in% c(2:4), 2, expdf$urbrur_ccb_8)
expdf$urbrur_ccb_8 <- factor(expdf$urbrur_ccb_8, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccc_8 <- ifelse(exp$cccur01ind %in% 1, 1, NA)
expdf$urbrur_ccc_8 <- ifelse(exp$cccur01ind %in% c(2:4), 2, expdf$urbrur_ccc_8)
expdf$urbrur_ccc_8 <- factor(expdf$urbrur_ccc_8, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccd_9 <- ifelse(exp$ccdur01ind %in% 1, 1, NA)
expdf$urbrur_ccd_9 <- ifelse(exp$ccdur01ind %in% c(2:4), 2, expdf$urbrur_ccd_9)
expdf$urbrur_ccd_9 <- factor(expdf$urbrur_ccd_9, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_cce_9 <- ifelse(exp$cceur01ind %in% 1, 1, NA)
expdf$urbrur_cce_9 <- ifelse(exp$cceur01ind %in% c(2:4), 2, expdf$urbrur_cce_9)
expdf$urbrur_cce_9 <- factor(expdf$urbrur_cce_9, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccf_10 <- ifelse(exp$ccfur01ind %in% 1, 1, NA)
expdf$urbrur_ccf_10 <- ifelse(exp$ccfur01ind %in% c(2:4), 2, expdf$urbrur_ccf_10)
expdf$urbrur_ccf_10 <- factor(expdf$urbrur_ccf_10, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccg_10 <- ifelse(exp$ccgur01ind %in% 1, 1, NA)
expdf$urbrur_ccg_10 <- ifelse(exp$ccgur01ind %in% c(2:4), 2, expdf$urbrur_ccg_10)
expdf$urbrur_ccg_10 <- factor(expdf$urbrur_ccg_10, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_11 <- ifelse(exp$cchur01ind %in% 1, 1, NA)
expdf$urbrur_11 <- ifelse(exp$cchur01ind %in% c(2:4), 2, expdf$urbrur_11)
expdf$urbrur_11 <- factor(expdf$urbrur_11, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_cck_12 <- ifelse(exp$cckur01ind %in% 1, 1, NA)
expdf$urbrur_cck_12 <- ifelse(exp$cckur01ind %in% c(2:4), 2, expdf$urbrur_cck_12)
expdf$urbrur_cck_12 <- factor(expdf$urbrur_cck_12, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccl_12 <- ifelse(exp$cclur01ind %in% 1, 1, NA)
expdf$urbrur_ccl_12 <- ifelse(exp$cclur01ind %in% c(2:4), 2, expdf$urbrur_ccl_12)
expdf$urbrur_ccl_12 <- factor(expdf$urbrur_ccl_12, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccm_13 <- ifelse(exp$ccmur01ind %in% 1, 1, NA)
expdf$urbrur_ccm_13 <- ifelse(exp$ccmur01ind %in% c(2:4), 2, expdf$urbrur_ccm_13)
expdf$urbrur_ccm_13 <- factor(expdf$urbrur_ccm_13, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccn_13 <- ifelse(exp$ccnur01ind %in% 1, 1, NA)
expdf$urbrur_ccn_13 <- ifelse(exp$ccnur01ind %in% c(2:4), 2, expdf$urbrur_ccn_13)
expdf$urbrur_ccn_13 <- factor(expdf$urbrur_ccn_13, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccp_14 <- ifelse(exp$ccpur01ind %in% 1, 1, NA)
expdf$urbrur_ccp_14 <- ifelse(exp$ccpur01ind %in% c(2:4), 2, expdf$urbrur_ccp_14)
expdf$urbrur_ccp_14 <- factor(expdf$urbrur_ccp_14, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccq_14 <- ifelse(exp$ccqur01ind %in% 1, 1, NA)
expdf$urbrur_ccq_14 <- ifelse(exp$ccqur01ind %in% c(2:4), 2, expdf$urbrur_ccq_14)
expdf$urbrur_ccq_14 <- factor(expdf$urbrur_ccq_14, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_ccr_14 <- ifelse(exp$ccqur01ind %in% 1, 1, NA)
expdf$urbrur_ccr_14 <- ifelse(exp$ccqur01ind %in% c(2:4), 2, expdf$urbrur_ccr_14)
expdf$urbrur_ccr_14 <- factor(expdf$urbrur_ccr_14, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_17 <- ifelse(exp$ccsur01ind %in% 1, 1, NA)
expdf$urbrur_17 <- ifelse(exp$ccsur01ind %in% c(2:4), 2, expdf$urbrur_17)
expdf$urbrur_17 <- factor(expdf$urbrur_17, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_18 <- ifelse(exp$cctur01ind %in% 1, 1, NA)
expdf$urbrur_18 <- ifelse(exp$cctur01ind %in% c(2:4), 2, expdf$urbrur_18)
expdf$urbrur_18 <- factor(expdf$urbrur_18, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_20 <- ifelse(exp$ccuur01ind %in% 1, 1, NA)
expdf$urbrur_20 <- ifelse(exp$ccuur01ind %in% c(2:4), 2, expdf$urbrur_20)
expdf$urbrur_20 <- factor(expdf$urbrur_20, levels=c(1:2), labels=c('Urban','Town or Rural'))

#G0 Townsend score (postcode)
expdf$m_townsend_12w_gest <- ifelse(exp$aTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_12w_gest <- ifelse(exp$aTownsendq5 %in% c(4:5), 2, expdf$m_townsend_12w_gest)
expdf$m_townsend_12w_gest <- factor(expdf$m_townsend_12w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_18w_gest <- ifelse(exp$bTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_18w_gest <- ifelse(exp$bTownsendq5 %in% c(4:5), 2, expdf$m_townsend_18w_gest)
expdf$m_townsend_18w_gest <- factor(expdf$m_townsend_18w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_32w_gest <- ifelse(exp$cTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_32w_gest <- ifelse(exp$cTownsendq5 %in% c(4:5), 2, expdf$m_townsend_32w_gest)
expdf$m_townsend_32w_gest <- factor(expdf$m_townsend_32w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_8to42w_gest <- ifelse(exp$dTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_8to42w_gest <- ifelse(exp$dTownsendq5 %in% c(4:5), 2, expdf$m_townsend_8to42w_gest)
expdf$m_townsend_8to42w_gest <- factor(expdf$m_townsend_8to42w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_8w <- ifelse(exp$eTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_8w <- ifelse(exp$eTownsendq5 %in% c(4:5), 2, expdf$m_townsend_8w)
expdf$m_townsend_8w <- factor(expdf$m_townsend_8w, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_1 <- ifelse(exp$fTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_1 <- ifelse(exp$fTownsendq5 %in% c(4:5), 2, expdf$m_townsend_1)
expdf$m_townsend_1 <- factor(expdf$m_townsend_1, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_2 <- ifelse(exp$gTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_2 <- ifelse(exp$gTownsendq5 %in% c(4:5), 2, expdf$m_townsend_2)
expdf$m_townsend_2 <- factor(expdf$m_townsend_2, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_3 <- ifelse(exp$hTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_3 <- ifelse(exp$hTownsendq5 %in% c(4:5), 2, expdf$m_townsend_3)
expdf$m_townsend_3 <- factor(expdf$m_townsend_3, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_4 <- ifelse(exp$jTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_4 <- ifelse(exp$jTownsendq5 %in% c(4:5), 2, expdf$m_townsend_4)
expdf$m_townsend_4 <- factor(expdf$m_townsend_4, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_5 <- ifelse(exp$kTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_5 <- ifelse(exp$kTownsendq5 %in% c(4:5), 2, expdf$m_townsend_5)
expdf$m_townsend_5 <- factor(expdf$m_townsend_5, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_6 <- ifelse(exp$lTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_6 <- ifelse(exp$lTownsendq5 %in% c(4:5), 2, expdf$m_townsend_6)
expdf$m_townsend_6 <- factor(expdf$m_townsend_6, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_7 <- ifelse(exp$mTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_7 <- ifelse(exp$mTownsendq5 %in% c(4:5), 2, expdf$m_townsend_7)
expdf$m_townsend_7 <- factor(expdf$m_townsend_7, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_8 <- ifelse(exp$nTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_8 <- ifelse(exp$nTownsendq5 %in% c(4:5), 2, expdf$m_townsend_8)
expdf$m_townsend_8 <- factor(expdf$m_townsend_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_9 <- ifelse(exp$pTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_9 <- ifelse(exp$pTownsendq5 %in% c(4:5), 2, expdf$m_townsend_9)
expdf$m_townsend_9 <- factor(expdf$m_townsend_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_10 <- ifelse(exp$qTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_10 <- ifelse(exp$qTownsendq5 %in% c(4:5), 2, expdf$m_townsend_10)
expdf$m_townsend_10 <- factor(expdf$m_townsend_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_11 <- ifelse(exp$rTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_11 <- ifelse(exp$rTownsendq5 %in% c(4:5), 2, expdf$m_townsend_11)
expdf$m_townsend_11 <- factor(expdf$m_townsend_11, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_12 <- ifelse(exp$sTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_12 <- ifelse(exp$sTownsendq5 %in% c(4:5), 2, expdf$m_townsend_12)
expdf$m_townsend_12 <- factor(expdf$m_townsend_12, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_townsend_18 <- ifelse(exp$tTownsendq5 %in% c(1:3), 1, NA)
expdf$m_townsend_18 <- ifelse(exp$tTownsendq5 %in% c(4:5), 2, expdf$m_townsend_18)
expdf$m_townsend_18 <- factor(expdf$m_townsend_18, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

#G0 IMD score (postcode)
expdf$m_IMD_12w_gest <- ifelse(exp$aimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_12w_gest <- ifelse(exp$aimd2000q5 %in% c(4:5), 2, expdf$m_IMD_12w_gest)
expdf$m_IMD_12w_gest <- factor(expdf$m_IMD_12w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_18w_gest <- ifelse(exp$bimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_18w_gest <- ifelse(exp$bimd2000q5 %in% c(4:5), 2, expdf$m_IMD_18w_gest)
expdf$m_IMD_18w_gest <- factor(expdf$m_IMD_18w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_32w_gest <- ifelse(exp$cimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_32w_gest <- ifelse(exp$cimd2000q5 %in% c(4:5), 2, expdf$m_IMD_32w_gest)
expdf$m_IMD_32w_gest <- factor(expdf$m_IMD_32w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_8to42w_gest <- ifelse(exp$dimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_8to42w_gest <- ifelse(exp$dimd2000q5 %in% c(4:5), 2, expdf$m_IMD_8to42w_gest)
expdf$m_IMD_8to42w_gest <- factor(expdf$m_IMD_8to42w_gest, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_8w <- ifelse(exp$eimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_8w <- ifelse(exp$eimd2000q5 %in% c(4:5), 2, expdf$m_IMD_8w)
expdf$m_IMD_8w <- factor(expdf$m_IMD_8w, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_1 <- ifelse(exp$fimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_1 <- ifelse(exp$fimd2000q5 %in% c(4:5), 2, expdf$m_IMD_1)
expdf$m_IMD_1 <- factor(expdf$m_IMD_1, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_2 <- ifelse(exp$gimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_2 <- ifelse(exp$gimd2000q5 %in% c(4:5), 2, expdf$m_IMD_2)
expdf$m_IMD_2 <- factor(expdf$m_IMD_2, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_3 <- ifelse(exp$himd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_3 <- ifelse(exp$himd2000q5 %in% c(4:5), 2, expdf$m_IMD_3)
expdf$m_IMD_3 <- factor(expdf$m_IMD_3, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_5 <- ifelse(exp$kimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_5 <- ifelse(exp$kimd2000q5 %in% c(4:5), 2, expdf$m_IMD_5)
expdf$m_IMD_5 <- factor(expdf$m_IMD_5, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_6 <- ifelse(exp$limd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_6 <- ifelse(exp$limd2000q5 %in% c(4:5), 2, expdf$m_IMD_6)
expdf$m_IMD_6 <- factor(expdf$m_IMD_6, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_7 <- ifelse(exp$mimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_7 <- ifelse(exp$mimd2000q5 %in% c(4:5), 2, expdf$m_IMD_7)
expdf$m_IMD_7 <- factor(expdf$m_IMD_7, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_8 <- ifelse(exp$nimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_8 <- ifelse(exp$nimd2000q5 %in% c(4:5), 2, expdf$m_IMD_8)
expdf$m_IMD_8 <- factor(expdf$m_IMD_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_9 <- ifelse(exp$pimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_9 <- ifelse(exp$pimd2000q5 %in% c(4:5), 2, expdf$m_IMD_9)
expdf$m_IMD_9 <- factor(expdf$m_IMD_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_10 <- ifelse(exp$qimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_10 <- ifelse(exp$qimd2000q5 %in% c(4:5), 2, expdf$m_IMD_10)
expdf$m_IMD_10 <- factor(expdf$m_IMD_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_11 <- ifelse(exp$rimd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_11 <- ifelse(exp$rimd2000q5 %in% c(4:5), 2, expdf$m_IMD_11)
expdf$m_IMD_11 <- factor(expdf$m_IMD_11, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$m_IMD_18 <- ifelse(exp$timd2000q5 %in% c(1:3), 1, NA)
expdf$m_IMD_18 <- ifelse(exp$timd2000q5 %in% c(4:5), 2, expdf$m_IMD_18)
expdf$m_IMD_18 <- factor(expdf$m_IMD_18, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

#G1 Townsend score (postcode)
expdf$townsend_ccb_8 <- ifelse(exp$ccbTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccb_8 <- ifelse(exp$ccbTownsendq5 %in% c(4:5), 2, expdf$townsend_ccb_8)
expdf$townsend_ccb_8 <- factor(expdf$townsend_ccb_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccc_8 <- ifelse(exp$cccTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccc_8 <- ifelse(exp$cccTownsendq5 %in% c(4:5), 2, expdf$townsend_ccc_8)
expdf$townsend_ccc_8 <- factor(expdf$townsend_ccc_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccd_9 <- ifelse(exp$ccdTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccd_9 <- ifelse(exp$ccdTownsendq5 %in% c(4:5), 2, expdf$townsend_ccd_9)
expdf$townsend_ccd_9 <- factor(expdf$townsend_ccd_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_cce_9 <- ifelse(exp$cceTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_cce_9 <- ifelse(exp$cceTownsendq5 %in% c(4:5), 2, expdf$townsend_cce_9)
expdf$townsend_cce_9 <- factor(expdf$townsend_cce_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccf_10 <- ifelse(exp$ccfTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccf_10 <- ifelse(exp$ccfTownsendq5 %in% c(4:5), 2, expdf$townsend_ccf_10)
expdf$townsend_ccf_10 <- factor(expdf$townsend_ccf_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccg_10 <- ifelse(exp$ccgTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccg_10 <- ifelse(exp$ccgTownsendq5 %in% c(4:5), 2, expdf$townsend_ccg_10)
expdf$townsend_ccg_10 <- factor(expdf$townsend_ccg_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_11 <- ifelse(exp$cchTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_11 <- ifelse(exp$cchTownsendq5 %in% c(4:5), 2, expdf$townsend_11)
expdf$townsend_11 <- factor(expdf$townsend_11, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_cck_12 <- ifelse(exp$cckTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_cck_12 <- ifelse(exp$cckTownsendq5 %in% c(4:5), 2, expdf$townsend_cck_12)
expdf$townsend_cck_12 <- factor(expdf$townsend_cck_12, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccl_12 <- ifelse(exp$cclTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccl_12 <- ifelse(exp$cclTownsendq5 %in% c(4:5), 2, expdf$townsend_ccl_12)
expdf$townsend_ccl_12 <- factor(expdf$townsend_ccl_12, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccm_13 <- ifelse(exp$ccmTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccm_13 <- ifelse(exp$ccmTownsendq5 %in% c(4:5), 2, expdf$townsend_ccm_13)
expdf$townsend_ccm_13 <- factor(expdf$townsend_ccm_13, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccn_13 <- ifelse(exp$ccnTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccn_13 <- ifelse(exp$ccnTownsendq5 %in% c(4:5), 2, expdf$townsend_ccn_13)
expdf$townsend_ccn_13 <- factor(expdf$townsend_ccn_13, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccp_14 <- ifelse(exp$ccpTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccp_14 <- ifelse(exp$ccpTownsendq5 %in% c(4:5), 2, expdf$townsend_ccp_14)
expdf$townsend_ccp_14 <- factor(expdf$townsend_ccp_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccq_14 <- ifelse(exp$ccqTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccq_14 <- ifelse(exp$ccqTownsendq5 %in% c(4:5), 2, expdf$townsend_ccq_14)
expdf$townsend_ccq_14 <- factor(expdf$townsend_ccq_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_ccr_14 <- ifelse(exp$ccrTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_ccr_14 <- ifelse(exp$ccrTownsendq5 %in% c(4:5), 2, expdf$townsend_ccr_14)
expdf$townsend_ccr_14 <- factor(expdf$townsend_ccr_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_17 <- ifelse(exp$ccsTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_17 <- ifelse(exp$ccsTownsendq5 %in% c(4:5), 2, expdf$townsend_17)
expdf$townsend_17 <- factor(expdf$townsend_17, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_18 <- ifelse(exp$cctTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_18 <- ifelse(exp$cctTownsendq5 %in% c(4:5), 2, expdf$townsend_18)
expdf$townsend_18 <- factor(expdf$townsend_18, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$townsend_20 <- ifelse(exp$ccuTownsendq5 %in% c(1:3), 1, NA)
expdf$townsend_20 <- ifelse(exp$ccuTownsendq5 %in% c(4:5), 2, expdf$townsend_20)
expdf$townsend_20 <- factor(expdf$townsend_20, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

#G1 IMD score (postcode) (1 is least deprived, 5 is most)
expdf$IMD_ccb_8 <- ifelse(exp$ccbimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccb_8 <- ifelse(exp$ccbimd2000q5 %in% c(4:5), 2, expdf$IMD_ccb_8)
expdf$IMD_ccb_8 <- factor(expdf$IMD_ccb_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccc_8 <- ifelse(exp$cccimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccc_8 <- ifelse(exp$cccimd2000q5 %in% c(4:5), 2, expdf$IMD_ccc_8)
expdf$IMD_ccc_8 <- factor(expdf$IMD_ccc_8, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccd_9 <- ifelse(exp$ccdimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccd_9 <- ifelse(exp$ccdimd2000q5 %in% c(4:5), 2, expdf$IMD_ccd_9)
expdf$IMD_ccd_9 <- factor(expdf$IMD_ccd_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_cce_9 <- ifelse(exp$cceimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_cce_9 <- ifelse(exp$cceimd2000q5 %in% c(4:5), 2, expdf$IMD_cce_9)
expdf$IMD_cce_9 <- factor(expdf$IMD_cce_9, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccf_10 <- ifelse(exp$ccfimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccf_10 <- ifelse(exp$ccfimd2000q5 %in% c(4:5), 2, expdf$IMD_ccf_10)
expdf$IMD_ccf_10 <- factor(expdf$IMD_ccf_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccg_10 <- ifelse(exp$ccgimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccg_10 <- ifelse(exp$ccgimd2000q5 %in% c(4:5), 2, expdf$IMD_ccg_10)
expdf$IMD_ccg_10 <- factor(expdf$IMD_ccg_10, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_11 <- ifelse(exp$cchimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_11 <- ifelse(exp$cchimd2000q5 %in% c(4:5), 2, expdf$IMD_11)
expdf$IMD_11 <- factor(expdf$IMD_11, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_cck_12 <- ifelse(exp$cckimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_cck_12 <- ifelse(exp$cckimd2000q5 %in% c(4:5), 2, expdf$IMD_cck_12)
expdf$IMD_cck_12 <- factor(expdf$IMD_cck_12, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccl_12 <- ifelse(exp$cclimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccl_12 <- ifelse(exp$cclimd2000q5 %in% c(4:5), 2, expdf$IMD_ccl_12)
expdf$IMD_ccl_12 <- factor(expdf$IMD_ccl_12, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccm_13 <- ifelse(exp$ccmimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccm_13 <- ifelse(exp$ccmimd2000q5 %in% c(4:5), 2, expdf$IMD_ccm_13)
expdf$IMD_ccm_13 <- factor(expdf$IMD_ccm_13, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccn_13 <- ifelse(exp$ccnimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccn_13 <- ifelse(exp$ccnimd2000q5 %in% c(4:5), 2, expdf$IMD_ccn_13)
expdf$IMD_ccn_13 <- factor(expdf$IMD_ccn_13, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccp_14 <- ifelse(exp$ccpimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccp_14 <- ifelse(exp$ccpimd2000q5 %in% c(4:5), 2, expdf$IMD_ccp_14)
expdf$IMD_ccp_14 <- factor(expdf$IMD_ccp_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccq_14 <- ifelse(exp$ccqimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccq_14 <- ifelse(exp$ccqimd2000q5 %in% c(4:5), 2, expdf$IMD_ccq_14)
expdf$IMD_ccq_14 <- factor(expdf$IMD_ccq_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_ccr_14 <- ifelse(exp$ccrimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_ccr_14 <- ifelse(exp$ccrimd2000q5 %in% c(4:5), 2, expdf$IMD_ccr_14)
expdf$IMD_ccr_14 <- factor(expdf$IMD_ccr_14, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_17 <- ifelse(exp$ccsimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_17 <- ifelse(exp$ccsimd2000q5 %in% c(4:5), 2, expdf$IMD_17)
expdf$IMD_17 <- factor(expdf$IMD_17, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_18 <- ifelse(exp$cctimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_18 <- ifelse(exp$cctimd2000q5 %in% c(4:5), 2, expdf$IMD_18)
expdf$IMD_18 <- factor(expdf$IMD_18, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles

expdf$IMD_20 <- ifelse(exp$ccuimd2000q5 %in% c(1:3), 1, NA)
expdf$IMD_20 <- ifelse(exp$ccuimd2000q5 %in% c(4:5), 2, expdf$IMD_20)
expdf$IMD_20 <- factor(expdf$IMD_20, levels=c(1:2), labels=c('Least deprived','Most deprived')) #quintiles


##Adverse Childhood Experiences 0-16 (Longitudinal)
expdf$aces_scex0to16 <- ifelse(exp$clon120 %in% c(0:4), 1, NA)
expdf$aces_scex0to16 <- ifelse(exp$clon120 %in% c(5:13), 2, expdf$aces_scex0to16)
expdf$aces_scex0to16 <- factor(expdf$aces_scex0to16, levels=c(1:2), labels=c('0-4','5+'))

expdf$aces_catex0to16 <- ifelse(exp$clon121 %in% c(1:2), 1, NA)
expdf$aces_catex0to16 <- ifelse(exp$clon121 %in% c(3:4), 2, expdf$aces_catex0to16)
expdf$aces_catex0to16 <- factor(expdf$aces_catex0to16, levels=c(1:2), labels=c('Low/Low-Mid','Mid-High/High')) #Low = 0-1, Low-Mid=2, Mid-High=3-5, High=6+

expdf$aces_sccl0to16 <- ifelse(exp$clon122 %in% c(0:2), 1, NA)
expdf$aces_sccl0to16 <- ifelse(exp$clon122 %in% c(3:10), 2, expdf$aces_sccl0to16)
expdf$aces_sccl0to16 <- factor(expdf$aces_sccl0to16, levels=c(1:2), labels=c('0-2','3+'))

expdf$aces_catcl0to16 <- ifelse(exp$clon123 %in% c(1:2), 1, NA)
expdf$aces_catcl0to16 <- ifelse(exp$clon123 %in% c(3:4), 2, expdf$aces_catcl0to16)
expdf$aces_catcl0to16 <- factor(expdf$aces_catcl0to16, levels=c(1:2), labels=c('0-1','2+')) 


##Trauma (Longitudinal)
expdf$trauma0to5 <- factor(exp$clon145, levels=c(0:1), labels=c('No','Yes'))
expdf$trauma5to11 <- factor(exp$clon152, levels=c(0:1), labels=c('No','Yes'))
expdf$trauma11to17 <- factor(exp$clon159, levels=c(0:1), labels=c('No','Yes'))
expdf$trauma0to17 <- factor(exp$clon166, levels=c(0:1), labels=c('No','Yes'))


##Age
#child based
#kb879a
#kd990
#kf999
#kj999a
#kl991a
#kn9991a
#kq998a
#kr991a
#ku991a
#kw9991a

#child clinic
#f7003c
#f8003c
#f9003c
#fd003c
#fe003c
#FKAR0010
#FKAR0011
#ff0011a
#fg0011a
#fh0011a
#FJ003a
#FJ003b

#child completed
#ccp991a
#ccp991b
#ccq991a
#ccq991b
#ccr991a
#ccr991b
#ccs9991a
#ccs9991b
#cct9991a
#cct9991c
#CCU9991
#ccxa991a
#ccxa991b
#YPA9020
#YPB9992
#YPC2650
#YPD9650
#YPE9650
#YPE9660
#YPH9520
#YPH9510

#mother based
#a902
#b924
#d991
#e301
#f993
#g990
#h991a
#h991b
#j914
#k9991a
#l9991a
#m9991a
#n9991a
#p9991a
#q9991a
#r9991a
#s9991a
#t9991a
#t9991c
#V9991

##SAVE (N = 511)
#save(expdf, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_measures.rda')
write_dta(data=expdf, path='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_measures.dta')
rm(exp)