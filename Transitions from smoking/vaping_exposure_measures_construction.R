##LOAD
library(haven)
library(labelled)
exp <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/vaping_exp_vars_ALSPAC.dta')
expdf <- data.frame(exp[,c('aln','qlet')])


##Mother no partner
#expdf$no_partner <- factor(exp$c665, levels=c(2:1), labels=c('No','Yes')) #No PTNR


##Mother substance use in pregnancy
#Smoking
expdf$m_pregsmk_0 <- ifelse(exp$b665 %in% 1, 1, NA)
expdf$m_pregsmk_0 <- ifelse(exp$b665 %in% c(2:5), 2, expdf$m_pregsmk_0)
expdf$m_pregsmk_0 <- factor(expdf$m_pregsmk_0, levels=c(1:2), labels=c('No','Yes')) #(Tobacco smoked in 1ST 3MTHS of PREG)
#Alcohol consumption
expdf$m_pregalc_0 <- ifelse(exp$b721 %in% 1, 1, NA)
expdf$m_pregalc_0 <- ifelse(exp$b721 %in% c(2:6), 2, expdf$m_pregalc_0) 
expdf$m_pregalc_0 <- factor(expdf$m_pregalc_0, levels=c(1:2), labels=c('No','Yes')) #(alcohol consumption in 1-3mths this preg)
#Cannabis use
expdf$m_pregcana_0 <- ifelse(exp$b701 %in% 5, 1, NA)
expdf$m_pregcana_0 <- ifelse(exp$b701 %in% c(1:4), 2, expdf$m_pregcana_0)
expdf$m_pregcana_0 <- factor(expdf$m_pregcana_0, levels=c(1:2), labels=c('No','Yes')) #(smoked cannabis in 1-3mths of preg)
#Other drugs
expdf$m_pregdrug_0 <- ifelse(exp$b714 %in% 2, 1, NA)
expdf$m_pregdrug_0 <- ifelse(exp$b714 %in% 1, 2, expdf$m_pregdrug_0)
expdf$m_pregdrug_0 <- factor(expdf$m_pregdrug_0, levels=c(1:2), labels=c('No','Yes')) #(derived drugs during pregnancy b706-b713)
#Any substance
expdf$m_pregsub_0 <- ifelse(expdf$m_pregsmk_0 %in% 'Yes' | expdf$m_pregalc_0 %in% 'Yes' | expdf$m_pregcana_0 %in% 'Yes' | expdf$m_pregdrug_0 %in% 'Yes', 1, NA)
expdf$m_pregsub_0 <- ifelse(is.na(expdf$m_pregsub_0) & (expdf$m_pregsmk_0 %in% 'No' | expdf$m_pregalc_0 %in% 'No' | expdf$m_pregcana_0 %in% 'No' | expdf$m_pregdrug_0 %in% 'No'), 2, expdf$m_pregsub_0)
expdf$m_pregsub_0 <- factor(expdf$m_pregsub_0, levels=c(1:2), labels=c('No','Yes'))


##Parental daily smoking
#Mother daily smoking
expdf$m_dailsmk_2 <- ifelse(exp$g820 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_2 <- ifelse(is.na(expdf$m_dailsmk_2) & exp$g820 %in% 0, 2, expdf$m_dailsmk_2)
expdf$m_dailsmk_2 <- factor(expdf$m_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day)

expdf$m_dailsmk_12 <- ifelse(exp$s1300 %in% 0 & exp$s1301 %in% 0, 1, NA)
expdf$m_dailsmk_12 <- ifelse(!is.na(exp$s1300) & !is.na(exp$s1301) & (exp$s1300 > 0 | exp$s1301 > 0), 2, expdf$m_dailsmk_12)
expdf$m_dailsmk_12 <- factor(expdf$m_dailsmk_12, levels=c(1:2), labels=c('No','Yes')) #(number of cigs smokes per day weekdays | weekends)
#Partner daily smoking
expdf$p_dailsmk_2 <- ifelse(exp$g648 %in% c(1:20,22,24:25,28,30,35,40,50,60), 1, NA)
expdf$p_dailsmk_2 <- ifelse(is.na(expdf$p_dailsmk_2) & exp$g648 %in% 0, 2, expdf$p_dailsmk_2)
expdf$p_dailsmk_2 <- factor(expdf$p_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(n cigs partner smokers every day)

expdf$p_dailsmk_12 <- ifelse(exp$pq1300 %in% 0 & exp$pq1301 %in% 0, 2, NA)
expdf$p_dailsmk_12 <- ifelse(!is.na(exp$pq1300) & !is.na(exp$pq1301) & (exp$pq1300 > 0 | exp$pq1301 > 0), 1, expdf$p_dailsmk_12)
expdf$p_dailsmk_12 <- factor(expdf$p_dailsmk_12, levels=c(2:1), labels=c('No','Yes'))
#Parental daily smoking
expdf$parent_dailsmk_2 <- ifelse((expdf$m_dailsmk_2 %in% 'No' & expdf$p_dailsmk_2 %in% NA) | 
                                   (expdf$m_dailsmk_2 %in% NA & expdf$p_dailsmk_2 %in% 'No') |
                                   (expdf$m_dailsmk_2 %in% 'No' & expdf$p_dailsmk_2 %in% 'No'), 1, NA)
expdf$parent_dailsmk_2 <- ifelse(expdf$m_dailsmk_2 %in% 'Yes' | expdf$p_dailsmk_2 %in% 'Yes', 2, expdf$parent_dailsmk_2)
expdf$parent_dailsmk_2 <- factor(expdf$parent_dailsmk_2, levels=c(1:2), labels=c('No','Yes'))

expdf$parent_dailsmk_12 <- ifelse((expdf$m_dailsmk_12 %in% 'No' & expdf$p_dailsmk_12 %in% NA) | 
                                    (expdf$m_dailsmk_12 %in% NA & expdf$p_dailsmk_12 %in% 'No') |
                                    (expdf$m_dailsmk_12 %in% 'No' & expdf$p_dailsmk_12 %in% 'No'), 1, NA)
expdf$parent_dailsmk_12 <- ifelse(expdf$m_dailsmk_12 %in% 'Yes' | expdf$p_dailsmk_12 %in% 'Yes', 2, expdf$parent_dailsmk_12)
expdf$parent_dailsmk_12 <- factor(expdf$parent_dailsmk_12, levels=c(1:2), labels=c('No','Yes'))


##Parental current smoking
#Mother current smoking
expdf$m_currsmk_18 <- factor(exp$t5520, levels=c(2:1), labels=c('No','Yes')) #currently a smoker
#Partner current smoking
expdf$p_currsmk_18 <- factor(exp$fpa5520, levels=c(2:1), labels=c('No','Yes')) #currently a smoker
#Parental current smoking
expdf$parent_currsmk_18 <- ifelse((expdf$m_currsmk_18 %in% 'No' & expdf$p_currsmk_18 %in% NA) | 
                                    (expdf$m_currsmk_18 %in% NA & expdf$p_currsmk_18 %in% 'No') |
                                    (expdf$m_currsmk_18 %in% 'No' & expdf$p_currsmk_18 %in% 'No'), 1, NA)
expdf$parent_currsmk_18 <- ifelse(expdf$m_currsmk_18 %in% 'Yes' | expdf$p_currsmk_18 %in% 'Yes', 2, expdf$parent_currsmk_18)
expdf$parent_currsmk_18 <- factor(expdf$parent_currsmk_18, levels=c(1:2), labels=c('No','Yes'))


##Parental alcohol consumption
#Mother alcohol consumption
expdf$m_alc_12 <- ifelse(exp$pq3190 %in% 1, 1, NA)
expdf$m_alc_12 <- ifelse(exp$pq3190 %in% c(2:3), 2, expdf$m_alc_12)
expdf$m_alc_12 <- ifelse(exp$pq3190 %in% c(4:6), 3, expdf$m_alc_12)
expdf$m_alc_12 <- factor(expdf$m_alc_12, levels=c(1:3), labels=c('Never','Weekly or less','Nearly every day/Every day')) #Amount of alcohol drunk by partner's wife/partner (don't know excluded)

expdf$m_alc_18 <- ifelse(exp$t5500 %in% 1, 1, NA)
expdf$m_alc_18 <- ifelse(exp$t5500 %in% c(2:3), 2, expdf$m_alc_18)
expdf$m_alc_18 <- ifelse(exp$t5500 %in% c(4:5), 3, expdf$m_alc_18)
expdf$m_alc_18 <- factor(expdf$m_alc_18, levels=c(1:3), labels=c('Never','Monthly or less','More than weekly')) #Frequency of alcohol consumption
#Partner alcohol consumption
expdf$p_alc_12 <- ifelse(exp$s3190 %in% 1, 1, NA)
expdf$p_alc_12 <- ifelse(exp$s3190 %in% c(2:3), 2, expdf$p_alc_12)
expdf$p_alc_12 <- ifelse(exp$s3190 %in% c(4:6), 3, expdf$p_alc_12)
expdf$p_alc_12 <- factor(expdf$p_alc_12, levels=c(1:3), labels=c('Never','Weekly or less','Nearly every day/Every day')) #Frequency alcohol drunk by mother's partner (don't know excluded)

expdf$p_alc_18 <- ifelse(exp$fpa5500 %in% 1, 1, NA)
expdf$p_alc_18 <- ifelse(exp$fpa5500 %in% c(2:3), 2, expdf$p_alc_18)
expdf$p_alc_18 <- ifelse(exp$fpa5500 %in% c(4:5), 3, expdf$p_alc_18)
expdf$p_alc_18 <- factor(expdf$p_alc_18, levels=c(1:3), labels=c('Never','Monthly or less','More than weekly')) #Frequency consume drink containing alcohol
#Parental alcohol consumption
expdf$parent_alc_12 <- ifelse((expdf$m_alc_12 %in% 'Never' & expdf$p_alc_12 %in% NA) | 
                                (expdf$m_alc_12 %in% NA & expdf$p_alc_12 %in% 'Never') |
                                (expdf$m_alc_12 %in% 'Never' & expdf$p_alc_12 %in% 'Never'), 1, NA)
expdf$parent_alc_12 <- ifelse((expdf$m_alc_12 %in% 'Weekly or less' & expdf$p_alc_12 %in% NA) | 
                                (expdf$m_alc_12 %in% NA & expdf$p_alc_12 %in% 'Weekly or less') |
                                (expdf$m_alc_12 %in% 'Weekly or less' & expdf$p_alc_12 %in% 'Weekly or less') |
                                (expdf$m_alc_12 %in% 'Never' & expdf$p_alc_12 %in% 'Weekly or less') |
                                (expdf$m_alc_12 %in% 'Weekly or less' & expdf$p_alc_12 %in% 'Never'), 2, expdf$parent_alc_12)
expdf$parent_alc_12 <- ifelse((expdf$m_alc_12 %in% 'Nearly every day/Every day' | expdf$p_alc_12 %in% 'Nearly every day/Every day'), 3, expdf$parent_alc_12)
expdf$parent_alc_12 <- factor(expdf$parent_alc_12, levels=c(1:3), labels=c('Never','Weekly or less','Nearly every day/Every day'))

expdf$parent_alc_18 <- ifelse((expdf$m_alc_18 %in% 'Never' & expdf$p_alc_18 %in% NA) | 
                                (expdf$m_alc_18 %in% NA & expdf$p_alc_18 %in% 'Never') |
                                (expdf$m_alc_18 %in% 'Never' & expdf$p_alc_18 %in% 'Never'), 1, NA)
expdf$parent_alc_18 <- ifelse((expdf$m_alc_18 %in% 'Monthly or less' & expdf$p_alc_18 %in% NA) | 
                                (expdf$m_alc_18 %in% NA & expdf$p_alc_18 %in% 'Monthly or less') |
                                (expdf$m_alc_18 %in% 'Monthly or less' & expdf$p_alc_18 %in% 'Monthly or less') |
                                (expdf$m_alc_18 %in% 'Never' & expdf$p_alc_18 %in% 'Monthly or less') |
                                (expdf$m_alc_18 %in% 'Monthly or less' & expdf$p_alc_18 %in% 'Never'), 2, expdf$parent_alc_18)
expdf$parent_alc_18 <- ifelse((expdf$m_alc_18 %in% 'More than weekly' | expdf$p_alc_18 %in% 'More than weekly'), 3, expdf$parent_alc_18)
expdf$parent_alc_18 <- factor(expdf$parent_alc_18, levels=c(1:3), labels=c('Never','Monthly or less','More than weekly'))


#Parental cannabis use
#Mother cannabis consumption
expdf$m_cana_9 <- ifelse(exp$p1052 %in% 4, 1, NA)
expdf$m_cana_9 <- ifelse(exp$p1052 %in% c(1:3), 2, expdf$m_cana_9)
expdf$m_cana_9 <- factor(expdf$m_cana_9, levels=c(1:2), labels=c('No','Yes')) #Frequency mother has taken cannabis/marihuana in last 2 years

expdf$m_cana_18 <- ifelse(exp$t5402 %in% 4, 1, NA)
expdf$m_cana_18 <- ifelse(exp$t5402 %in% c(1:3), 2, expdf$m_cana_18)
expdf$m_cana_18 <- factor(expdf$m_cana_18, levels=c(1:2), labels=c('No','Yes')) #Frequency respondent taken cannabis/marijuana in last two years
#Partner cannabis consumption
expdf$p_cana_9 <- ifelse(exp$pm1052 %in% 4, 1, NA)
expdf$p_cana_9 <- ifelse(exp$pm1052 %in% c(1:3), 2, expdf$p_cana_9)
expdf$p_cana_9 <- factor(expdf$p_cana_9, levels=c(1:2), labels=c('No','Yes')) #Frequency father has taken cannabis/marihuana in last 2 years

expdf$p_cana_18 <- ifelse(exp$fpa5402 %in% 4, 1, NA)
expdf$p_cana_18 <- ifelse(exp$fpa5402 %in% c(1:3), 2, expdf$p_cana_18)
expdf$p_cana_18 <- factor(expdf$p_cana_18, levels=c(1:2), labels=c('No','Yes')) #In last 2 years, how often taken: Cannabis/marijuana
#Parental cannabis use
expdf$parent_cana_9 <- ifelse((expdf$m_cana_9 %in% 'No' & expdf$p_cana_9 %in% NA) | 
                                (expdf$m_cana_9 %in% NA & expdf$p_cana_9 %in% 'No') |
                                (expdf$m_cana_9 %in% 'No' & expdf$p_cana_9 %in% 'No'), 1, NA)
expdf$parent_cana_9 <- ifelse(expdf$m_cana_9 %in% 'Yes' | expdf$p_cana_9 %in% 'Yes', 2, expdf$parent_cana_9)
expdf$parent_cana_9 <- factor(expdf$parent_cana_9, levels=c(1:2), labels=c('No','Yes'))

expdf$parent_cana_18 <- ifelse((expdf$m_cana_18 %in% 'No' & expdf$p_cana_18 %in% NA) | 
                                 (expdf$m_cana_18 %in% NA & expdf$p_cana_18 %in% 'No') |
                                 (expdf$m_cana_18 %in% 'No' & expdf$p_cana_18 %in% 'No'), 1, NA)
expdf$parent_cana_18 <- ifelse(expdf$m_cana_18 %in% 'Yes' | expdf$p_cana_18 %in% 'Yes', 2, expdf$parent_cana_18)
expdf$parent_cana_18 <- factor(expdf$parent_cana_18, levels=c(1:2), labels=c('No','Yes'))


#Parental other drug use
#Mother other drug use
expdf$m_drug_9 <- ifelse(exp$p1063 %in% 4, 1, NA)
expdf$m_drug_9 <- ifelse(exp$p1063 %in% c(1:3), 2, expdf$m_drug_9)
expdf$m_drug_9 <- factor(expdf$m_drug_9, levels=c(1:2), labels=c('No','Yes')) #Frequency last 2 years mother has taken; p1063 = heroin, methadone, crack, cocaine

expdf$m_drug_18 <- ifelse(exp$t5412 %in% 4, 1, NA)
expdf$m_drug_18 <- ifelse(exp$t5412 %in% c(1:3), 2, expdf$m_drug_18)
expdf$m_drug_18 <- factor(expdf$m_drug_18, levels=c(1:2), labels=c('No','Yes')) #Frequency last two years respondent taken; t5412 = heroin, methadone, crack or other hard drug
#Partner other drug use
expdf$p_drug_9 <- ifelse(exp$pm1063 %in% 4, 1, NA)
expdf$p_drug_9 <- ifelse(exp$pm1063 %in% c(1:3), 2, expdf$p_drug_9)
expdf$p_drug_9 <- factor(expdf$p_drug_9, levels=c(1:2), labels=c('No','Yes')) #Frequency last 2 years father has taken; pm1063 = heroin, methadone, crack, cocaine

expdf$p_drug_18 <- ifelse(exp$fpa5411 %in% 4, 1, NA)
expdf$p_drug_18 <- ifelse(exp$fpa5411 %in% c(1:3), 2, expdf$p_drug_18)
expdf$p_drug_18 <- factor(expdf$p_drug_18, levels=c(1:2), labels=c('No','Yes')) #In last 2 years, how often taken; fpa5411 = heroin, methadone, crack or other hard drug
#Parental drug use
expdf$parent_drug_9 <- ifelse((expdf$m_drug_9 %in% 'No' & expdf$p_drug_9 %in% NA) | 
                                (expdf$m_drug_9 %in% NA & expdf$p_drug_9 %in% 'No') |
                                (expdf$m_drug_9 %in% 'No' & expdf$p_drug_9 %in% 'No'), 1, NA)
expdf$parent_drug_9 <- ifelse(expdf$m_drug_9 %in% 'Yes' | expdf$p_drug_9 %in% 'Yes', 2, expdf$parent_drug_9)
expdf$parent_drug_9 <- factor(expdf$parent_drug_9, levels=c(1:2), labels=c('No','Yes'))

expdf$parent_drug_18 <- ifelse((expdf$m_drug_18 %in% 'No' & expdf$p_drug_18 %in% NA) | 
                                 (expdf$m_drug_18 %in% NA & expdf$p_drug_18 %in% 'No') |
                                 (expdf$m_drug_18 %in% 'No' & expdf$p_drug_18 %in% 'No'), 1, NA)
expdf$parent_drug_18 <- ifelse(expdf$m_drug_18 %in% 'Yes' | expdf$p_drug_18 %in% 'Yes', 2, expdf$parent_drug_18)
expdf$parent_drug_18 <- factor(expdf$parent_drug_18, levels=c(1:2), labels=c('No','Yes'))


#Parental mental health
#Mother mental health condition
expdf$m_mh_1 <- ifelse(exp$f020 %in% c(1:2) | exp$f021 %in% c(1:2), 1, NA)
expdf$m_mh_1 <- ifelse(is.na(expdf$m_mh_1) & exp$f020 %in% 3 & exp$f021 %in% 3, 2, expdf$m_mh_1) 
expdf$m_mh_1 <- factor(expdf$m_mh_1, levels=c(2:1), labels=c('No','Yes')) #since birth had anxiety or depression

expdf$m_mh_12 <- ifelse(exp$s1010 %in% c(1:2) | exp$s1011 %in% c(1:2) | exp$s1020 %in% c(1:2), 1, NA)
expdf$m_mh_12 <- ifelse(is.na(expdf$m_mh_12) & (exp$s1010 %in% 3 | exp$s1011 %in% 3 | exp$s1020 %in% 3), 2, expdf$m_mh_12) 
expdf$m_mh_12 <- factor(expdf$m_mh_12, levels=c(2:1), labels=c('No','Yes')) #Mother has had in the last 2 years; s1010 = anxiety (or nerves); s1011 = depression; s1020 = schizophrenia
#Partner mental health condition
expdf$p_mh_1 <- ifelse(exp$f518 %in% c(1:2) | exp$f519 %in% c(1:2) | exp$f526 %in% c(1:2), 1, NA)
expdf$p_mh_1 <- ifelse(is.na(expdf$p_mh_1) & exp$f518 %in% 3 & exp$f519 %in% 3 & exp$f526 %in% 3, 2, expdf$p_mh_1) 
expdf$p_mh_1 <- factor(expdf$p_mh_1, levels=c(2:1), labels=c('No','Yes')) #since birth partner had depression, anxiety or schizophrenia

expdf$p_mh_12 <- ifelse(exp$pq1010 %in% c(1:2) | exp$pq1011 %in% c(1:2) | exp$pq1020 %in% c(1:2), 1, NA)
expdf$p_mh_12 <- ifelse(is.na(expdf$p_mh_12) & (exp$pq1010 %in% 3 | exp$pq1011 %in% 3 | exp$pq1020 %in% 3), 2, expdf$p_mh_12) 
expdf$p_mh_12 <- factor(expdf$p_mh_12, levels=c(2:1), labels=c('No','Yes')) #Partner has had in last 2 years; pq1010 = depression, pq1011 = anxiety/nerves; pq1020 = schizophrenia)
#Parental mental health condition
expdf$parent_mh_1 <- ifelse((expdf$m_mh_1 %in% 'No' & expdf$p_mh_1 %in% NA) | 
                               (expdf$m_mh_1 %in% NA & expdf$p_mh_1 %in% 'No') |
                               (expdf$m_mh_1 %in% 'No' & expdf$p_mh_1 %in% 'No'), 1, NA)
expdf$parent_mh_1 <- ifelse(expdf$m_mh_1 %in% 'Yes' | expdf$p_mh_1 %in% 'Yes', 2, expdf$parent_mh_1)
expdf$parent_mh_1 <- factor(expdf$parent_mh_1, levels=c(1:2), labels=c('No','Yes'))

expdf$parent_mh_12 <- ifelse((expdf$m_mh_12 %in% 'No' & expdf$p_mh_12 %in% NA) | 
                                (expdf$m_mh_12 %in% NA & expdf$p_mh_12 %in% 'No') |
                                (expdf$m_mh_12 %in% 'No' & expdf$p_mh_12 %in% 'No'), 1, NA)
expdf$parent_mh_12 <- ifelse(expdf$m_mh_12 %in% 'Yes' | expdf$p_mh_12 %in% 'Yes', 2, expdf$parent_mh_12)
expdf$parent_mh_12 <- factor(expdf$parent_mh_12, levels=c(1:2), labels=c('No','Yes'))

#Mother mental health medication
expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% 4, 1, NA)
expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% c(3:1), 2, expdf$m_mhmeds_18)
expdf$m_mhmeds_18 <- factor(expdf$m_mhmeds_18, levels=c(1:2), labels=c('No','Yes')) #Frequency respondent taken pills for depression in last two years
#Father mental health medication
expdf$p_mhmeds_18 <- ifelse(exp$fpa5404 %in% 4, 1, NA)
expdf$p_mhmeds_18 <- ifelse(exp$fpa5404 %in% c(3:1), 2, expdf$p_mhmeds_18)
expdf$p_mhmeds_18 <- factor(expdf$p_mhmeds_18, levels=c(1:2), labels=c('No','Yes')) #In last 2 years, how often taken: Pills for depression
#Parental mental health medication
expdf$parent_mhmeds_18 <- ifelse((expdf$m_mhmeds_18 %in% 'No' & expdf$p_mhmeds_18 %in% NA) | 
                                   (expdf$m_mhmeds_18 %in% NA & expdf$p_mhmeds_18 %in% 'No') |
                                   (expdf$m_mhmeds_18 %in% 'No' & expdf$p_mhmeds_18 %in% 'No'), 1, NA)
expdf$parent_mhmeds_18 <- ifelse(expdf$m_mhmeds_18 %in% 'Yes' | expdf$p_mhmeds_18 %in% 'Yes', 2, expdf$parent_mhmeds_18)
expdf$parent_mhmeds_18 <- factor(expdf$parent_mhmeds_18, levels=c(1:2), labels=c('No','Yes'))


##Parental education
#Mother highest educational qualifications
expdf$m_hiqual_0 <- ifelse(exp$c645 %in% 5, 1, NA)
expdf$m_hiqual_0 <- ifelse(exp$c645 %in% 4, 2, expdf$m_hiqual_0)
expdf$m_hiqual_0 <- ifelse(exp$c645 %in% c(0:3), 3, expdf$m_hiqual_0)
expdf$m_hiqual_0 <- factor(expdf$m_hiqual_0, levels=c(1:3), labels=c('Degree','A-level','O-level or below')) #Mums highest ed qualification
#Partner highest educational qualifications (answered by mother)
expdf$p_hiqual_0 <- ifelse(exp$c666 %in% 5, 1, NA)
expdf$p_hiqual_0 <- ifelse(exp$c666 %in% 4, 2, expdf$p_hiqual_0)
expdf$p_hiqual_0 <- ifelse(exp$c666 %in% c(0:3), 3, expdf$p_hiqual_0)
expdf$p_hiqual_0 <- factor(expdf$p_hiqual_0, levels=c(1:3), labels=c('Degree','A-level','O-level or below')) #Partners highest ed qualification
#Parental highest qualifications
expdf$parent_hiqual_0 <- ifelse((expdf$m_hiqual_0 %in% 'O-level or below' & expdf$p_hiqual_0 %in% NA) | 
                                  (expdf$m_hiqual_0 %in% NA & expdf$p_hiqual_0 %in% 'O-level or below') |
                                  (expdf$m_hiqual_0 %in% 'O-level or below' & expdf$p_hiqual_0 %in% 'O-level or below'), 1, NA)
expdf$parent_hiqual_0 <- ifelse((expdf$m_hiqual_0 %in% 'A-level' & expdf$p_hiqual_0 %in% NA) | 
                                  (expdf$m_hiqual_0 %in% NA & expdf$p_hiqual_0 %in% 'A-level') |
                                  (expdf$m_hiqual_0 %in% 'A-level' & expdf$p_hiqual_0 %in% 'A-level') |
                                  (expdf$m_hiqual_0 %in% 'O-level or below' & expdf$p_hiqual_0 %in% 'A-level') |
                                  (expdf$m_hiqual_0 %in% 'A-level' & expdf$p_hiqual_0 %in% 'O-level or below'), 2, expdf$parent_hiqual_0)
expdf$parent_hiqual_0 <- ifelse(expdf$m_hiqual_0 %in% 'Degree' | expdf$p_hiqual_0 %in% 'Degree', 3, expdf$parent_hiqual_0)
expdf$parent_hiqual_0 <- factor(expdf$parent_hiqual_0, levels=c(3:1), labels=c('Degree','A-level','O-level or below'))


##Parental social class
#Mother social class
expdf$m_sc_0 <- ifelse(exp$c755 %in% 1, 1, NA)
expdf$m_sc_0 <- ifelse(exp$c755 %in% 2, 2, expdf$m_sc_0)
expdf$m_sc_0 <- ifelse(exp$c755 %in% c(3:4), 3, expdf$m_sc_0)
expdf$m_sc_0 <- ifelse(exp$c755 %in% c(5:6), 4, expdf$m_sc_0)
expdf$m_sc_0 <- factor(expdf$m_sc_0, levels=c(1:4), labels=c('I','II','III','IV/V')) #Social Class - Maternal (Armed forces excluded)
#Partner social class (answered by mother)
expdf$p_sc_0 <- ifelse(exp$c765 %in% 1, 1, NA)
expdf$p_sc_0 <- ifelse(exp$c765 %in% 2, 2, expdf$p_sc_0)
expdf$p_sc_0 <- ifelse(exp$c765 %in% c(3:4), 3, expdf$p_sc_0)
expdf$p_sc_0 <- ifelse(exp$c765 %in% c(5:6), 4, expdf$p_sc_0)
expdf$p_sc_0 <- factor(expdf$p_sc_0, levels=c(1:4), labels=c('I','II','III','IV/V')) #Social Class - Paternal (Armed forces excluded)
#Parental social class
expdf$parent_sc_0 <- ifelse((expdf$m_sc_0 %in% 'IV/V' & expdf$p_sc_0 %in% NA) | 
                              (expdf$m_sc_0 %in% NA & expdf$p_sc_0 %in% 'IV/V') |
                              (expdf$m_sc_0 %in% 'IV/V' & expdf$p_sc_0 %in% 'IV/V'), 1, NA)
expdf$parent_sc_0 <- ifelse((expdf$m_sc_0 %in% 'III' & expdf$p_sc_0 %in% NA) | 
                              (expdf$m_sc_0 %in% NA & expdf$p_sc_0 %in% 'III') |
                              (expdf$m_sc_0 %in% 'III' & expdf$p_sc_0 %in% 'III') |
                              (expdf$m_sc_0 %in% 'IV/V' & expdf$p_sc_0 %in% 'III') |
                              (expdf$m_sc_0 %in% 'III' & expdf$p_sc_0 %in% 'IV/V'), 2, expdf$parent_sc_0)
expdf$parent_sc_0 <- ifelse((expdf$m_sc_0 %in% 'II' & expdf$p_sc_0 %in% NA) | 
                              (expdf$m_sc_0 %in% NA & expdf$p_sc_0 %in% 'II') |
                              (expdf$m_sc_0 %in% 'II' & expdf$p_sc_0 %in% 'II') |
                              (expdf$m_sc_0 %in% 'III' & expdf$p_sc_0 %in% 'II') |
                              (expdf$m_sc_0 %in% 'II' & expdf$p_sc_0 %in% 'III') |
                              (expdf$m_sc_0 %in% 'IV/V' & expdf$p_sc_0 %in% 'II') |
                              (expdf$m_sc_0 %in% 'II' & expdf$p_sc_0 %in% 'IV/V'), 3, expdf$parent_sc_0)
expdf$parent_sc_0 <- ifelse(expdf$m_sc_0 %in% 'I' | expdf$p_sc_0 %in% 'I', 4, expdf$parent_sc_0)
expdf$parent_sc_0 <- factor(expdf$parent_sc_0, levels=c(4:1), labels=c('I','II','III','IV/V'))


##Parental economic activity
#Mother economic activity
expdf$m_econ_8 <- ifelse(exp$n4100 %in% 1 | exp$n4101 %in% 1 | exp$n4102 %in% 1 | exp$n4103 %in% 1 | exp$n4104 %in% 1 | exp$n4105 %in% 1 | exp$n4107 %in% 1, 1, NA)
expdf$m_econ_8 <- ifelse(is.na(expdf$m_econ_8) & (exp$n4106 %in% 1 | exp$n4108 %in% 1 | exp$n4109 %in% 1 | exp$n4110 %in% 1 | exp$n4111 %in% 1), 2, expdf$m_econ_8)
expdf$m_econ_8 <- factor(expdf$m_econ_8, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

expdf$m_econ_18 <- ifelse(exp$t1150 %in% 1 | exp$t1158 %in% 1 | exp$t1164 %in% 1, 1, NA)
expdf$m_econ_18 <- ifelse(is.na(expdf$m_econ_18) & (exp$t1154 %in% 1 | exp$t1156 %in% 1 | exp$t1152 %in% 1 | exp$t1162 %in% 1 | exp$t1160 %in% 1), 2, expdf$m_econ_18)
expdf$m_econ_18 <- factor(expdf$m_econ_18, levels=c(1:2), c('Employed/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

#Partner economic activity (answered by mother)
expdf$p_econ_8 <- ifelse(exp$n4120 %in% 1 | exp$n4121 %in% 1 | exp$n4122 %in% 1 | exp$n4123 %in% 1 | exp$n4124 %in% 1 | exp$n4125 %in% 1 | exp$n4127 %in% 1, 1, NA)
expdf$p_econ_8 <- ifelse(is.na(expdf$p_econ_8) & (exp$n4126 %in% 1 | exp$n4128 %in% 1 | exp$n4129 %in% 1 | exp$n4130 %in% 1 | exp$n4131 %in% 1), 2, expdf$p_econ_8)
expdf$p_econ_8 <- factor(expdf$p_econ_8, levels=c(1:2), c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

expdf$p_econ_18 <- ifelse(exp$t1151 %in% 1 | exp$t1159 %in% 1 | exp$t1165 %in% 1, 1, NA)
expdf$p_econ_18 <- ifelse(is.na(expdf$p_econ_18) & (exp$t1155 %in% 1 | exp$t1157 %in% 1 | exp$t1153 %in% 1 | exp$t1163 %in% 1 | exp$t1161 %in% 1), 2, expdf$p_econ_18)
expdf$p_econ_18 <- factor(expdf$p_econ_18, levels=c(1:2), c('Employed/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))
#Parental highest qualifications
expdf$parent_econ_8 <- ifelse((expdf$m_econ_8 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer' & expdf$p_econ_8 %in% NA) | 
                                (expdf$m_econ_8 %in% NA & expdf$p_econ_8 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer') |
                                (expdf$m_econ_8 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer' & expdf$p_econ_8 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer'), 1, NA)
expdf$parent_econ_8 <- ifelse(expdf$m_econ_8 %in% 'Employed/Training/Education' | expdf$p_econ_8 %in% 'Employed/Training/Education', 2, expdf$parent_econ_8)
expdf$parent_econ_8 <- factor(expdf$parent_econ_8, levels=c(2:1), labels=c('Employed/Training/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))

expdf$parent_econ_18 <- ifelse((expdf$m_econ_18 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer' & expdf$p_econ_18 %in% NA) | 
                                 (expdf$m_econ_18 %in% NA & expdf$p_econ_18 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer') |
                                 (expdf$m_econ_18 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer' & expdf$p_econ_18 %in% 'Unemployed/Disabled/Retired/Looks after family/Volunteer'), 1, NA)
expdf$parent_econ_18 <- ifelse(expdf$m_econ_18 %in% 'Employed/Education' | expdf$p_econ_18 %in% 'Employed/Education', 2, expdf$parent_econ_18)
expdf$parent_econ_18 <- factor(expdf$parent_econ_18, levels=c(2:1), labels=c('Employed/Education','Unemployed/Disabled/Retired/Looks after family/Volunteer'))


#Household income
expdf$hhincome_8 <- ifelse(exp$n8130 %in% 5, 1, NA)
expdf$hhincome_8 <- ifelse(exp$n8130 %in% 4, 2, expdf$hhincome_8)
expdf$hhincome_8 <- ifelse(exp$n8130 %in% 3, 3, expdf$hhincome_8)
expdf$hhincome_8 <- ifelse(exp$n8130 %in% c(1:2), 4, expdf$hhincome_8)
expdf$hhincome_8 <- factor(expdf$hhincome_8, levels=c(1:4), labels=c('400+','300-399','200-299','<200')) #Average family take-home income per week (Don't know excluded)

expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(9:10), 1, NA)
expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(7:8), 2, expdf$hhincome_11)
expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(4:6), 3, expdf$hhincome_11)
expdf$hhincome_11 <- ifelse(exp$r9020 %in% c(1:3), 4, expdf$hhincome_11)
expdf$hhincome_11 <- factor(expdf$hhincome_11, levels=c(1:4), labels=c('560+','430-559','240-429','<240')) #each week

expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(9:10), 1, NA)
expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(7:8), 2, expdf$hhincome_18)
expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(4:6), 3, expdf$hhincome_18)
expdf$hhincome_18 <- ifelse(exp$t1300 %in% c(1:3), 4, expdf$hhincome_18)
expdf$hhincome_18 <- factor(expdf$hhincome_18, levels=c(1:4), labels=c('3,400+','2,400-3,399','1,550-2,399','<1,550')) #Average take-home household income each month (Don't know excluded)


#Mother home ownership status
expdf$m_homown_gest <- ifelse(exp$a006 %in% c(0:1), 1, NA)
expdf$m_homown_gest <- ifelse(exp$a006 %in% 2, 2, expdf$m_homown_gest)
expdf$m_homown_gest <- ifelse(exp$a006 %in% c(3:5), 3, expdf$m_homown_gest)
expdf$m_homown_gest <- ifelse(exp$a006 %in% 6, 4, expdf$m_homown_gest)
expdf$m_homown_gest <- factor(expdf$m_homown_gest, levels=c(1:4), labels=c('Mortgaged/Owned','Rented from council','Rented privately/from HA','Other'))

expdf$m_homown_5 <- ifelse(exp$k5010 %in% c(0:2), 1, NA)
expdf$m_homown_5 <- ifelse(exp$k5010 %in% 3, 2, expdf$m_homown_5)
expdf$m_homown_5 <- ifelse(exp$k5010 %in% c(4:6), 3, expdf$m_homown_5)
expdf$m_homown_5 <- ifelse(exp$k5010 %in% 7, 4, expdf$m_homown_5)
expdf$m_homown_5 <- factor(expdf$m_homown_5, levels=c(1:4), labels=c('Mortgaged/Owned','Rented from council','Rented privately/from HA','Other'))

expdf$m_homown_10 <- ifelse(exp$q2010 %in% c(0:2), 1, NA)
expdf$m_homown_10 <- ifelse(exp$q2010 %in% 3, 2, expdf$m_homown_10)
expdf$m_homown_10 <- ifelse(exp$q2010 %in% c(4:6), 3, expdf$m_homown_10)
expdf$m_homown_10 <- ifelse(exp$q2010 %in% 7, 4, expdf$m_homown_10)
expdf$m_homown_10 <- factor(expdf$m_homown_10, levels=c(1:4), labels=c('Mortgaged/Owned','Rented from council','Rented privately/from HA','Other'))

expdf$m_homown_18 <- ifelse(exp$t1010 %in% c(1:3), 1, NA)
expdf$m_homown_18 <- ifelse(exp$t1010 %in% 4, 2, expdf$m_homown_18)
expdf$m_homown_18 <- ifelse(exp$t1010 %in% c(5:7), 3, expdf$m_homown_18)
expdf$m_homown_18 <- ifelse(exp$t1010 %in% 8, 4, expdf$m_homown_18)
expdf$m_homown_18 <- factor(expdf$m_homown_18, levels=c(1:4), labels=c('Mortgaged/Owned','Rented from council','Rented privately/from HA','Other'))


##Sex and Ethnicity
#Sex
expdf$sex <- factor(exp$kz021, levels=c(1:2), labels=c('Male','Female'))

#Ethnicity
expdf$ethnicity <- factor(exp$c804, levels=c(1:2), labels=c('White','BAME*'))

expdf$m_ethnic <- ifelse(exp$c800 %in% 1, 1, NA)
expdf$m_ethnic <- ifelse(exp$c800 %in% c(2:9), 2, expdf$m_ethnic)
expdf$m_ethnic <- factor(expdf$m_ethnic, levels=c(1:2), labels=c('White','BAME*'))

expdf$p_ethnic <- ifelse(exp$c801 %in% 1, 1, NA)
expdf$p_ethnic <- ifelse(exp$c801 %in% c(2:9), 2, expdf$p_ethnic)
expdf$p_ethnic <- factor(expdf$p_ethnic, levels=c(1:2), labels=c('White','BAME*'))


#Diet
expdf$totkcal_14 <- as.numeric(exp$fg1560)
expdf$totkcal_14[expdf$totkcal_14<0] <- NA

expdf$foodgr_14 <- ifelse(exp$fg1570 %in% 2, 1, NA)
expdf$foodgr_14 <- ifelse(exp$fg1570 %in% 1, 2, expdf$foodgr_14)
expdf$foodgr_14 <- ifelse(exp$fg1570 %in% 3, 3, expdf$foodgr_14)
expdf$foodgr_14 <- ifelse(exp$fg1570 %in% 4, 4, expdf$foodgr_14)
expdf$foodgr_14 <- factor(expdf$foodgr_14, levels=c(1:4), labels = c('Healthy','Processed','Traditional','Lunch'))


#BMI (only asked at clinics?)
exp$bmi_14 <- as.numeric(exp$fg3139)
exp$bmi_14[exp$bmi_14<0] <- NA
expdf$bmi_14_gr <- ifelse(exp$bmi_14 < 25, 1, NA)
expdf$bmi_14_gr <- ifelse(exp$bmi_14 >= 25, 2, expdf$bmi_14_gr)
expdf$bmi_14_gr <- factor(expdf$bmi_14_gr, levels=c(1:2), labels = c('<25','>=25'))


exp$bmi_16 <- as.numeric(exp$fh3019)
exp$bmi_16[exp$bmi_16<0] <- NA
expdf$bmi_16_gr <- ifelse(exp$bmi_16 < 25, 1, NA)
expdf$bmi_16_gr <- ifelse(exp$bmi_16 >= 25, 2, expdf$bmi_16_gr)
expdf$bmi_16_gr <- factor(expdf$bmi_16_gr, levels=c(1:2), labels = c('<25','>=25'))


exp$bmi_18 <- as.numeric(exp$FJMR022a)
exp$bmi_18[exp$bmi_18<0] <- NA
expdf$bmi_18_gr <- ifelse(exp$bmi_18 < 25, 1, NA)
expdf$bmi_18_gr <- ifelse(exp$bmi_18 >= 25, 2, expdf$bmi_18_gr)
expdf$bmi_18_gr <- factor(expdf$bmi_18_gr, levels=c(1:2), labels = c('<25','>=25'))

exp$bmi_24 <- as.numeric(exp$FKMS1040)
exp$bmi_24[exp$bmi_24<0] <- NA
expdf$bmi_24_gr <- ifelse(exp$bmi_24 < 25, 1, NA)
expdf$bmi_24_gr <- ifelse(exp$bmi_24 >= 25, 2, expdf$bmi_24_gr)
expdf$bmi_24_gr <- factor(expdf$bmi_24_gr, levels=c(1:2), labels = c('<25','>=25'))


#Exercise
expdf$exerc_16 <- ifelse(exp$ccs5510 %in% c(1:2), 1, NA)
expdf$exerc_16 <- ifelse(exp$ccs5510 %in% c(3:5), 2, expdf$exerc_16)
expdf$exerc_16 <- factor(expdf$exerc_16, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #Frequency during the past year YP did exercise

expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(1:2), 1, NA)
expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(3:5), 2, expdf$exerc_18)
expdf$exerc_18 <- factor(expdf$exerc_18, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #Frequency respondent exercised (going to gym, brisk walking, any sports activity) during past year

expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(4:6), 1, NA)
expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(1:3), 2, expdf$exerc_22)
expdf$exerc_22 <- factor(expdf$exerc_22, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #unsure not included


#Sleep
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


##Mental health and wellbeing
#DAWBA
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

#MFQ
exp$mfq_14 <- as.numeric(exp$fg7226)
exp$mfq_14[exp$mfq_14<0] <- NA
expdf$mfq_14_gr <- ifelse(exp$mfq_14 < 12, 1, NA)
expdf$mfq_14_gr <- ifelse(exp$mfq_14 >= 12, 2, expdf$mfq_14_gr)
expdf$mfq_14_gr <- factor(expdf$mfq_14_gr, levels=c(1:2), labels = c('<12','>=12'))


exp$mfq_18 <- as.numeric(exp$YPB5180)
exp$mfq_18[exp$mfq_18<0] <- NA
expdf$mfq_18_gr <- ifelse(exp$mfq_18 < 12, 1, NA)
expdf$mfq_18_gr <- ifelse(exp$mfq_18 >= 12, 2, expdf$mfq_18_gr)
expdf$mfq_18_gr <- factor(expdf$mfq_18_gr, levels=c(1:2), labels = c('<12','>=12'))


mfq21_labs <- c('YPA2000','YPA2010','YPA2020','YPA2030','YPA2040','YPA2050','YPA2060','YPA2070','YPA2080','YPA2090','YPA2100','YPA2110','YPA2120')
exp[,mfq21_labs] <- sapply(exp[,mfq21_labs], function(x){ifelse(x < 0, NA, x)})
exp[,mfq21_labs] <- sapply(exp[,mfq21_labs], function(x){ifelse(x %in% 3, 0, ifelse(x %in% 2, 1, ifelse(x %in% 1, 2, NA)))})
exp$mfq_21 <- rowSums(exp[,mfq21_labs])
expdf$mfq_21_gr <- ifelse(exp$mfq_21 < 12, 1, NA)
expdf$mfq_21_gr <- ifelse(exp$mfq_21 >= 12, 2, expdf$mfq_21_gr)
expdf$mfq_21_gr <- factor(expdf$mfq_21_gr, levels=c(1:2), labels = c('<12','>=12'))


#Psychosis-like symptoms (PLIKS)
expdf$pliks_18 <- factor(exp$FJPL172, levels=c(0:1), labels=c('No','Yes'))

pliks21_labs <- c('YPA2130','YPA2131','YPA2132','YPA2133','YPA2134','YPA2140','YPA2141','YPA2142','YPA2143','YPA2144','YPA2150','YPA2151','YPA2152','YPA2153')

#Warwick-Edinburgh Mental Wellbeing Scale (WEMWBS) (a score below 42 cutoff for low wellbeing)
expdf$wemwbs_18 <- as.numeric(exp$CCXD814)
expdf$wemwbs_18[expdf$wemwbs_18<0] <- NA

#Clinical Interview Schedule-Revised (CIS-R) (A score of 12 or above on the CIS-R indicates caseness)
expdf$cisr_18 <- as.numeric(exp$FJCI050)
expdf$cisr_18[expdf$cisr_18<0] <- NA

expdf$cisr_gad_18 <- factor(exp$FJCI602, levels=c(0:1), labels=c('No','Yes')) #generalised anxiety disorder
expdf$cisr_mildep_18 <- factor(exp$FJCI603, levels=c(0:1), labels=c('No','Yes')) #mild depressive episode
expdf$cisr_panicdis_18 <- factor(exp$FJCI604, levels=c(0:1), labels=c('No','Yes')) #panic disorder syndrome
expdf$cisr_agoraphob_18 <- factor(exp$FJCI605, levels=c(0:1), labels=c('No','Yes')) #agoraphobia
expdf$cisr_socphob_18 <- factor(exp$FJCI606, levels=c(0:1), labels=c('No','Yes')) #social phobia
expdf$cisr_specphob_18 <- factor(exp$FJCI607, levels=c(0:1), labels=c('No','Yes')) #specific phobias
expdf$cisr_moddep_18 <- factor(exp$FJCI608, levels=c(0:1), labels=c('No','Yes')) #moderate depressive episode
expdf$cisr_sevdep_18 <- factor(exp$FJCI609, levels=c(0:1), labels=c('No','Yes')) #severe depressive episode
expdf$cisr_chrofat_18 <- factor(exp$FJCI610, levels=c(0:1), labels=c('No','Yes')) #chronic fatigue indicator

cisr21_labs <- c('YPA2230','YPA2240','YPA2250','YPA2260','YPA2270','YPA2280','YPA2290','YPA2300','YPA2310','YPA2320','YPA2330','YPA2340','YPA2350','YPA2360','YPA2370','YPA2380')


#GAD-7
gad21_labs <- c('YPA2160','YPA2170','YPA2180','YPA2190','YPA2200','YPA2210','YPA2220')
exp[,gad21_labs] <- sapply(exp[,gad21_labs], function(x){ifelse(x < 0, NA, x)})
exp[,gad21_labs] <- sapply(exp[,gad21_labs], function(x){ifelse(x %in% 1, 0, ifelse(x %in% 2, 1, ifelse(x %in% 3, 2, ifelse(x %in% 4, 3, NA))))})
expdf$gad_21 <- rowSums(exp[,gad21_labs])


##Other substance use
#Alcohol consumption frequency
expdf$everalc_13 <- factor(exp$ff7011, levels=c(2:1), labels=c('No','Yes')) #without permission
expdf$everalc_14 <- factor(exp$fg4872, levels=c(2:1), labels=c('No','Yes')) #without permission

expdf$alc_16 <- ifelse(exp$fh8510 %in% 2 | exp$fh8511 %in% c(1:3), 1, NA)
expdf$alc_16 <- ifelse(exp$fh8511 %in% c(4:6), 2, expdf$alc_16)
expdf$alc_16 <- factor(expdf$alc_16, levels=c(1:2), labels=c('Never, or less than weekly','Weekly or more')) #Frequency of alcohol consumption

expdf$alc_Q_18 <- ifelse(exp$cct5030 %in% c(-2,1:3), 1, NA)
expdf$alc_Q_18 <- ifelse(exp$cct5030 %in% c(4:5), 2, expdf$alc_Q_18)
expdf$alc_Q_18 <- factor(expdf$alc_Q_18, levels=c(1:2), labels=c('Never, or less than weekly','Weekly or more')) #Frequency of alcohol consumption

expdf$alc_C_18 <- ifelse(exp$FJAL1000 %in% c(1:3), 1, NA)
expdf$alc_C_18 <- ifelse(exp$FJAL1000 %in% c(4:5), 2, expdf$alc_C_18)
expdf$alc_C_18 <- factor(expdf$alc_C_18, levels=c(1:2), labels=c('Never, or less than weekly','Weekly or more')) #Frequency of alcohol consumption

expdf$alc_18 <- expdf$alc_Q_18
expdf$alc_18[is.na(expdf$alc_Q_18)] <- expdf$alc_C_18[is.na(expdf$alc_Q_18)]
expdf[,c('alc_Q_18','alc_C_18')] <- NULL

expdf$alc_20 <- ifelse(exp$CCU3100 %in% c(1:3), 1, NA)
expdf$alc_20 <- ifelse(exp$CCU3100 %in% c(4:5), 2, expdf$alc_20)
expdf$alc_20 <- factor(expdf$alc_20, levels=c(1:2), labels=c('Never, or less than weekly','Weekly or more')) #Frequency of alcohol consumption

#Binge drinking
#Binge drinking is defined as consuming 5 or more drinks on an occasion for men or 4 or more drinks on an occasion for women.
expdf$bingalc_Q_18 <- ifelse(exp$cct5032 %in% c(1:4), 1, NA)
expdf$bingalc_Q_18 <- ifelse(exp$cct5032 %in% c(5:6), 2, expdf$bingalc_Q_18)
expdf$bingalc_Q_18 <- factor(expdf$bingalc_Q_18, levels=c(1:2), labels=c('Never, monthly, or less','Weekly or more')) #Over the past year, frequency had six or more units on one occasion

expdf$bingalc_C_18 <- ifelse(exp$FJAL1100 %in% c(1:4), 1, NA)
expdf$bingalc_C_18 <- ifelse(exp$FJAL1100 %in% c(5:6), 2, expdf$bingalc_C_18)
expdf$bingalc_C_18 <- factor(expdf$bingalc_C_18, levels=c(1:2), labels=c('Never, monthly, or less','Weekly or more')) #Over the past year, frequency had six or more units on one occasion

expdf$bingalc_18 <- expdf$bingalc_Q_18
expdf$bingalc_18[is.na(expdf$bingalc_Q_18)] <- expdf$bingalc_C_18[is.na(expdf$bingalc_Q_18)]
expdf[,c('bingalc_Q_18','bingalc_C_18')] <- NULL

expdf$bingalc_20 <- ifelse(exp$CCU3102 %in% c(1:3), 1, NA)
expdf$bingalc_20 <- ifelse(exp$CCU3102 %in% c(4:5), 2, expdf$bingalc_20)
expdf$bingalc_20 <- factor(expdf$bingalc_20, levels=c(1:2), labels=c('Never, monthly, or less','Weekly or more')) #Over the past year, frequency had six or more units on one occasion

#Cannabis
expdf$evercana_13 <- factor(exp$ff7750, levels=c(2:1), labels=c('No','Yes')) #ever

expdf$evercana_C_14 <- factor(exp$fg5423, levels=c(2:1), labels=c('No','Yes')) #past 6 months
expdf$evercana_Q_14 <- factor(exp$ccr760, levels=c(2:1), labels=c('No','Yes')) #ever
expdf$evercana_14 <- expdf$evercana_Q_14
expdf$evercana_14[is.na(expdf$evercana_Q_14)] <- expdf$evercana_C_14[is.na(expdf$evercana_Q_14)]
expdf[,c('evercana_C_14','evercana_Q_14')] <- NULL

expdf$cana_16 <- ifelse(exp$fh8610 %in% 2 | exp$fh8611 %in% c(1:2), 1, NA)
expdf$cana_16 <- ifelse(exp$fh8611 %in% c(3:6), 2, expdf$cana_16)
expdf$cana_16 <- factor(expdf$cana_16, levels=c(1:2), labels=c('Never, once or twice, or used to','Sometimes, weekly, or more')) #fh8610 = YP has ever tried cannabis, fh8611 = YPs description of their cannabis taking

expdf$cana_Q_18 <- ifelse(exp$cct5050 %in% 2 | exp$cct5055 %in% c(1:2), 1, NA)
expdf$cana_Q_18 <- ifelse(exp$cct5055 %in% c(3:5), 2, expdf$cana_Q_18)
expdf$cana_Q_18 <- factor(expdf$cana_Q_18, levels=c(1:2), labels=c('Never, or less than monthly','Monthly or more')) #cct5050 = Respondent tried cannabis, cct5055 = Frequency used cannabis in past 12 months

expdf$cana_C_18 <- ifelse(exp$FJDR050 %in% 2 | exp$FJDR250 %in% 1, 1, NA)
expdf$cana_C_18 <- ifelse(exp$FJDR250 %in% c(2:4), 2, expdf$cana_C_18)
expdf$cana_C_18 <- factor(expdf$cana_C_18, levels=c(1:2), labels=c('Never, or less than monthly','Monthly or more')) #FJDR050 = YP has ever tried cannabis, FJDR250 = Frequency YP uses cannabis: TF4

expdf$cana_18 <- expdf$cana_Q_18
expdf$cana_18[is.na(expdf$cana_Q_18)] <- expdf$cana_C_18[is.na(expdf$cana_Q_18)]
expdf[,c('cana_Q_18','cana_C_18')] <- NULL

expdf$cana_20 <- ifelse(exp$CCU3300 %in% 2 | exp$CCU3302 %in% c(1:2,6), 1, NA)
expdf$cana_20 <- ifelse(exp$CCU3302 %in% c(3:5), 2, expdf$cana_20)
expdf$cana_20 <- factor(expdf$cana_20, levels=c(1:2), labels=c('Never, less than monthly, or not in past year','Monthly or more')) #CCU3300 = Tried cannabis, CCU3302 = In the last 12 months, frequency used cannabis


#Other drugs
#expdf$everhigh_14 <- factor(exp$fg5520, levels=c(2:1), labels=c('No','Yes')) #used drugs (other than cannabis) to get high

#expdf$everoffdrug_13 <- factor(exp$ff8100, levels=c(2:1), labels=c('No','Yes'))
#expdf$everoffdrug_14 <- factor(exp$fg5500, levels=c(2:1), labels=c('No','Yes')) 

expdf$everdrug_13 <- factor(exp$ff8170, levels=c(2:1), labels=c('No','Yes')) #used drugs other than cannabis
expdf$everdrug_14 <- ifelse(exp$ccr840 %in% c(1:2) | exp$ccr841 %in% c(1:2) | exp$ccr842 %in% c(1:2) | exp$ccr843 %in% c(1:2) | exp$ccr844 %in% c(1:2) | exp$ccr850 %in% c(1:2) | exp$ccr851 %in% c(1:2) | exp$ccr852 %in% c(1:2) | exp$ccr853 %in% c(1:2) | exp$ccr855 %in% c(1:2) | exp$ccr856 %in% c(1:2) | exp$ccr857 %in% c(1:2), 1, NA) #respondent has tried aerosols, gas, glue, solvents, poppers, amphetamines, ecstacy, LSD, magic mushrooms, cocaine, crack, heroin
expdf$everdrug_14 <- ifelse(is.na(expdf$everdrug_14) & (exp$ccr840 %in% 3 & exp$ccr841 %in% 3  & exp$ccr842 %in% 3 & exp$ccr843 %in% 3 & exp$ccr844 %in% 3 & exp$ccr850 %in% 3 & exp$ccr851 %in% 3 & exp$ccr852 %in% 3 & exp$ccr853 %in% 3 & exp$ccr855 %in% 3 & exp$ccr856 %in% 3 & exp$ccr857 %in% 3), 2, expdf$everdrug_14)
expdf$everdrug_14 <- factor(expdf$everdrug_14, levels=c(2:1), labels=c('No','Yes')) #respondent has tried aerosols, gas, glue, solvents, poppers, amphetamines, ecstacy, LSD, magic mushrooms, cocaine, crack, heroin

expdf$drug_16 <- ifelse(exp$fh8702 %in% 1 | exp$fh8703 %in% 1 | exp$fh8704 %in% 1 | exp$fh8705 %in% 1 | exp$fh8706 %in% 1 | exp$fh8707 %in% 1, 1, NA)
expdf$drug_16 <- ifelse(is.na(expdf$drug_16) & (exp$fh8702 %in% 2 | exp$fh8703 %in% 2 | exp$fh8704 %in% 2 | exp$fh8705 %in% 2 | exp$fh8706 %in% 2 | exp$fh8707 %in% 2), 2, expdf$drug_16)
expdf$drug_16 <- factor(expdf$drug_16, levels=c(2:1), labels=c('No','Yes')) #since 15th birthday used fh8702 = ecstasy, fh8703 = cocaine, fh8704 = crack, fh8705 = LSD, fh8706 = heroin, fh8707 = ketamine

expdf$drug_Q_18 <- ifelse(exp$cct5100 %in% 1 | exp$cct5110 %in% 1 | exp$cct5150 %in% 1 | exp$cct5170 %in% 11, 1, NA)
expdf$drug_Q_18 <- ifelse(is.na(expdf$drug_Q_18) & (exp$cct5100 %in% 2 | exp$cct5110 %in% 2 | exp$cct5150 %in% 2 | exp$cct5170 %in% 2), 2, expdf$drug_Q_18)
expdf$drug_Q_18 <- factor(expdf$drug_Q_18, levels=c(2:1), labels=c('No','Yes')) #ever used cct5100 = cocaine, cct5110 = crack, cct5150 = hallucinogens, cct5170 = other stimulants (e.g. methedrone, k)

expdf$drug_C_18 <- ifelse(exp$FJDR5000 %in% 1 | exp$FJDR5600 %in% 1, 1, NA)
expdf$drug_C_18 <- ifelse(is.na(expdf$drug_C_18) & (exp$FJDR5000 %in% 2 | exp$FJDR5600 %in% 2), 2, expdf$drug_C_18)
expdf$drug_C_18 <- factor(expdf$drug_C_18, levels=c(2:1), labels=c('No','Yes')) #ever used FJDR5000 = cocaine, FJDR5600 = hallucinogens

expdf$drug_18 <- expdf$drug_Q_18
expdf$drug_18[is.na(expdf$drug_Q_18)] <- expdf$drug_C_18[is.na(expdf$drug_Q_18)]
expdf[,c('drug_Q_18','drug_C_18')] <- NULL

expdf$drug_20 <- ifelse(exp$CCU3401 %in% 1 | exp$CCU3411 %in% 1 | exp$CCU3421 %in% 1 | exp$CCU3441 %in% 1, 1, NA)
expdf$drug_20 <- ifelse(is.na(expdf$drug_20) & (exp$CCU3401 %in% 2 & exp$CCU3411 %in% 2 & exp$CCU3421 %in% 2 & exp$CCU3431 %in% 2 & exp$CCU3441 %in% 2 & exp$CCU3451 %in% 2 & exp$CCU3461 %in% 2), 2, expdf$drug_20)
expdf$drug_20 <- factor(expdf$drug_20, levels=c(2:1), labels=c('No','Yes')) #last year used CCU3401 = cocaine, CCU3411 = amphetamine-type stimulants, CCU3421 = inhalants, CCU3441 = hallucinogens, heroin

#CCU3470 = Count of types illicit substances YP has ever used
#CCU3471 = Count of types illicit substances YP has used in the last year


##Peer substance use
#Smoking
expdf$friends_smk_14 <- factor(exp$fg4820, levels=c(2:1), labels=c('No','Yes')) #(TF2 clinic)

expdf$friends_smk_16 <- ifelse(exp$fh8340 %in% c(1:2), 1, NA)
expdf$friends_smk_16 <- ifelse(exp$fh8340 %in% 3, 2, expdf$friends_smk_16)
expdf$friends_smk_16 <- factor(expdf$friends_smk_16, levels=c(1:2), labels=c('None, one, or some','Most or all')) #Number of YPs friends that smoked cigarettes, during the last year

expdf$friends_smk_18 <- ifelse(exp$FJAA3300 %in% c(1:2), 1, NA)
expdf$friends_smk_18 <- ifelse(exp$FJAA3300 %in% 3, 2, expdf$friends_smk_18)
expdf$friends_smk_18 <- factor(expdf$friends_smk_18, levels=c(1:2), labels=c('None, one, or some','Most or all')) #Number of YP's friends who smoked cigarettes during last year (Not sure excluded)

expdf$friends_smk_20 <- ifelse(exp$CCU3201 %in% c(1:3), 1, NA)
expdf$friends_smk_20 <- ifelse(exp$CCU3201 %in% c(4:5), 2, expdf$friends_smk_20)
expdf$friends_smk_20 <- factor(expdf$friends_smk_20, levels=c(1:2), labels=c('None, a few, or some','Most or all')) #Between ages of 18 and 21, number of friends who would have smoked cigarettes


#Alcohol
expdf$friends_alc_14 <- factor(exp$fg4870, levels=c(2:1), labels=c('No','Yes')) #without permission

expdf$friends_alc_16 <- factor(exp$fh8341, levels=c(1:3), labels=c('None','One or some','Most or all')) #Number of YPs friends that drank alcohol, during the last year
expdf$friends_alc_18 <- factor(exp$FJAA3350, levels=c(1:3), labels=c('None','One or some','Most or all')) #Number of YP's friends who drank alcohol during last year (Not sure excluded)

expdf$friends_alc_20 <- ifelse(exp$CCU3204 %in% 1, 1, NA)
expdf$friends_alc_20 <- ifelse(exp$CCU3204 %in% c(2:3), 2, expdf$friends_alc_20)
expdf$friends_alc_20 <- ifelse(exp$CCU3204 %in% c(4:5), 3, expdf$friends_alc_20)
expdf$friends_alc_20 <- factor(expdf$friends_alc_20, levels=c(1:3), labels=c('None','A few or some','Most or all')) #Between ages of 18 and 21, number of friends who would have smoked cigarettes

#Cannabis
expdf$friends_cana_14 <- factor(exp$fg5420, levels=c(2:1), labels=c('No','Yes'))

expdf$friends_cana_20 <- ifelse(exp$CCU3207 %in% 1, 1, NA)
expdf$friends_cana_20 <- ifelse(exp$CCU3207 %in% c(2:3), 2, expdf$friends_cana_20)
expdf$friends_cana_20 <- ifelse(exp$CCU3207 %in% c(4:5), 3, expdf$friends_cana_20)
expdf$friends_cana_20 <- factor(expdf$friends_cana_20, levels=c(1:3), labels=c('None','A few or some','Most or all')) #Between ages of 18 and 21, number of friends who would have smoked cigarettes

#Other drugs
expdf$friends_offdrug_14 <- factor(exp$fg5480, levels=c(2:1), labels=c('No','Yes'))

expdf$friends_drug_16 <- factor(exp$fh8342, levels=c(1:3), labels=c('None','One or some','Most or all')) #Number of YPs friends that took illegal, drugs during the last year
expdf$friends_drug_18 <- factor(exp$FJAA3400, levels=c(1:3), labels=c('None','One or some','Most or all'))#Number of YP's friends who took illegal drugs during last year (Not sure excluded)

expdf$friends_drug_20 <- ifelse(exp$CCU3208 %in% c(2:5) | exp$CCU3209 %in% c(2:5), 1, NA)
expdf$friends_drug_20 <- ifelse(is.na(expdf$friends_drug_20) & (exp$CCU3208 %in% 1 | exp$CCU3209 %in% 1), 2, expdf$friends_drug_20)
expdf$friends_drug_20 <- factor(expdf$friends_drug_20, levels=c(2:1), labels=c('No','Yes')) #between ages of 18 and 21, number of friends who would have used CCU3208 = inhalants like glue or gas; CCU3209 = other drugs like cocaine, downers, ecstasy or LSD


##Education
#Aspirations and plans
expdf$eduasp_14 <- factor(exp$ccp800, level=c(1:2), c('Stay in education','Leave education'))
expdf$eduperc_14 <- factor(exp$ccp815, level=c(1:2), c('Stay in education','Leave education'))

expdf$eduasp_16 <- factor(exp$ccxa290, level=c(1:2), c('Stay in education','Leave education')) #What the YP will do when they finish year 11 (Don't know yet excluded)

#Educational qualifications obtained
expdf$qual_18 <- ifelse((exp$cct2902 %in% 1 | exp$cct2904 %in% 1), 1, NA) #A/AS/A2 levels
expdf$qual_18 <- ifelse(is.na(expdf$qual_18) & (exp$cct2900 %in% 1 | exp$cct2901 %in% 1 | exp$cct2906 %in% 1 | exp$cct2908 %in% 1 | exp$cct2909 %in% 1 | exp$cct2910 %in% 1 | exp$cct2912 %in% 1 | exp$cct2914 %in% 1| exp$cct2915 %in% 1 | exp$cct2916 %in% 1 | exp$cct2918 %in% 1), 2, expdf$qual_18) #GCSEs/Key Skills/Basic Skills/GCSEs/Edexcel/BTEC/LQL/OCR (not A/AS/A2)/AVCEs/GNVQs/NVQs/City and Guilds
expdf$qual_18 <- factor(expdf$qual_18, levels=c(1:2), labels=c('A-level', 'GCSE or lower/Other'))

expdf$qual_20 <- ifelse((exp$CCU4000 %in% 1), 1, NA) #degree-level qualification
expdf$qual_20 <- ifelse(is.na(expdf$qual_20) & (exp$CCU4007 %in% 1 | exp$CCU4013 %in% 1 | exp$CCU4001 %in% 1 | exp$CCU4002 %in% 1 | exp$CCU4003 %in% 1 | exp$CCU4004 %in% 1 | exp$CCU4005 %in% 1 | exp$CCU4006 %in% 1 | exp$CCU4008 %in% 1 | exp$CCU4009 %in% 1 | exp$CCU4010 %in% 1 | exp$CCU4011 %in% 1 | exp$CCU4012 %in% 1 | exp$CCU4014 %in% 1 | exp$CCU4015 %in% 1 | exp$CCU4016 %in% 1 | exp$CCU4017 %in% 1 | exp$CCU4018 %in% 1 | exp$CCU4019 %in% 1 | exp$CCU4020 %in% 1 | exp$CCU4021 %in% 1 | exp$CCU4022 %in% 1 | exp$CCU4023 %in% 1 | exp$CCU4024 %in% 1), 2, expdf$qual_20) 
expdf$qual_20 <- factor(expdf$qual_20, levels=c(1:2), labels=c('Degree-level', 'A-level or lower/Other'))

#Educational activity
expdf$eduact_18 <- ifelse(exp$cct2950 %in% c(1:2), 1, NA)
expdf$eduact_18 <- ifelse(exp$cct2950 %in% c(3:6), 2, expdf$eduact_18)
expdf$eduact_18 <- ifelse(exp$cct2950 %in% 7, 3, expdf$eduact_18)
expdf$eduact_18 <- factor(expdf$eduact_18, levels=c(1:3), labels=c('Education', 'Training','Not in education or training'))

expdf$eduact_20 <- ifelse(exp$CCU4055 %in% c(1:2), 1, NA)
expdf$eduact_20 <- ifelse(exp$CCU4055 %in% c(3:6), 2, expdf$eduact_20)
expdf$eduact_20 <- ifelse(exp$CCU4055 %in% 7, 3, expdf$eduact_20)
expdf$eduact_20 <- factor(expdf$eduact_20, levels=c(1:3), labels=c('Education', 'Training','Not in education or training'))

expdf$eduact_21 <- ifelse(exp$YPA8010 %in% c(1:2), 1, NA)
expdf$eduact_21 <- ifelse(exp$YPA8010 %in% c(3:6), 2, expdf$eduact_21)
expdf$eduact_21 <- ifelse(exp$YPA8010 %in% 7, 3, expdf$eduact_21)
expdf$eduact_21 <- factor(expdf$eduact_21, levels=c(1:3), labels=c('Education', 'Training','Not in education or training'))


#Work activity
expdf$workact_18 <- ifelse(exp$cct3100 %in% c(1:3), 1, NA)
expdf$workact_18 <- ifelse(exp$cct3100 %in% 7, 2, expdf$workact_18)
expdf$workact_18 <- ifelse(exp$cct3100 %in% c(4:6,8), 3, expdf$workact_18)
expdf$workact_18 <- factor(expdf$workact_18, levels=c(1:3), labels=c('Working','Full-time education','Unemployed/Something else'))

expdf$workact_20 <- ifelse(exp$CCU4060 %in% c(1:3), 1, NA)
expdf$workact_20 <- ifelse(exp$CCU4060 %in% 5, 2, expdf$workact_20)
expdf$workact_20 <- ifelse(exp$CCU4060 %in% c(4,6), 3, expdf$workact_20)
expdf$workact_20 <- factor(expdf$workact_20, levels=c(1:3), labels=c('Working','Full-time education','Unemployed/Something else'))

expdf$workact_21 <- ifelse(exp$YPA8020 %in% c(1:3), 1, NA)
expdf$workact_21 <- ifelse(exp$YPA8020 %in% 5, 2, expdf$workact_21)
expdf$workact_21 <- ifelse(exp$YPA8020 %in% c(4,6), 3, expdf$workact_21)
expdf$workact_21 <- factor(expdf$workact_21, levels=c(1:3), labels=c('Working','Full-time education','Unemployed/Something else'))


##Mother neighborhood deprivation
#G0 Urban/rural (postcode)
expdf$m_urbrur_12 <- ifelse(exp$sur01ind %in% 1, 1, NA)
expdf$m_urbrur_12 <- ifelse(exp$sur01ind %in% c(2:4), 2, expdf$m_urbrur_12)
expdf$m_urbrur_12 <- factor(expdf$m_urbrur_12, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$m_urbrur_18 <- ifelse(exp$tur01ind %in% 1, 1, NA)
expdf$m_urbrur_18 <- ifelse(exp$tur01ind %in% c(2:4), 2, expdf$m_urbrur_18)
expdf$m_urbrur_18 <- factor(expdf$m_urbrur_18, levels=c(1:2), labels=c('Urban','Town or Rural'))

#G0 Townsend score (postcode)
expdf$m_townsend_12 <- exp$sTownsendq5
expdf$m_townsend_12[expdf$m_townsend_12 < 0] <- NA
expdf$m_townsend_12 <- drop_unused_value_labels(expdf$m_townsend_12)

expdf$m_townsend_18 <- exp$tTownsendq5
expdf$m_townsend_18[expdf$m_townsend_18 < 0] <- NA
expdf$m_townsend_18 <- drop_unused_value_labels(expdf$m_townsend_18)
#G0 IMD score (postcode)
expdf$m_IMD_12 <- exp$simd2000q5
expdf$m_IMD_12[expdf$m_IMD_12 < 0] <- NA
expdf$m_IMD_12 <- drop_unused_value_labels(expdf$m_IMD_12)

expdf$m_IMD_18 <- exp$timd2000q5
expdf$m_IMD_18[expdf$m_IMD_18 < 0] <- NA
expdf$m_IMD_18 <- drop_unused_value_labels(expdf$m_IMD_18)


##YP neighbourhood deprivation
#G1 Urban/rural (postcode)
expdf$urbrur_16 <- ifelse(exp$ccsur01ind %in% 1, 1, NA)
expdf$urbrur_16 <- ifelse(exp$ccsur01ind %in% c(2:4), 2, expdf$urbrur_16)
expdf$urbrur_16 <- factor(expdf$urbrur_16, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_18 <- ifelse(exp$cctur01ind %in% 1, 1, NA)
expdf$urbrur_18 <- ifelse(exp$cctur01ind %in% c(2:4), 2, expdf$urbrur_18)
expdf$urbrur_18 <- factor(expdf$urbrur_18, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_20 <- ifelse(exp$ccuur01ind %in% 1, 1, NA)
expdf$urbrur_20 <- ifelse(exp$ccuur01ind %in% c(2:4), 2, expdf$urbrur_20)
expdf$urbrur_20 <- factor(expdf$urbrur_20, levels=c(1:2), labels=c('Urban','Town or Rural'))

expdf$urbrur_21 <- ifelse(exp$YPAur01ind %in% 1, 1, NA)
expdf$urbrur_21 <- ifelse(exp$YPAur01ind %in% c(2:4), 2, expdf$urbrur_21)
expdf$urbrur_21 <- factor(expdf$urbrur_21, levels=c(1:2), labels=c('Urban','Town or Rural'))

#G1 Townsend score (postcode)
expdf$townsend_16 <- exp$ccsTownsendq5
expdf$townsend_16[expdf$townsend_16 < 0] <- NA
expdf$townsend_16 <- drop_unused_value_labels(expdf$townsend_16)
expdf$townsend_16_gr <- ifelse(expdf$townsend_16 %in% 1, 1, NA)
expdf$townsend_16_gr <- ifelse(expdf$townsend_16 %in% c(2:5), 2, expdf$townsend_16_gr)
expdf$townsend_16_gr <- factor(expdf$townsend_16_gr, levels=c(1:2), labels = c('Least deprived','More deprived'))

expdf$townsend_18 <- exp$cctTownsendq5
expdf$townsend_18[expdf$townsend_18<0] <- NA
expdf$townsend_18 <- drop_unused_value_labels(expdf$townsend_18)
expdf$townsend_18_gr <- ifelse(expdf$townsend_18 %in% 1, 1, NA)
expdf$townsend_18_gr <- ifelse(expdf$townsend_18 %in% c(2:5), 2, expdf$townsend_18_gr)
expdf$townsend_18_gr <- factor(expdf$townsend_18_gr, levels=c(1:2), labels = c('Least deprived','More deprived'))

expdf$townsend_20 <- exp$ccuTownsendq5
expdf$townsend_20[expdf$townsend_20<0] <- NA
expdf$townsend_20 <- drop_unused_value_labels(expdf$townsend_20)
expdf$townsend_20_gr <- ifelse(expdf$townsend_20 %in% 1, 1, NA)
expdf$townsend_20_gr <- ifelse(expdf$townsend_20 %in% c(2:5), 2, expdf$townsend_20_gr)
expdf$townsend_20_gr <- factor(expdf$townsend_20_gr, levels=c(1:2), labels = c('Least deprived','More deprived'))

expdf$townsend_21 <- exp$YPATownsendq5
expdf$townsend_21[expdf$townsend_21<0] <- NA
expdf$townsend_21 <- drop_unused_value_labels(expdf$townsend_21)
expdf$townsend_21_gr <- ifelse(expdf$townsend_21 %in% 1, 1, NA)
expdf$townsend_21_gr <- ifelse(expdf$townsend_21 %in% c(2:5), 2, expdf$townsend_21_gr)
expdf$townsend_21_gr <- factor(expdf$townsend_21_gr, levels=c(1:2), labels = c('Least deprived','More deprived'))


#G1 IMD score (postcode)
expdf$IMD_16 <- exp$ccsimd2000q5
expdf$IMD_16[expdf$IMD_16<0] <- NA
expdf$IMD_16 <- drop_unused_value_labels(expdf$IMD_16)

expdf$IMD_18 <- exp$cctimd2000q5
expdf$IMD_18[expdf$IMD_18<0] <- NA
expdf$IMD_18 <- drop_unused_value_labels(expdf$IMD_18)

expdf$IMD_20 <- exp$ccuimd2000q5
expdf$IMD_20[expdf$IMD_20<0] <- NA
expdf$IMD_20 <- drop_unused_value_labels(expdf$IMD_20)

expdf$IMD_21 <- exp$YPAimd2000q5
expdf$IMD_21[expdf$IMD_21<0] <- NA
expdf$IMD_21 <- drop_unused_value_labels(expdf$IMD_21)


##Adverse Childhood Experiences 0-16 (Longitudinal)
expdf$aces_sccl0to16 <- as.numeric(exp$clon122)
expdf$aces_sccl0to16[expdf$aces_sccl0to16<0] <- NA

expdf$aces_catcl0to16 <- exp$clon123
expdf$aces_catcl0to16[expdf$aces_catcl0to16 < 0] <- NA
expdf$aces_catcl0to16 <- factor(expdf$aces_catcl0to16, levels=c(1:4), labels = c('0','1','2-3','4+'))


##Respondent or partner pregnant
expdf$parent_20 <- factor(exp$CCU1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_21 <- factor(exp$YPA1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_22 <- factor(exp$YPB7000, levels=c(2:1), labels=c('No','Yes'))
expdf$parent_28 <- factor(exp$YPH3010, levels=c(0:1), labels=c('No','Yes'))


#SAVE
write_dta(data=expdf, path='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/vaping_exp_measures.dta')
rm(cisr21_labs, gad21_labs, mfq21_labs, pliks21_labs)



#Lives with variables 
#CCU4030
#CCU4031
#CCU4032
#CCU4033
#CCU4034
#CCU4035
#CCU4036
#CCU4037
#CCU4038
#CCU4039
#CCU4040
