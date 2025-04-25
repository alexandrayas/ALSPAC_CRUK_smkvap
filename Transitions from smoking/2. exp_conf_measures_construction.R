# VARIABLES USED/REQUESTED FROM ALSPAC

## Cohort profile
#kz021

## CCP/CCR/CCS/CCT/CCU/CCXA/YPA/YPB G1 self questionnaires
#ccp800
#ccr760 ccr840 ccr841 ccr842 ccr843 ccr844 ccr850 ccr851 ccr852 ccr853 ccr855 ccr856 ccr857
#ccs5510
#cct2900 cct2901 cct2902 cct2904 cct2906 cct2908 cct2909 cct2910 cct2912 cct2914 cct2915 cct2916 cct2918 cct4105 cct5030 cct5032 cct5050 cct5055 cct5100 cct5110 cct5150 cct5170
#CCU1000 CCU3102 CCU3201 CCU3300 CCU3302 CCU3401 CCU3411 CCU3421 CCU3431 CCU3441 CCU3451 CCU3461 
#CCU4000 CCU4001 CCU4002 CCU4003 CCU4004 CCU4005 CCU4006 CCU4007 CCU4008 CCU4009 CCU4010 CCU4011 CCU4012 CCU4013 CCU4014 CCU4015 CCU4016 CCU4017 CCU4018 CCU4019 CCU4020 CCU4021 CCU4022 CCU4023 CCU4024
#ccxa290
#YPA1000 YPA2000 YPA2010 YPA2020 YPA2030 YPA2040 YPA2050 YPA2060 YPA2070 YPA2080 YPA2090 YPA2100 YPA2110 YPA2120
#YPB2040 YPB5180 YPB7000

## TF2/TF3/TF4 G1 clinic
#fg4820 fg4872 fg5423 fg7226
#fh3019 fh8340 fh8510 fh8511 fh8610 fh8611 fh8702 fh8703 fh8704 fh8705 fh8706 fh8707
#FJAA3300 FJAL1000 FJAL1100 FJDR050 FJDR250 FJDR5000 FJDR5600 FJMR022a FKMS1040

## Other G1_IMD
#cctTownsendq5 ccuTownsendq5 YPATownsendq5

# C/F/G/N/R/S/T G0 mother questionnaires
#c804
#f020 f021 f518 f519 f526
#g648 g820
#n8130
#r9020
#s1010 s1011 s1020 s1300 s1301
#t1300 t5404 t5520

# PQ/FPA G0 partner questionnaires
#pq1010 pq1010 pq1011 pq1011 pq1020 pq1020 pq1300 pq1301
#fpa5404 fpa5520



# LOAD PACKAGES AND DATA
library(haven)
library(labelled)
exp <- read_dta('//path/to/files/vaping_exp_vars_ALSPAC.dta')
expdf <- data.frame(exp[,c('aln','qlet')])


# Sex
expdf$sex <- factor(exp$kz021, levels=c(1:2), labels=c('Male','Female'))


# Ethnicity
expdf$ethnicity <- factor(exp$c804, levels=c(1:2), labels=c('White','BAME*'))


# Household income
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


# Mother daily smoking
expdf$m_dailsmk_2 <- ifelse(exp$g820 %in% c(1,5,10,15,20,25,30), 1, NA)
expdf$m_dailsmk_2 <- ifelse(is.na(expdf$m_dailsmk_2) & exp$g820 %in% 0, 2, expdf$m_dailsmk_2)
expdf$m_dailsmk_2 <- factor(expdf$m_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(number of cigs smokes per day)

expdf$m_dailsmk_12 <- ifelse(exp$s1300 %in% 0 & exp$s1301 %in% 0, 1, NA)
expdf$m_dailsmk_12 <- ifelse(!is.na(exp$s1300) & !is.na(exp$s1301) & (exp$s1300 > 0 | exp$s1301 > 0), 2, expdf$m_dailsmk_12)
expdf$m_dailsmk_12 <- factor(expdf$m_dailsmk_12, levels=c(1:2), labels=c('No','Yes')) #(number of cigs smokes per day weekdays | weekends)

# Mother's partner daily smoking
expdf$p_dailsmk_2 <- ifelse(exp$g648 %in% c(1:20,22,24:25,28,30,35,40,50,60), 1, NA)
expdf$p_dailsmk_2 <- ifelse(is.na(expdf$p_dailsmk_2) & exp$g648 %in% 0, 2, expdf$p_dailsmk_2)
expdf$p_dailsmk_2 <- factor(expdf$p_dailsmk_2, levels=c(2:1), labels=c('No','Yes')) #(n cigs partner smokers every day)

expdf$p_dailsmk_12 <- ifelse(exp$pq1300 %in% 0 & exp$pq1301 %in% 0, 2, NA)
expdf$p_dailsmk_12 <- ifelse(!is.na(exp$pq1300) & !is.na(exp$pq1301) & (exp$pq1300 > 0 | exp$pq1301 > 0), 1, expdf$p_dailsmk_12)
expdf$p_dailsmk_12 <- factor(expdf$p_dailsmk_12, levels=c(2:1), labels=c('No','Yes'))

# Parental daily smoking
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

## Mother current smoking
expdf$m_currsmk_18 <- factor(exp$t5520, levels=c(2:1), labels=c('No','Yes')) #currently a smoker

## Mother's partner current smoking
expdf$p_currsmk_18 <- factor(exp$fpa5520, levels=c(2:1), labels=c('No','Yes')) #currently a smoker

## Parental current smoking
expdf$parent_currsmk_18 <- ifelse((expdf$m_currsmk_18 %in% 'No' & expdf$p_currsmk_18 %in% NA) | 
                                    (expdf$m_currsmk_18 %in% NA & expdf$p_currsmk_18 %in% 'No') |
                                    (expdf$m_currsmk_18 %in% 'No' & expdf$p_currsmk_18 %in% 'No'), 1, NA)
expdf$parent_currsmk_18 <- ifelse(expdf$m_currsmk_18 %in% 'Yes' | expdf$p_currsmk_18 %in% 'Yes', 2, expdf$parent_currsmk_18)
expdf$parent_currsmk_18 <- factor(expdf$parent_currsmk_18, levels=c(1:2), labels=c('No','Yes'))


## Exercise
expdf$exerc_16 <- ifelse(exp$ccs5510 %in% c(1:2), 1, NA)
expdf$exerc_16 <- ifelse(exp$ccs5510 %in% c(3:5), 2, expdf$exerc_16)
expdf$exerc_16 <- factor(expdf$exerc_16, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #Frequency during the past year YP did exercise

expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(1:2), 1, NA)
expdf$exerc_18 <- ifelse(exp$cct4105 %in% c(3:5), 2, expdf$exerc_18)
expdf$exerc_18 <- factor(expdf$exerc_18, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #Frequency respondent exercised (going to gym, brisk walking, any sports activity) during past year

expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(4:6), 1, NA)
expdf$exerc_22 <- ifelse(exp$YPB2040 %in% c(1:3), 2, expdf$exerc_22)
expdf$exerc_22 <- factor(expdf$exerc_22, levels=c(1:2), labels = c('Weekly or more','Less than weekly')) #unsure not included

## BMI
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


## Educational aspirations
expdf$eduasp_14 <- factor(exp$ccp800, level=c(1:2), c('Stay in education','Leave education'))

expdf$eduasp_16 <- factor(exp$ccxa290, level=c(1:2), c('Stay in education','Leave education')) #What the YP will do when they finish year 11 (Don't know yet excluded)

## Educational qualifications obtained
expdf$qual_18 <- ifelse((exp$cct2902 %in% 1 | exp$cct2904 %in% 1), 1, NA) #A/AS/A2 levels
expdf$qual_18 <- ifelse(is.na(expdf$qual_18) & (exp$cct2900 %in% 1 | exp$cct2901 %in% 1 | exp$cct2906 %in% 1 | exp$cct2908 %in% 1 | exp$cct2909 %in% 1 | exp$cct2910 %in% 1 | exp$cct2912 %in% 1 | exp$cct2914 %in% 1| exp$cct2915 %in% 1 | exp$cct2916 %in% 1 | exp$cct2918 %in% 1), 2, expdf$qual_18) #GCSEs/Key Skills/Basic Skills/GCSEs/Edexcel/BTEC/LQL/OCR (not A/AS/A2)/AVCEs/GNVQs/NVQs/City and Guilds
expdf$qual_18 <- factor(expdf$qual_18, levels=c(1:2), labels=c('A-level', 'GCSE or lower/Other'))

expdf$qual_20 <- ifelse((exp$CCU4000 %in% 1), 1, NA) #degree-level qualification
expdf$qual_20 <- ifelse(is.na(expdf$qual_20) & (exp$CCU4007 %in% 1 | exp$CCU4013 %in% 1 | exp$CCU4001 %in% 1 | exp$CCU4002 %in% 1 | exp$CCU4003 %in% 1 | exp$CCU4004 %in% 1 | exp$CCU4005 %in% 1 | exp$CCU4006 %in% 1 | exp$CCU4008 %in% 1 | exp$CCU4009 %in% 1 | exp$CCU4010 %in% 1 | exp$CCU4011 %in% 1 | exp$CCU4012 %in% 1 | exp$CCU4014 %in% 1 | exp$CCU4015 %in% 1 | exp$CCU4016 %in% 1 | exp$CCU4017 %in% 1 | exp$CCU4018 %in% 1 | exp$CCU4019 %in% 1 | exp$CCU4020 %in% 1 | exp$CCU4021 %in% 1 | exp$CCU4022 %in% 1 | exp$CCU4023 %in% 1 | exp$CCU4024 %in% 1), 2, expdf$qual_20) 
expdf$qual_20 <- factor(expdf$qual_20, levels=c(1:2), labels=c('Degree-level', 'A-level or lower/Other'))


## Alcohol consumption
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

## Binge drinking
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


## Cannabis use
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


## Other drug use
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


## Peer smoking
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


## Short Moods and Feelings Questionnaire (SMFQ) scores
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


## G1 Townsend score (based on postcodes)
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


## Respondent is a parent
expdf$parent_20 <- factor(exp$CCU1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_21 <- factor(exp$YPA1000, levels=c(3:1), labels=c('No','Yes','Yes'))
expdf$parent_22 <- factor(exp$YPB7000, levels=c(2:1), labels=c('No','Yes'))


# Mother mental health condition
expdf$m_mh_1 <- ifelse(exp$f020 %in% c(1:2) | exp$f021 %in% c(1:2), 1, NA)
expdf$m_mh_1 <- ifelse(is.na(expdf$m_mh_1) & exp$f020 %in% 3 & exp$f021 %in% 3, 2, expdf$m_mh_1) 
expdf$m_mh_1 <- factor(expdf$m_mh_1, levels=c(2:1), labels=c('No','Yes')) #since birth had anxiety or depression

expdf$m_mh_12 <- ifelse(exp$s1010 %in% c(1:2) | exp$s1011 %in% c(1:2) | exp$s1020 %in% c(1:2), 1, NA)
expdf$m_mh_12 <- ifelse(is.na(expdf$m_mh_12) & (exp$s1010 %in% 3 | exp$s1011 %in% 3 | exp$s1020 %in% 3), 2, expdf$m_mh_12) 
expdf$m_mh_12 <- factor(expdf$m_mh_12, levels=c(2:1), labels=c('No','Yes')) #Mother has had in the last 2 years; s1010 = anxiety (or nerves); s1011 = depression; s1020 = schizophrenia

# Mother's partner mental health condition
expdf$p_mh_1 <- ifelse(exp$f518 %in% c(1:2) | exp$f519 %in% c(1:2) | exp$f526 %in% c(1:2), 1, NA)
expdf$p_mh_1 <- ifelse(is.na(expdf$p_mh_1) & exp$f518 %in% 3 & exp$f519 %in% 3 & exp$f526 %in% 3, 2, expdf$p_mh_1) 
expdf$p_mh_1 <- factor(expdf$p_mh_1, levels=c(2:1), labels=c('No','Yes')) #since birth partner had depression, anxiety or schizophrenia

expdf$p_mh_12 <- ifelse(exp$pq1010 %in% c(1:2) | exp$pq1011 %in% c(1:2) | exp$pq1020 %in% c(1:2), 1, NA)
expdf$p_mh_12 <- ifelse(is.na(expdf$p_mh_12) & (exp$pq1010 %in% 3 | exp$pq1011 %in% 3 | exp$pq1020 %in% 3), 2, expdf$p_mh_12) 
expdf$p_mh_12 <- factor(expdf$p_mh_12, levels=c(2:1), labels=c('No','Yes')) #Partner has had in last 2 years; pq1010 = depression, pq1011 = anxiety/nerves; pq1020 = schizophrenia)

# Parental mental health condition
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

# Mother mental health medication
expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% 4, 1, NA)
expdf$m_mhmeds_18 <- ifelse(exp$t5404 %in% c(3:1), 2, expdf$m_mhmeds_18)
expdf$m_mhmeds_18 <- factor(expdf$m_mhmeds_18, levels=c(1:2), labels=c('No','Yes')) #Frequency respondent taken pills for depression in last two years

# Father mental health medication
expdf$p_mhmeds_18 <- ifelse(exp$fpa5404 %in% 4, 1, NA)
expdf$p_mhmeds_18 <- ifelse(exp$fpa5404 %in% c(3:1), 2, expdf$p_mhmeds_18)
expdf$p_mhmeds_18 <- factor(expdf$p_mhmeds_18, levels=c(1:2), labels=c('No','Yes')) #In last 2 years, how often taken: Pills for depression

# Parental mental health medication
expdf$parent_mhmeds_18 <- ifelse((expdf$m_mhmeds_18 %in% 'No' & expdf$p_mhmeds_18 %in% NA) | 
                                   (expdf$m_mhmeds_18 %in% NA & expdf$p_mhmeds_18 %in% 'No') |
                                   (expdf$m_mhmeds_18 %in% 'No' & expdf$p_mhmeds_18 %in% 'No'), 1, NA)
expdf$parent_mhmeds_18 <- ifelse(expdf$m_mhmeds_18 %in% 'Yes' | expdf$p_mhmeds_18 %in% 'Yes', 2, expdf$parent_mhmeds_18)
expdf$parent_mhmeds_18 <- factor(expdf$parent_mhmeds_18, levels=c(1:2), labels=c('No','Yes'))


# SAVE
write_dta(data=expdf, path='//path/to/files/vaping_exp_measures.dta')

