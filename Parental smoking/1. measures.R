#ALSPAC variable names

## Cohort profile
#kz021

## G0 quest mother
#a006 a520 a636 a901
#b370 b665
#c755 c765 c645 c666 c804
#e170 e172 e174 
#f020 f021 f247 f258 f246 f257 f518 f519
#g648 g820
#h366 h470
#n1057 n1058 n1059 n1060 n1061 n1062 n3000 n6060 n6061 n6062 n6063 n6064 n6065 n6066 n6067 n6068 n6069 n8130
#p2027 p2038 p2026 p2037
#q2010
#s1300 s3000 s3070

## G0 other geo
#aTownsendq5
## G1 other geo
#cccTownsendq5

## G1 quest partner
#pa910
#pb260 
#pl1057 pl1058 pl1059 pl1060 pl1061 pl1062 pl6100

## G1 clinic self
#f8fp140 f8fp150 f8fp160 f8fp170 f8fp180
#fdaa480 fdaa482 fdaa490 fdaa492 fdaa520 fdaa522 fddp130
#ff5310 ff6000 ff6010 ff6020 ff6030 ff6040 ff6050 ff6600 ff6610 ff6611 ff6612 ff6613  ff7000 ff7010 ff7011 ff7012 ff7013  ff7720 ff7750 ff7751 ff7752 ff7753
#fg7226

## G1 quest self
#ccr700 ccr705
#ccs4000 ccs4005



# Load packages/data
library(haven)
data <- read_dta('//path/to/files/parent_smk_data_ALSPAC.dta')
df <- data[,c('aln','qlet')]



# Partner status

##mother (A 8 weeks gestation)
df$m_partner_0 <- ifelse(data$a520 %in% c(1,2,4), 1, NA) #includes other
df$m_partner_0 <- ifelse(data$a520 %in% 3, 0, df$m_partner_0)

##mother (P 8 years 1 month)
df$m_partner_8 <- ifelse(data$n3000 %in% c(1,2,4), 1, NA) #includes partners not living with mother, and multiple
df$m_partner_8 <- ifelse(data$n3000 %in% 3, 0, df$m_partner_8)

##mother (S 12 years 1 month)
###mother currently has a partner
df$m_partner_12 <- ifelse(data$s3000 %in% c(1:3), 1, NA) #includes female partners
df$m_partner_12 <- ifelse(data$s3000 %in% 4, 0, df$m_partner_12)



# Parental smoking

##mother (G 1 year 9 months) - number of cigs smokes per day
df$m_dailsmk_2 <- ifelse(data$g820 > 0, 1, NA)
df$m_dailsmk_2 <- ifelse(is.na(df$m_dailsmk_2) & data$g820 %in% 0, 0, df$m_dailsmk_2)

##mother's partner (G 1 year 9 months) - n cigs partner smokers every day
df$p_dailsmk_2 <- ifelse(data$g648 > 0, 1, NA)
df$p_dailsmk_2 <- ifelse(is.na(df$p_dailsmk_2) & data$g648 %in% 0, 0, df$p_dailsmk_2)

##parental daily smoking (G 1 year 9 months)
###excludes participants where the mother reported not smoking but did not report whether they had a partner
df$parent_dailsmk_2 <- ifelse(df$m_dailsmk_2 %in% 0 & data$g648 %in% -2, 0, NA)
df$parent_dailsmk_2 <- ifelse(df$m_dailsmk_2 %in% 0 & df$p_dailsmk_2 %in% 0, 0, df$parent_dailsmk_2)
df$parent_dailsmk_2 <- ifelse(df$m_dailsmk_2 %in% 1, 1, df$parent_dailsmk_2)
df$parent_dailsmk_2 <- ifelse(df$m_dailsmk_2 %in% 0 & df$p_dailsmk_2 %in% 1, 1, df$parent_dailsmk_2)
df$parent_dailsmk_2 <- factor(df$parent_dailsmk_2, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_dailsmk_2', 'p_dailsmk_2')] <- NULL

##mother (S 12 years 1 month) - number of cigs smokes per day on weekdays/weekends
df$m_dailsmk_12 <- ifelse(data$s1300 %in% 0 & data$s1301 %in% 0, 0, NA)
df$m_dailsmk_12 <- ifelse(data$s1300 > 0 | data$s1301 > 0, 1, df$m_dailsmk_12)

##mother's partner (S 12 years 1 month) - number of cigs smokes per day on weekdays/weekends
df$p_dailsmk_12 <- ifelse(data$s3070 %in% 0 & data$s3071 %in% 0, 0, NA)
df$p_dailsmk_12 <- ifelse(data$s3070 > 0 | data$s3071 > 0, 1, df$p_dailsmk_12)

##parental daily smoking (S 12 years 1 month)
###whether at least one parent is smoking
###excludes participants where the mother reported not smoking but did not report whether they had a partner
df$parent_dailsmk_12 <- ifelse(df$m_dailsmk_12 %in% 0 & df$m_partner_12 %in% 0, 0, NA)
df$parent_dailsmk_12 <- ifelse(df$m_dailsmk_12 %in% 0 & df$p_dailsmk_12 %in% 0, 0, df$parent_dailsmk_12)
df$parent_dailsmk_12 <- ifelse(df$m_dailsmk_12 %in% 1, 1, df$parent_dailsmk_12)
df$parent_dailsmk_12 <- ifelse(df$m_dailsmk_12 %in% 0 & df$p_dailsmk_12 %in% 1, 1, df$parent_dailsmk_12)
df$parent_dailsmk_12 <- factor(df$parent_dailsmk_12, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_dailsmk_12', 'p_dailsmk_12')] <- NULL



# Offspring substance use

##early substance use (F10 10+ years)
df$subst_10 <- ifelse(data$fdaa482 %in% 1 | data$fdaa492 %in% 1 | data$fdaa522 %in% 1, 1, NA)
df$subst_10 <- ifelse(data$fdaa482 %in% 2 & data$fdaa492 %in% 2 & data$fdaa522 %in% 2, 0, df$subst_10)
df$subst_10 <- factor(df$subst_10, levels=c(0:1), labels=c('No','Yes'))

#smoking past 6 months (TF1 12.5 years)
df$smk_13 <- ifelse(data$ff6610 %in% 2, 0, NA) #has never smoked cigarettes
df$smk_13 <- ifelse(data$ff6611 %in% 2, 0, df$smk_13) #TF1 full not smoked past 6 months
df$smk_13 <- ifelse(data$ff6613 %in% 0, 0, df$smk_13) #TF1 FastTrack not smoked (i.e. smoked 0 times) past 6 months
df$smk_13 <- ifelse(data$ff6610 %in% 1 & data$ff6611 %in% 1, 1, df$smk_13) #smoked past 6 months
df$smk_13 <- ifelse(data$ff6610 %in% 1 & data$ff6612 %in% c(1:3), 1, df$smk_13) #smoked past 6 months
df$smk_13 <- ifelse(data$ff6610 %in% 1 & data$ff6613 > 0, 1, df$smk_13) #smoked at least once in past 6 months

#drank alcohol without permission past 6 months (TF1 12.5 years)
df$alc_13 <- ifelse(data$ff7010 %in% 2, 0, NA) #has never drunk alcohol
df$alc_13 <- ifelse(data$ff7011 %in% 2, 0, df$alc_13) #has never drunk alcohol without permission
df$alc_13 <- ifelse(data$ff7012 %in% 4, 0, df$alc_13) #TF1 full not drunk alcohol without permission in past 6 months
df$alc_13 <- ifelse(data$ff7013 %in% 0, 0, df$alc_13) #TF1 FastTrack not drunk alcohol (i.e. drank 0 times) past 6 months
df$alc_13 <- ifelse(data$ff7011 %in% 1 & data$ff7012 %in% c(1:3), 1, df$alc_13) #drunk alcohol without permission in past 6 months
df$alc_13 <- ifelse(data$ff7011 %in% 1 & data$ff7013 > 0, 1, df$alc_13) #drank alcohol without permission at least once in past 6 months

#cannabis use past 6 months (TF1 12.5 years)
df$canna_13 <- ifelse(data$ff7750 %in% 2, 0, NA) #has never tried cannabis
df$canna_13 <- ifelse(data$ff7751 %in% 2, 0, df$canna_13) #TF1 not tried cannabis past 6 months
df$canna_13 <- ifelse(data$ff7753 %in% 4, 0, df$canna_13) #TF1 FastTrack not used cannabis (i.e. smoked 0 times) past 6 months
df$canna_13 <- ifelse(data$ff7751 %in% 1, 1, df$canna_13) #TF1 tried cannabis past 6 months
df$canna_13 <- ifelse(data$ff7752 %in% 1, 1, df$canna_13) #TF1 Fastrack used cannabis at least once in past 6 months
df$canna_13 <- ifelse(data$ff7753 %in% c(1:3), 1, df$canna_13) #TF1 tried cannabis at least once in past 6 months

##early substance use (smoking/alcohol/cannabis), past 6 months (TF1 12.5 years)
df$subst_13 <- ifelse(df$smk_13 %in% 0 & df$alc_13 %in% 0 & df$canna_13 %in% 0, 0, NA)
df$subst_13 <- ifelse(df$smk_13 %in% 1 | df$alc_13 %in% 1 | df$canna_13 %in% 1, 1, df$subst_13)
df$subst_13 <- factor(df$subst_13, levels=c(0:1), labels=c('No','Yes'))
df[,c('smk_13', 'alc_13','canna_13')] <- NULL

##current smoking 14y (CCR 14 years)
###participants who only ever tried once or twice, or used to sometimes but never now, categorised as non-current smoking
df$currsmk_14 <- ifelse(data$ccr705 %in% c(3:6), 1, NA)
df$currsmk_14 <- ifelse(is.na(df$currsmk_14) & (data$ccr700 %in% 2 | data$ccr705 %in% c(1:2)), 0, df$currsmk_14)
df$currsmk_14 <- factor(df$currsmk_14, levels=c(0:1), labels=c('No','Yes'))

##current smoking 16y (CCS 16 years)
###participants who only ever smoked cigarettes once or twice, or used to smoke but doesn't now, categorised as non-current smoking
df$currsmk_16 <- ifelse(data$ccs4005 %in% c(3:6), 1, NA)
df$currsmk_16 <- ifelse(is.na(df$currsmk_16) & (data$ccs4000 %in% 2 | data$ccs4005 %in% c(1:2)), 0, df$currsmk_16)
df$currsmk_16 <- factor(df$currsmk_16, levels=c(0:1), labels=c('No','Yes'))



# Offspring depressive symptoms
###cut off of 8 or more as is significant in children
###may need to use different thresholds

##SMFQ (F10 10+ years)
df$mfq_10 <- as.numeric(data$fddp130)
df$mfq_10[df$mfq_10<0] <- NA
df$mfq_10_8cut <- factor(ifelse(df$mfq_10 >= 8, 1, 0), levels=c(0:1), labels=c('<8','>=8'))

##SMFQ (TF2 13.5 years)
df$mfq_14 <- as.numeric(data$fg7226)
df$mfq_14[df$mfq_14<0] <- NA
df$mfq_14_8cut <- factor(ifelse(df$mfq_14 >= 8, 1, 0), levels=c(0:1), labels=c('<8','>=8'))



# Parental social class
###excludes armed forces

##maternal (C 32 weeks gestation)
df$m_sc_0 <- ifelse(data$c755 %in% c(1:2), 1, NA)
df$m_sc_0 <- ifelse(data$c755 %in% c(3:4), 2, df$m_sc_0)
df$m_sc_0 <- ifelse(data$c755 %in% c(5:6), 3, df$m_sc_0)
df$m_sc_0 <- factor(df$m_sc_0, levels=c(1:3), labels=c('I/II','III','IV/V'))

##paternal (C 32 weeks gestation)
df$p_sc_0 <- ifelse(data$c765 %in% c(1:2), 1, NA)
df$p_sc_0 <- ifelse(data$c765 %in% c(3:4), 2, df$p_sc_0)
df$p_sc_0 <- ifelse(data$c765 %in% c(5:6), 3, df$p_sc_0)
df$p_sc_0 <- factor(df$p_sc_0, levels=c(1:3), labels=c('I/II','III','IV/V'))

##parental lowest social class (C 32 weeks gestation)
df$parent_sc_0 <- ifelse(df$m_sc_0 %in% 'IV/V' | df$p_sc_0 %in% 'IV/V', 3, NA)
df$parent_sc_0 <- ifelse(is.na(df$parent_sc_0) & (df$m_sc_0 %in% 'III' | df$p_sc_0 %in% 'III'), 2, df$parent_sc_0)
df$parent_sc_0 <- ifelse(is.na(df$parent_sc_0) & (df$m_sc_0 %in% 'I/II' | df$p_sc_0 %in% 'I/II'), 1, df$parent_sc_0)
df$parent_sc_0 <- factor(df$parent_sc_0, levels=c(1:3), labels=c('I/II','III','IV/V'))
df[,c('m_sc_0', 'p_sc_0')] <- NULL



# Parental education
###nobody with no educational qualifications

##mother (C 32 weeks gestation)
df$m_hiqual_0 <- ifelse(data$c645 %in% c(0:2), 3, NA)
df$m_hiqual_0 <- ifelse(data$c645 %in% 3, 2, df$m_hiqual_0)
df$m_hiqual_0 <- ifelse(data$c645 %in% c(4:5), 1, df$m_hiqual_0)
df$m_hiqual_0 <- factor(df$m_hiqual_0, levels=c(1:3), labels=c('A level or above','O level','Vocational/CSE'))

##mother's partner (C 32 weeks gestation)
df$p_hiqual_0 <- ifelse(data$c666 %in% c(0:2), 3, NA)
df$p_hiqual_0 <- ifelse(data$c666 %in% 3, 2, df$p_hiqual_0)
df$p_hiqual_0 <- ifelse(data$c666 %in% c(4:5), 1, df$p_hiqual_0)
df$p_hiqual_0 <- factor(df$p_hiqual_0, levels=c(1:3), labels=c('A level or above','O level','Vocational/CSE'))

##parental lowest education (C 32 weeks gestation)
df$parent_hiqual_0 <- ifelse(df$m_hiqual_0 %in% 'Vocational/CSE' | df$p_hiqual_0 %in% 'Vocational/CSE', 3, NA)
df$parent_hiqual_0 <- ifelse(is.na(df$parent_hiqual_0) & (df$m_hiqual_0 %in% 'O level' | df$p_hiqual_0 %in% 'O level'), 2, df$parent_hiqual_0)
df$parent_hiqual_0 <- ifelse(is.na(df$parent_hiqual_0) & (df$m_hiqual_0 %in% 'A level or above' | df$p_hiqual_0 %in% 'A level or above'), 1, df$parent_hiqual_0)
df$parent_hiqual_0 <- factor(df$parent_hiqual_0, levels=c(1:3), labels=c('A level or above','O level','Vocational/CSE'))
df[,c('m_hiqual_0', 'p_hiqual_0')] <- NULL



# Housing tenure

##home ownership/rental status (A 8 weeks gestation)
df$m_homown_0 <- ifelse(data$a006 %in% c(0:1), 0, NA)
df$m_homown_0 <- ifelse(data$a006 %in% c(2:6), 1, df$m_homown_0)
df$m_homown_0 <- factor(df$m_homown_0, levels=c(0:1), labels=c('Mortgaged/Owned','Rented/Other'))

##home ownership/rental status (Q 10 years 2 months)
df$m_homown_10 <- ifelse(data$q2010 %in% c(0:2), 0, NA)
df$m_homown_10 <- ifelse(data$q2010 %in% c(3:7), 1, df$m_homown_10)
df$m_homown_10 <- factor(df$m_homown_10, levels=c(0:1), labels=c('Mortgaged/Owned','Rented/Other'))



# Household income

##family income per week (H 2 years 9 months)
df$hhincome_3 <- ifelse(data$h470 %in% c(1:3), 1, NA)
df$hhincome_3 <- ifelse(data$h470 %in% c(4:5), 0, df$hhincome_3)
df$hhincome_3 <- factor(df$hhincome_3, levels=c(0:1), labels=c('300+','<300'))

##average family take-home income per week (N 8 years 1 month) - excludes don't know
df$hhincome_8 <- ifelse(data$n8130 %in% c(1:4), 1, NA)
df$hhincome_8 <- ifelse(data$n8130 %in% 5, 0, df$hhincome_8)
df$hhincome_8 <- factor(df$hhincome_8, levels=c(0:1), labels=c('400+','<400'))



# Smoking during pregnancy
###excludes mother's who missed one of the two questionnaires/don't know

##first 3 months (B 18 weeks gestation)
df$m_f3m_pregsmk_0 <- ifelse(data$b665 %in% c(2:5), 1, NA)
df$m_f3m_pregsmk_0 <- ifelse(data$b665 %in% 1, 0, df$m_f3m_pregsmk_0)

##last 2 months (E 8 weeks)
df$m_l2m_pregsmk_8w <- ifelse(data$e170 %in% 1 | data$e172 %in% 1 | data$e174 %in% 1 | data$e176 %in% 1, 1, NA)
df$m_l2m_pregsmk_8w <- ifelse(data$e170 %in% 2 & data$e172 %in% 2 & data$e174 %in% 2 & data$e176 %in% 2, 0, df$m_l2m_pregsmk_8w)

##any (B/E 18 weeks gestation/8 weeks)
df$m_pregsmk <- ifelse(df$m_f3m_pregsmk_0 %in% 1 | df$m_l2m_pregsmk_8w %in% 1, 1, NA)
df$m_pregsmk <- ifelse(is.na(df$m_pregsmk) & (df$m_f3m_pregsmk_0 %in% 0 & df$m_l2m_pregsmk_8w %in% 0), 0, df$m_pregsmk)
df$m_pregsmk <- factor(df$m_pregsmk, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_f3m_pregsmk_0', 'm_l2m_pregsmk_8w')] <- NULL



# Parental EPDS
##mother (B 18 weeks gestation)
df$m_epds_0 <- as.numeric(data$b370)
df$m_epds_0[df$m_epds_0<0] <- NA

##mother's partner (PB 18 weeks gestation)
df$p_epds_0 <- as.numeric(data$pb260)
df$p_epds_0[df$p_epds_0<0] <- NA

##mother (N 8 years 1 month)
m_epds_8 <- data[,c('n6060','n6061','n6062','n6063','n6064','n6065','n6066','n6067','n6068','n6069')]
m_epds_8 <- data.frame(sapply(m_epds_8,function(x) ifelse(x < 0, NA,x)))
m_epds_8 <- data.frame(sapply(m_epds_8,function(x) ifelse(x %in% c(1:4),x-1,x)))
m_epds_8[,c('n6062','n6064','n6065','n6066','n6067','n6068','n6069')] <- sapply(m_epds_8[,c('n6062','n6064','n6065','n6066','n6067','n6068','n6069')],function(x) ifelse(x %in% c(0:3), 3-x,x))
df$m_epds_8 <- rowSums(m_epds_8)

##mother's partner (PL 8 years 1 month)
df$p_epds_8 <- as.numeric(data$pl6100)
df$p_epds_8[df$p_epds_8<0] <- NA

##parental lowest EDPS score (N/PL 8 years 1 month)
###excludes participants where either the mother did not report if they had a partner, or the mother had a partner but missing partner's data
df$parent_epds_8 <- ifelse(df$m_partner_8 %in% 0, df$m_epds_8, NA)
df$parent_epds_8 <- ifelse(df$m_partner_8 %in% 1 & !is.na(df$m_epds_8) & !is.na(df$p_epds_8) & (df$m_epds_8 > df$p_epds_8), df$m_epds_8, df$parent_epds_8)
df$parent_epds_8 <- ifelse(df$m_partner_8 %in% 1 & !is.na(df$m_epds_8) & !is.na(df$p_epds_8) & (df$p_epds_8 > df$m_epds_8), df$p_epds_8, df$parent_epds_8)
df$parent_epds_8 <- ifelse(df$m_partner_8 %in% 1 & !is.na(df$m_epds_8) & !is.na(df$p_epds_8) & (df$p_epds_8 == df$m_epds_8), df$m_epds_8, df$parent_epds_8)
df[,c('m_epds_8', 'p_epds_8')] <- NULL



# Parental mental health condition

##mother (F 8 months) - since birth had anxiety or depression
df$m_mhp_1 <- ifelse(data$f020 %in% c(1:2) | data$f021 %in% c(1:2), 1, NA)
df$m_mhp_1 <- ifelse(is.na(df$m_mhp_1) & data$f020 %in% 3 & data$f021 %in% 3, 0, df$m_mhp_1) 

##mother's partner (F 8 months) - since birth partner had depression, anxiety or schizophrenia
df$p_mhp_1 <- ifelse(data$f518 %in% c(1:2) | data$f519 %in% c(1:2), 1, NA)
df$p_mhp_1 <- ifelse(is.na(df$p_mhp_1) & data$f518 %in% 3 & data$f519 %in% 3, 0, df$p_mhp_1) 

##parental mental health condition (F 8 months) - had depression or anxiety since child was born
###excludes participants where the mother reported not having a MHC but did not report whether they had a partner
df$parent_mhp_1 <- ifelse((df$m_mhp_1 %in% 0 & df$p_mhp_1 %in% 0) | (df$m_mhp_1 %in% 0 & data$f518 %in% -2), 0, NA)
df$parent_mhp_1 <- ifelse((df$m_mhp_1 %in% 1) | (df$m_mhp_1 %in% 0 & df$p_mhp_1 %in% 1), 1, df$parent_mhp_1)
df$parent_mhp_1 <- factor(df$parent_mhp_1, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_mhp_1', 'p_mhp_1')] <- NULL

##mother (N 8 years 1 month)
df$m_mhp_8 <- ifelse(data$n1057 %in% c(1:2) | data$n1058 %in% c(1:2) | data$n1059 %in% c(1:2) | data$n1060 %in% c(1:2) | data$n1061 %in% c(1:2) | data$n1062 %in% c(1:2), 1, NA)
df$m_mhp_8 <- ifelse(is.na(df$m_mhp_8) & (data$n1057 %in% 3 | data$n1058 %in% 3 | data$n1059 %in% 3 | data$n1060 %in% 3 | data$n1061 %in% 3 | data$n1062 %in% 3), 0, df$m_mhp_8) 

##mother's partner (PL 8 years 1 month)
df$p_mhp_8 <- ifelse(data$pl1057 %in% c(1:2) | data$pl1058 %in% c(1:2) | data$pl1059 %in% c(1:2) | data$pl1060 %in% c(1:2) | data$pl1061 %in% c(1:2) | data$pl1062 %in% c(1:2), 1, NA)
df$p_mhp_8 <- ifelse(is.na(df$p_mhp_8) & (data$pl1057 %in% 3 | data$pl1058 %in% 3 | data$pl1059 %in% 3 | data$pl1060 %in% 3 | data$pl1061 %in% 3 | data$pl1062 %in% 3), 0, df$p_mhp_8) 

##parental mental health condition (N/PL 8 years 1 month) - ever had drug addiction,alcoholism,schizophrenia,anorexia nervosa,severe depression,other psychiatric problems
###excludes participants where the mother reported not having a MHC but did not report whether they had a partner
df$parent_mhp_8 <- ifelse((df$m_mhp_8 %in% 0 & df$p_mhp_8 %in% 0) | (df$m_mhp_8 %in% 0 & df$m_partner_8 %in% 0), 0, NA)
df$parent_mhp_8 <- ifelse((df$m_mhp_8 %in% 1) | (df$m_mhp_8 %in% 0 & df$p_mhp_8 %in% 1), 1, df$parent_mhp_8)
df$parent_mhp_8 <- factor(df$parent_mhp_8, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_mhp_8', 'p_mhp_8')] <- NULL



# Neighbourhood deprivation

##neighbourhood quality index (A 8 weeks gestation)
df$hoodqual_0 <- as.numeric(data$a636)
df$hoodqual_0[df$hoodqual_0<0] <- NA

##neighbourhood stress score (H 2 years 9 months)
df$hoodstress_3 <- as.numeric(data$h366)
df$hoodstress_3[df$hoodstress_3<0] <- NA

##townsend deprivation score quintiles (A 8 weeks gestation)
df$m_townsend_0 <- as.numeric(data$aTownsendq5)
df$m_townsend_0[df$m_townsend_0<0] <- NA
df$m_townsend_0 <- factor(df$m_townsend_0, levels=c(1:5), labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

##townsend deprivation score quintiles (CCC 97 months/8.08 years)
df$townsend_8 <- as.numeric(data$cccTownsendq5)
df$townsend_8[df$townsend_8<0] <- NA
df$townsend_8 <- factor(df$townsend_8, levels=c(1:5), labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))



# Parental age at birth

##mother
df$m_age_0 <- as.numeric(data$a901)
df$m_age_0[df$m_age_0<0] <- NA

#mother's partner
df$p_age_0 <- as.numeric(data$pa910)
df$p_age_0[df$p_age_0<0] <- NA

#oldest parental age at birth
df$parent_age_0 <- ifelse(df$m_partner_0 %in% 0, df$m_age_0, NA)
df$parent_age_0 <- ifelse(df$m_partner_0 %in% 1 & !is.na(df$m_age_0) & !is.na(df$p_age_0) & (df$m_age_0 > df$p_age_0), df$m_age_0, df$parent_age_0)
df$parent_age_0 <- ifelse(df$m_partner_0 %in% 1 & !is.na(df$m_age_0) & !is.na(df$p_age_0) & (df$p_age_0 > df$m_age_0), df$p_age_0, df$parent_age_0)
df$parent_age_0 <- ifelse(df$m_partner_0 %in% 1 & !is.na(df$m_age_0) & !is.na(df$p_age_0) & (df$p_age_0 == df$m_age_0), df$m_age_0, df$parent_age_0)
df[,c('m_age_0', 'p_age_0')] <- NULL



# Sex (cohort profile)
df$sex <- factor(as.numeric(data$kz021)-1, levels=c(0:1), labels=c('Male','Female'))



# Ethnicity (C 32 weeks gestation)
df$ethnicity <- factor(as.numeric(data$c804)-1, levels=c(0:1), labels=c('White','Minority ethnic'))

df$m_ethnicity <- ifelse(data$c802 %in% 1, 0, NA)
df$m_ethnicity <- ifelse(data$c802 %in% c(2:9), 1, df$m_ethnicity)
df$m_ethnicity <- factor(df$m_ethnicity, levels=c(0:1), labels=c('White','Minority ethnic'))

df$p_ethnicity <- ifelse(data$c803 %in% 1, 0, NA)
df$p_ethnicity <- ifelse(data$c803 %in% c(2:9), 1, df$p_ethnicity)
df$p_ethnicity <- factor(df$p_ethnicity, levels=c(0:1), labels=c('White','Minority ethnic'))



# Peer substance use (F10 10+ years)
df$peer_subst_10 <- ifelse(data$fdaa480 %in% 1 | data$fdaa490 %in% 1 | data$fdaa520 %in% 1, 1, NA)
df$peer_subst_10 <- ifelse(data$fdaa480 %in% 2 & data$fdaa490 %in% 2 & data$fdaa520 %in% 2, 0, df$peer_subst_10)
df$peer_subst_10 <- factor(df$peer_subst_10, levels=c(0:1), labels=c('No','Yes'))

##peer substance use (TF1 12.5 years) - smoking, alcohol without permission, and cannabis use only (not including offered cannabis/drugs)
df$peer_subst_13 <- ifelse(data$ff6600 %in% 1 | data$ff7000 %in% 1 | data$ff7720 %in% 1, 1, NA)
df$peer_subst_13 <- ifelse(data$ff6600 %in% 2 & data$ff7000 %in% 2 & data$ff7720 %in% 2, 0, df$peer_subst_13)
df$peer_subst_13 <- factor(df$peer_subst_13, levels=c(0:1), labels=c('No','Yes'))


# Emotional/physical abuse
###physically/emotionally cruel to child(ren), based on mother's reports

##mother (F 8 months), since the study child was born
df$m_cruel_1 <- ifelse(data$f247 %in% c(1:4) | data$f258 %in% c(1:4), 1, NA)
df$m_cruel_1 <- ifelse(data$f247 %in% 5 & data$f258 %in% 5, 0, df$m_cruel_1)

##mother's partner (F 8 months), since the study child was born
df$p_cruel_1 <- ifelse(data$f246 %in% c(1:4) | data$f257 %in% c(1:4), 1, NA)
df$p_cruel_1 <- ifelse(data$f246 %in% 5 & data$f257 %in% 5, 0, df$p_cruel_1)

##parental physical/emotional abuse (P 9 years 2 months), since the study child was born
#excludes participants missing data on either mother or mother's partner
df$parent_cruel_1 <- ifelse((df$m_cruel_1 %in% 0 & df$p_cruel_1 %in% 0), 0, NA)
df$parent_cruel_1 <- ifelse((df$m_cruel_1 %in% 1 & df$p_cruel_1 %in% 0) | (df$m_cruel_1 %in% 0 & df$p_cruel_1 %in% 1) | (df$m_cruel_1 %in% 1 & df$p_cruel_1 %in% 1), 1, df$parent_cruel_1)
df$parent_cruel_1 <- factor(df$parent_cruel_1, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_cruel_1', 'p_cruel_1')] <- NULL

##mother (P 9 years 2 months), since the study child's 6th birthday
df$m_cruel_9 <- ifelse(data$p2027 %in% c(1:3) | data$p2038 %in% c(1:3), 1, NA)
df$m_cruel_9 <- ifelse(is.na(df$m_cruel_9) & data$p2027 %in% 4 & data$p2038 %in% 4, 0, df$m_cruel_9)

##mother's partner (P 9 years 2 months), since the study child's 6th birthday
df$p_cruel_9 <- ifelse(data$p2026 %in% c(1:3) | data$p2037 %in% c(1:3), 1, NA)
df$p_cruel_9 <- ifelse(is.na(df$p_cruel_9) & data$p2026 %in% 4 & data$p2037 %in% 4, 0, df$p_cruel_9)

##parental physical/emotional abuse (P 9 years 2 months), since the study child's 6th birthday
#excludes participants missing data on either mother or mother's partner
df$parent_cruel_9 <- ifelse((df$m_cruel_9 %in% 0 & df$p_cruel_9 %in% 0), 0, NA)
df$parent_cruel_9 <- ifelse((df$m_cruel_9 %in% 1 & df$p_cruel_9 %in% 0) | (df$m_cruel_9 %in% 0 & df$p_cruel_9 %in% 1) | (df$m_cruel_9 %in% 1 & df$p_cruel_9 %in% 1), 1, df$parent_cruel_9)
df$parent_cruel_9 <- factor(df$parent_cruel_9, levels=c(0:1), labels=c('No','Yes'))
df[,c('m_cruel_9', 'p_cruel_9')] <- NULL



# Parental monitoring
###frequency carers know what teenager does in free time

##(TF1 12.5 years)
df$monitor_13 <- ifelse(data$ff5310 %in% c(1:3), 1, NA)
df$monitor_13 <- ifelse(is.na(df$monitor_13) & data$ff5310 %in% c(4:5), 0, df$monitor_13)
df$monitor_13 <- factor(df$monitor_13, levels=c(0:1), labels=c('Always/most of the time','Never/Hardly ever/Sometimes'))



# Bullying
###taken personal belongings without asking,threatened/blackmailed,hit/beaten up,tricked,called nasty names,or done other bad things

##experienced any bullying (F8 8 years) - those not bullied did not say yes to any but said no at least once
df$bullying_8 <- ifelse(data$f8fp140 %in% 1 | data$f8fp150 %in% 1 | data$f8fp160 %in% 1 | data$f8fp170 %in% 1 | data$f8fp180 %in% 1, 1, NA)
df$bullying_8 <- ifelse(is.na(df$bullying_8) & (data$f8fp140 %in% 2 | data$f8fp150 %in% 2 | data$f8fp160 %in% 2 | data$f8fp170 %in% 2 | data$f8fp180 %in% 2), 0, df$bullying_8) 
df$bullying_8 <- factor(df$bullying_8, levels=c(0:1), labels=c('No','Yes'))

##experienced any bullying (TF1 12.5 years) - those not bullied did not say yes to any but said no at least once
df$bullying_13 <- ifelse(data$ff6000 %in% 1 | data$ff6010 %in% 1 | data$ff6020 %in% 1 | data$ff6030 %in% 1 | data$ff6040 %in% 1 | data$ff6050 %in% 1, 1, NA)
df$bullying_13 <- ifelse(is.na(df$bullying_13) & (data$ff6000 %in% 2 | data$ff6010 %in% 2 | data$ff6020 %in% 2 | data$ff6030 %in% 2 | data$ff6040 %in% 2 | data$ff6050 %in% 2), 0, df$bullying_13) 
df$bullying_13 <- factor(df$bullying_13, levels=c(0:1), labels=c('No','Yes'))



# Remove those missing sex in full ALSPAC sample
df <- df[!is.na(df$sex),]



# Save dataset with original binary value labels
write_dta(data=df,path='//path/to/files/parent_smk_all_measures.dta')
