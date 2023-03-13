##LOAD
library(haven)
smok <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/smoking_vars_ALSPAC.dta')
smokdf <- data.frame(smok[,c('aln','qlet')])

##SMOKING CATEGORIES
#smk13 (13.5) (TF2)
smokdf$smk13 <- NA
smokdf$smk13 <- ifelse(is.na(smokdf$smk13) & smok$fg4822 %in% 1 & smok$fg4827 %in% 1 & smok$fg4824 %in% 3, 'regular', smokdf$smk13)
smokdf$smk13 <- ifelse(is.na(smokdf$smk13) & smok$fg4822 %in% 1 & smok$fg4827 %in% 1 & (smok$fg4824 %in% 1 | smok$fg4824 %in% 2), 'occasional', smokdf$smk13)
smokdf$smk13 <- ifelse(is.na(smokdf$smk13) & smok$fg4822 %in% 2 | smok$fg4827 %in% 2 | smok$fg4824 %in% 4, 'non-smoker', smokdf$smk13)
smokdf$smk13 <- factor(smokdf$smk13, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk13)

#smk14 (14.1)
smokdf$smk14 <- NA
smokdf$smk14 <- ifelse(is.na(smokdf$smk14) & smok$ccr700 %in% 1 & (smok$ccr705 %in% 4 | smok$ccr705 %in% 5 | smok$ccr705 %in% 6), 'regular', smokdf$smk14)
smokdf$smk14 <- ifelse(is.na(smokdf$smk14) & smok$ccr700 %in% 1 & smok$ccr705 %in% 3, 'occasional', smokdf$smk14)
smokdf$smk14 <- ifelse(is.na(smokdf$smk14) & smok$ccr700 %in% 2 | smok$ccr705 %in% 1 | smok$ccr705 %in% 2, 'non-smoker', smokdf$smk14)
smokdf$smk14 <- factor(smokdf$smk14, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk14)

#smk15 (15.5) (TF3)
smokdf$smk15 <- NA
smokdf$smk15 <- ifelse(is.na(smokdf$smk15) & smok$fh8430 %in% 1 & smok$fh8441 %in% 1 & (smok$fh8450 %in% 1 | smok$fh8455 %in% 1), 'regular', smokdf$smk15)
smokdf$smk15 <- ifelse(is.na(smokdf$smk15) & smok$fh8430 %in% 1 & smok$fh8441 %in% 1 & smok$fh8455 %in% 2, 'occasional', smokdf$smk15)
smokdf$smk15 <- ifelse(is.na(smokdf$smk15) & smok$fh8410 %in% 2 | smok$fh8430 %in% 2 | smok$fh8441 %in% 2, 'non-smoker', smokdf$smk15)
smokdf$smk15 <- factor(smokdf$smk15, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk15)

#smk16 (16.5)
smokdf$smk16 <- NA
smokdf$smk16 <- ifelse(is.na(smokdf$smk16) & smok$ccs4000 %in% 1 & (smok$ccs4005 %in% 4 | smok$ccs4005 %in% 5 | smok$ccs4005 %in% 6), 'regular', smokdf$smk16)
smokdf$smk16 <- ifelse(is.na(smokdf$smk16) & smok$ccs4000 %in% 1 & smok$ccs4005 %in% 3, 'occasional', smokdf$smk16)
smokdf$smk16 <- ifelse(is.na(smokdf$smk16) & smok$ccs4000 %in% 2 | smok$ccs4005 %in% 2 | smok$ccs4005 %in% 1, 'non-smoker', smokdf$smk16)
smokdf$smk16 <- factor(smokdf$smk16, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk16)

#smk17 (17.5) (TF4)
smokdf$smk17 <- NA
smokdf$smk17 <- ifelse(is.na(smokdf$smk17) & smok$FJSM050 %in% 1 & smok$FJSM250 %in% 1 & (smok$FJSM350 %in% 1 | smok$FJSM450 %in% 1), 'regular', smokdf$smk17)
smokdf$smk17 <- ifelse(is.na(smokdf$smk17) & smok$FJSM050 %in% 1 & smok$FJSM250 %in% 1 & smok$FJSM450 %in% 2, 'occasional', smokdf$smk17)
smokdf$smk17 <- ifelse(is.na(smokdf$smk17) & smok$FJSM050 %in% 2 | smok$FJSM250 %in% 2, 'non-smoker', smokdf$smk17)
smokdf$smk17 <- factor(smokdf$smk17, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk17)

#smk18
smokdf$smk18 <- NA
smokdf$smk18 <- ifelse(is.na(smokdf$smk18) & smok$cct5000 %in% 1 & smok$cct5010 %in% 1 & (smok$cct5012 %in% 1 | smok$cct5014 %in% 1), 'regular', smokdf$smk18)
smokdf$smk18 <- ifelse(is.na(smokdf$smk18) & smok$cct5000 %in% 1 & smok$cct5010 %in% 1 & smok$cct5014 %in% 2, 'occasional', smokdf$smk18)
smokdf$smk18 <- ifelse(is.na(smokdf$smk18) & smok$cct5000 %in% 2 | smok$cct5010 %in% 2, 'non-smoker', smokdf$smk18)
smokdf$smk18 <- factor(smokdf$smk18, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk18)

#smk20
smokdf$smk20 <- NA
smokdf$smk20 <- ifelse(is.na(smokdf$smk20) & smok$CCU3000 %in% 1 & smok$CCU3010 %in% 1 & (smok$CCU3012 %in% 1 | smok$CCU3014 %in% 1), 'regular', smokdf$smk20)
smokdf$smk20 <- ifelse(is.na(smokdf$smk20) & smok$CCU3000 %in% 1 & smok$CCU3010 %in% 1 & smok$CCU3014 %in% 2, 'occasional', smokdf$smk20)
smokdf$smk20 <- ifelse(is.na(smokdf$smk20) & smok$CCU3000 %in% 2 | smok$CCU3010 %in% 2, 'non-smoker', smokdf$smk20)
smokdf$smk20 <- factor(smokdf$smk20, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk20)

#smk21
smokdf$smk21 <- NA
smokdf$smk21 <- ifelse(is.na(smokdf$smk21) & smok$YPA6000 %in% 1 & smok$YPA6010 %in% 1 & (smok$YPA6020 %in% 1 | smok$YPA6030 %in% 1), 'regular', smokdf$smk21)
smokdf$smk21 <- ifelse(is.na(smokdf$smk21) & smok$YPA6000 %in% 1 & smok$YPA6010 %in% 1 & smok$YPA6030 %in% 2, 'occasional', smokdf$smk21)
smokdf$smk21 <- ifelse(is.na(smokdf$smk21) & smok$YPA6000 %in% 2 | smok$YPA6010 %in% 2, 'non-smoker', smokdf$smk21)
smokdf$smk21 <- factor(smokdf$smk21, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk21)

#smk22
smokdf$smk22 <- NA
smokdf$smk22 <- ifelse(is.na(smokdf$smk22) & smok$YPB4000 %in% 1 & smok$YPB4010 %in% 1 & (smok$YPB4030 %in% 1 | smok$YPB4020 %in% 1), 'regular', smokdf$smk22)
smokdf$smk22 <- ifelse(is.na(smokdf$smk22) & smok$YPB4000 %in% 1 & smok$YPB4010 %in% 1 & smok$YPB4020 %in% 2, 'occasional', smokdf$smk22)
smokdf$smk22 <- ifelse(is.na(smokdf$smk22) & smok$YPB4000 %in% 2 | smok$YPB4010 %in% 2, 'non-smoker', smokdf$smk22)
smokdf$smk22 <- factor(smokdf$smk22, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk22)

#smk23
smokdf$smk23 <- NA
smokdf$smk23 <- ifelse(is.na(smokdf$smk23) & smok$YPC1350 %in% 1 & smok$YPC1360 %in% 1 & (smok$YPC1370 %in% 1 | smok$YPC1380 %in% 1), 'regular', smokdf$smk23)
smokdf$smk23 <- ifelse(is.na(smokdf$smk23) & smok$YPC1350 %in% 1 & smok$YPC1360 %in% 1 & smok$YPC1380 %in% 0, 'occasional', smokdf$smk23)
smokdf$smk23 <- ifelse(is.na(smokdf$smk23) & smok$YPC1350 %in% 0 | smok$YPC1360 %in% 0, 'non-smoker', smokdf$smk23)
smokdf$smk23 <- factor(smokdf$smk23, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk23)

#smk24C (F24)
smokdf$smk24C <- NA
smokdf$smk24C <- ifelse((is.na(smokdf$smk24C) & smok$FKSM1010 %in% 1 & smok$FKSM1040 %in% 1 & (smok$FKSM1060 %in% 1 | smok$FKSM1080 %in% 1)), 'regular', smokdf$smk24C)
smokdf$smk24C <- ifelse((is.na(smokdf$smk24C) & smok$FKSM1010 %in% 1 & smok$FKSM1040 %in% 1 & smok$FKSM1080 %in% 0), 'occasional', smokdf$smk24C)
smokdf$smk24C <- ifelse((is.na(smokdf$smk24C) & smok$FKSM1010 %in% 0) | (is.na(smokdf$smk24C) & smok$FKSM1040 %in% 0), 'non-smoker', smokdf$smk24C)
smokdf$smk24C <- factor(smokdf$smk24C, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk24C)

#smk24Q
smokdf$smk24Q <- NA
smokdf$smk24Q <- ifelse((is.na(smokdf$smk24Q) & smok$YPD7000 %in% 1 & smok$YPD7010 %in% 1 & (smok$YPD7020 %in% 1 | smok$YPD7030 %in% 1)), 'regular', smokdf$smk24Q)
smokdf$smk24Q <- ifelse((is.na(smokdf$smk24Q) & smok$YPD7000 %in% 1 & smok$YPD7010 %in% 1 & smok$YPD7030 %in% 0), 'occasional', smokdf$smk24Q)
smokdf$smk24Q <- ifelse((is.na(smokdf$smk24Q) & smok$YPD7000 %in% 0) | (is.na(smokdf$smk24Q) & smok$YPD7010 %in% 0), 'non-smoker', smokdf$smk24Q)
smokdf$smk24Q <- factor(smokdf$smk24Q, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk24Q)

#smk24
smokdf$smk24 <- as.character(smokdf$smk24Q)
smokdf$smk24 <- ifelse((is.na(smokdf$smk24) & smok$FKSM1010 %in% 1 & smok$FKSM1040 %in% 1 & (smok$FKSM1060 %in% 1 | smok$FKSM1080 %in% 1)), 'regular', smokdf$smk24)
smokdf$smk24 <- ifelse((is.na(smokdf$smk24) & smok$FKSM1010 %in% 1 & smok$FKSM1040 %in% 1 & smok$FKSM1080 %in% 0), 'occasional', smokdf$smk24)
smokdf$smk24 <- ifelse((is.na(smokdf$smk24) & smok$FKSM1010 %in% 0) | (is.na(smokdf$smk24) & smok$FKSM1040 %in% 0), 'non-smoker', smokdf$smk24)
smokdf$smk24 <- factor(smokdf$smk24, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk24)

#smk28
smokdf$smk28 <- NA
smokdf$smk28 <- ifelse(is.na(smokdf$smk28) & smok$YPH5510 %in% 1 & (smok$YPH5520 %in% 1 | smok$YPH5540 %in% 1), 'regular', smokdf$smk28)
smokdf$smk28 <- ifelse(is.na(smokdf$smk28) & smok$YPH5510 %in% 1 & smok$YPH5520 %in% 0, 'occasional', smokdf$smk28)
smokdf$smk28 <- ifelse(is.na(smokdf$smk28) & smok$YPH5510 %in% 0, 'non-smoker', smokdf$smk28)
smokdf$smk28 <- factor(smokdf$smk28, levels=c('regular','occasional','non-smoker'))
table(smokdf$smk28)

#missingness
smokdf$numiss <- rowSums(cbind(is.na(smokdf[,c('smk13','smk14','smk15','smk16','smk17','smk18','smk20','smk21','smk22','smk23','smk24','smk28')])))
smokdf$miss13to16 <- ifelse(!is.na(smokdf$smk13) | !is.na(smokdf$smk14) | !is.na(smokdf$smk15) | !is.na(smokdf$smk16), 0, 1)
smokdf$miss17to21 <- ifelse(!is.na(smokdf$smk17) | !is.na(smokdf$smk18) | !is.na(smokdf$smk20) | !is.na(smokdf$smk21), 0, 1)
smokdf$miss22to28 <- ifelse(!is.na(smokdf$smk22) | !is.na(smokdf$smk23) | !is.na(smokdf$smk24) | !is.na(smokdf$smk28), 0, 1)
smokdf$miss_longit <- rowSums(smokdf[,c('miss13to16','miss17to21','miss22to28')])
#smokdf[,c('smk24C','smk24Q','miss13to16','miss17to21','miss22to28')] <- NULL

#save
write_dta(data=smokdf, path='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/smoking_categories_collapsed.dta')

#ages
exp <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_vars_ALSPAC.dta')
exp[,c('fg0011a','ccr991a','fh0011a','ccs9991a','FJ003a','FJ003b','cct9991a','cct9991c','CCU9991','YPA9020','YPB9992','YPC2650','YPD9650','FKAR0010','FKAR0011','YPH9510','YPH9520')] <- sapply(exp[,c('fg0011a','ccr991a','fh0011a','ccs9991a','FJ003a','FJ003b','cct9991a','cct9991c','CCU9991','YPA9020','YPB9992','YPC2650','YPD9650','FKAR0010','FKAR0011','YPH9510','YPH9520')], function(x) replace(x, x < 0, NA))
smokdf$ages13 <- exp$fg0011a/12
smokdf$ages14 <- exp$ccr991a/12
smokdf$ages15 <- exp$fh0011a/12
smokdf$ages16 <- exp$ccs9991a/12
smokdf$ages17 <- exp$FJ003b
smokdf$ages18 <- exp$cct9991c
smokdf$ages20 <- exp$CCU9991
smokdf$ages21 <- exp$YPA9020/12
smokdf$ages22 <- exp$YPB9992/12
smokdf$ages23 <- exp$YPC2650/12
smokdf$ages24C <- exp$FKAR0010/12
smokdf$ages24Q <- exp$YPD9650/12
smokdf$ages24 <- NA
smokdf$ages24[!is.na(smokdf$smk24Q)] <- exp$YPD9650[!is.na(smokdf$smk24Q)]
smokdf$ages24[is.na(smokdf$smk24Q) & !is.na(smokdf$smk24C)] <- exp$FKAR0010[is.na(smokdf$smk24Q) & !is.na(smokdf$smk24C)]
smokdf$ages24 <- smokdf$ages24/12
smokdf$ages28 <- exp$YPH9520/12
