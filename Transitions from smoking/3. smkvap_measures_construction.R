##LOAD
library(haven)
library(xlsx)
vap <- read_dta('//path/to/ieu/project/folder/working/data/vaping_transitions/vaping_vars_ALSPAC.dta')
vapdf <- vap[,c('aln','qlet')]


##SMOKING
#ever smoking 21
vapdf$eversmk21 <- ifelse((vap$YPA6000 %in% 1), 1, NA)
vapdf$eversmk21 <- ifelse((vap$YPA6000 %in% 2), 0, vapdf$eversmk21)
vapdf$eversmk21 <- factor(vapdf$eversmk21, levels=c(0:1), labels = c('No','Yes'))
sum(table(vapdf$eversmk21)) #total = 3293
table(vapdf$eversmk21)

#current smoking 21 (smoked past 30 days)
vapdf$currsmk21 <- ifelse((vap$YPA6000 %in% 1) & (vap$YPA6010 %in% 1), 2, NA)
vapdf$currsmk21 <- ifelse((vap$YPA6000 %in% 1) & (vap$YPA6010 %in% 2), 1, vapdf$currsmk21)
vapdf$currsmk21 <- ifelse((vap$YPA6000 %in% 2), 0, vapdf$currsmk21)
vapdf$currsmk21 <- factor(vapdf$currsmk21, levels=c(0:2), labels = c('Never','Past','Current'))
sum(table(vapdf$currsmk21)) #total = 3290
table(vapdf$currsmk21)

vapdf$smk_21 <- ifelse(vapdf$currsmk21 %in% 'Current', 1, NA)
vapdf$smk_21 <- ifelse(vapdf$currsmk21 %in% c('Never','Past'), 0, vapdf$smk_21)
sum(table(vapdf$smk_21)) #total = 3290
table(vapdf$smk_21)


#smoking frequency 21
vapdf$freqsmk_21 <- ifelse(vapdf$currsmk21 %in% 'Current' & vap$YPA6020 %in% 2 & vap$YPA6030 %in% 2, 1, NA)
vapdf$freqsmk_21 <- ifelse(vapdf$currsmk21 %in% 'Current' & (vap$YPA6020 %in% 1 | vap$YPA6030 %in% 1), 2, vapdf$freqsmk_21)
vapdf$freqsmk_21 <- factor(vapdf$freqsmk_21, levels=c(1:2), labels = c('Occasional','Weekly or more'))
sum(table(vapdf$freqsmk_21)) #853 (5 missing)
table(vapdf$freqsmk_21)


##VAPING
#vap21
vapdf$vap21 <- ifelse((vapdf$currsmk21 %in% 'Current'), 1, NA)
vapdf$vap21 <- factor(vapdf$vap21, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
sum(table(vapdf$vap21)) #total = 858
table(vapdf$vap21)

#vap22
vapdf$vap22 <- ifelse((vap$YPB4000 %in% 2 | vap$YPB4010 %in% 2) & (vap$YPB4090 %in% 2 | vap$YPB4100 %in% 2), 4, NA)
vapdf$vap22 <- ifelse((vap$YPB4000 %in% 2 | vap$YPB4010 %in% 2) & (vap$YPB4100 %in% 1), 3, vapdf$vap22)
vapdf$vap22 <- ifelse((vap$YPB4010 %in% 1) & (vap$YPB4100 %in% 1), 2, vapdf$vap22)
vapdf$vap22 <- ifelse((vap$YPB4010 %in% 1) & (vap$YPB4090 %in% 2 | vap$YPB4100 %in% 2), 1, vapdf$vap22)
vapdf$vap22 <- factor(vapdf$vap22, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
sum(table(vapdf$vap22)) #total = 3954
table(vapdf$vap22)

#vap23
vapdf$vap23 <- ifelse((vap$YPC1350 %in% 0 | vap$YPC1360 %in% 0) & (vap$YPC1420 %in% 0 | vap$YPC1440 %in% 0), 4, NA)
vapdf$vap23 <- ifelse((vap$YPC1350 %in% 0 | vap$YPC1360 %in% 0) & (vap$YPC1440 %in% 1), 3, vapdf$vap23)
vapdf$vap23 <- ifelse((vap$YPC1360 %in% 1) & (vap$YPC1440 %in% 1), 2, vapdf$vap23)
vapdf$vap23 <- ifelse((vap$YPC1360 %in% 1) & (vap$YPC1420 %in% 0 | vap$YPC1440 %in% 0), 1, vapdf$vap23)
vapdf$vap23 <- factor(vapdf$vap23, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
sum(table(vapdf$vap23)) #total = 4076
table(vapdf$vap23)

#vap24
vapdf$vap24Q <- ifelse((vap$YPD7000 %in% 0 | vap$YPD7010 %in% 0) & (vap$YPD7070 %in% 0 | vap$YPD7090 %in% 0), 4, NA)
vapdf$vap24Q <- ifelse((vap$YPD7000 %in% 0 | vap$YPD7010 %in% 0) & (vap$YPD7090 %in% 1), 3, vapdf$vap24Q)
vapdf$vap24Q <- ifelse((vap$YPD7010 %in% 1) & (vap$YPD7090 %in% 1), 2, vapdf$vap24Q)
vapdf$vap24Q <- ifelse((vap$YPD7010 %in% 1) & (vap$YPD7070 %in% 0 | vap$YPD7090 %in% 0), 1, vapdf$vap24Q)
vapdf$vap24Q <- factor(vapdf$vap24Q, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
table(vapdf$vap24Q)

vapdf$vap24C <- ifelse((vap$FKSM1010 %in% 0 | vap$FKSM1040 %in% 0) & (vap$FKSM1200 %in% 0 | vap$FKSM1210 %in% 0), 4, NA)
vapdf$vap24C <- ifelse((vap$FKSM1010 %in% 0 | vap$FKSM1040 %in% 0) & (vap$FKSM1210 %in% 1), 3, vapdf$vap24C)
vapdf$vap24C <- ifelse((vap$FKSM1040 %in% 1) & (vap$FKSM1210 %in% 1), 2, vapdf$vap24C)
vapdf$vap24C <- ifelse((vap$FKSM1040 %in% 1) & (vap$FKSM1200 %in% 0 | vap$FKSM1210 %in% 0), 1, vapdf$vap24C)
vapdf$vap24C <- factor(vapdf$vap24C, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
table(vapdf$vap24C)

vapdf$vap24 <- vapdf$vap24Q
vapdf$qorc24 <- NA
vapdf$qorc24[!is.na(vapdf$vap24)] <- 'Q'
vapdf$vap24[is.na(vapdf$vap24)] <- vapdf$vap24C[is.na(vapdf$vap24)]
vapdf$qorc24[!is.na(vapdf$vap24) & is.na(vapdf$qorc24)] <- 'C'
vapdf$vap24C <- NULL
vapdf$vap24Q <- NULL
sum(table(vapdf$vap24)) #total = 5177
table(vapdf$vap24)
table(vapdf$qorc24)

#vap28 (no ever smoked question)
vapdf$vap28 <- ifelse((vap$YPH5510 %in% 0) & (vap$YPH5610 %in% 0 | vap$YPH5620 %in% 0), 4, NA)
vapdf$vap28 <- ifelse((vap$YPH5510 %in% 0) & (vap$YPH5620 %in% 1), 3, vapdf$vap28)
vapdf$vap28 <- ifelse((vap$YPH5510 %in% 1) & (vap$YPH5620 %in% 1), 2, vapdf$vap28)
vapdf$vap28 <- ifelse((vap$YPH5510 %in% 1) & (vap$YPH5610 %in% 0 | vap$YPH5620 %in% 0), 1, vapdf$vap28)
vapdf$vap28 <- factor(vapdf$vap28, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
sum(table(vapdf$vap28)) #total = 4380
table(vapdf$vap28) 

#vap30 (used e-cigs past 30 days not currently using)
vapdf$vap30 <- ifelse((vap$YPK6010 %in% 0 | vap$YPK6040 %in% 0) & (vap$YPK6520 %in% 0 | vap$YPK6540 %in% 0), 4, NA)
vapdf$vap30 <- ifelse((vap$YPK6010 %in% 0 | vap$YPK6040 %in% 0) & (vap$YPK6540 %in% 1), 3, vapdf$vap30)
vapdf$vap30 <- ifelse((vap$YPK6040 %in% 1) & (vap$YPK6540 %in% 1), 2, vapdf$vap30)
vapdf$vap30 <- ifelse((vap$YPK6040 %in% 1) & (vap$YPK6520 %in% 0 | vap$YPK6540 %in% 0), 1, vapdf$vap30)
vapdf$vap30 <- factor(vapdf$vap30, levels=c(1:4), labels=c('Exclusive smoking','Dual use','Exclusive e-cigarette use','Non-use'))
sum(table(vapdf$vap30)) #total = 3848
table(vapdf$vap30)


##AGES
#age21
vapdf$age21 <- as.numeric(vap$YPA9020)
vapdf$age21[vapdf$age21 < 0] <- NA
#table(is.na(vapdf$age21) & !is.na(vapdf$currsmk21)) #TRUE = 0

#age22
vapdf$age22 <- as.numeric(vap$YPB9992)
vapdf$age22[vapdf$age22 < 0] <- NA
vapdf$age22 <- round(vapdf$age22, 0)
#table(is.na(vapdf$age22) & !is.na(vapdf$vap22)) #TRUE = 0

#age23
vapdf$age23 <- as.numeric(vap$YPC2650)
vapdf$age23[vapdf$age23 < 0] <- NA
#table(is.na(vapdf$age23) & !is.na(vapdf$vap23)) #TRUE = 43

#age24
vapdf$age24 <- NA
vapdf$age24[vapdf$qorc24 %in% 'Q'] <- as.numeric(vap$YPD9650[vapdf$qorc24 %in% 'Q'])
vapdf$age24[vapdf$qorc24 %in% 'C'] <- as.numeric(vap$FKAR0010[vapdf$qorc24 %in% 'C'])
vapdf$age24[vapdf$age24 < 0] <- NA
#table(is.na(vapdf$age24) & !is.na(vapdf$vap24)) #TRUE = 20

#age28
vapdf$age28 <- as.numeric(vap$YPH9520)
vapdf$age28[vapdf$age28 < 0] <- NA
#table(is.na(vapdf$age28) & !is.na(vapdf$vap28)) #TRUE = 0

#age30 (asked in years)
vapdf$age30 <- as.numeric(vap$YPK9510)
vapdf$age30[vapdf$age30 < 0] <- NA
vapdf$age30 <- vapdf$age30 * 12
#table(is.na(vapdf$age30) & !is.na(vapdf$vap30)) #TRUE = 0


##TOOK PART
vapdf$tookpart_vap22to30 <- ifelse((!is.na(vapdf$vap22) | !is.na(vapdf$vap23) | !is.na(vapdf$vap24) | !is.na(vapdf$vap28) | !is.na(vapdf$vap30)), T, F)
table(vapdf$tookpart_vap22to30) #6619 TRUE
table(vapdf$tookpart_vap22to30[vapdf$currsmk21 %in% 'Current'])

vapdf$tookpart_vap22to30_twice <- ifelse((!is.na(vapdf$vap22) & !is.na(vapdf$vap23)) |
                                         (!is.na(vapdf$vap22) & !is.na(vapdf$vap24)) | 
                                         (!is.na(vapdf$vap22) & !is.na(vapdf$vap28)) | 
                                         (!is.na(vapdf$vap22) & !is.na(vapdf$vap30)) | 
                                         (!is.na(vapdf$vap23) & !is.na(vapdf$vap24)) | 
                                         (!is.na(vapdf$vap23) & !is.na(vapdf$vap28)) | 
                                         (!is.na(vapdf$vap23) & !is.na(vapdf$vap30)) | 
                                         (!is.na(vapdf$vap24) & !is.na(vapdf$vap28)) |
                                         (!is.na(vapdf$vap24) & !is.na(vapdf$vap30)) |
                                         (!is.na(vapdf$vap28) & !is.na(vapdf$vap30)), T, F)
table(vapdf$tookpart_vap22to30_twice) #5347 TRUE


##DATES OF COLLECTION
#replace ages @ 30y for those with date of completion data
vapdf$losfoll <- NA
vapdf$losfoll[!is.na(vapdf$vap21) & is.na(vapdf$vap22) & is.na(vapdf$vap23) & is.na(vapdf$vap24) & is.na(vapdf$vap28) & is.na(vapdf$vap30)] <- 1
vapdf$losfoll[!is.na(vapdf$vap21) & (!is.na(vapdf$vap22) | !is.na(vapdf$vap23) | !is.na(vapdf$vap24) | !is.na(vapdf$vap28) | !is.na(vapdf$vap30))] <- 0
table(vapdf$losfoll) #55 lost to follow up

vap$YPA9010 <- as.numeric(vap$YPA9010) #date of completion: month @ 21y
vap$YPA9010[vap$YPA9010 < 0] <- NA
vap$YPA9011 <- as.numeric(vap$YPA9011) #date of completion: year @ 21y
vap$YPA9011[vap$YPA9011 < 0] <- NA
vap$coldat21 <- as.yearmon(paste(vap$YPA9011, vap$YPA9010), "%Y %m")

vap$YPK9021 <- as.numeric(vap$YPK9021) #date of completion: month @ 30y
vap$YPK9021[vap$YPK9021 < 0] <- NA
vap$YPK9022 <- as.numeric(vap$YPK9022) #date of completion: year @ 30y
vap$YPK9022[vap$YPK9022 < 0] <- NA
vap$coldat30 <- as.yearmon(paste(vap$YPK9022, vap$YPK9021), "%Y %m")

vap$coldat30diff <- (vap$coldat30 - vap$coldat21)*12
head(vap[,c('coldat21','coldat30','coldat30diff')],20)
vapdf[!is.na(vap$coldat30diff), 'age30'] <- (vapdf[!is.na(vap$coldat30diff), 'age21'] + vap[!is.na(vap$coldat30diff), 'coldat30diff'])
vapdf[, 'age30'] <- round(vapdf[, 'age30'],0)

out <- list()
for(i in 1:6){
  ages <- c('age21', 'age22', 'age23', 'age24', 'age28', 'age30')
  observs <- c('vap21', 'vap22', 'vap23', 'vap24', 'vap28', 'vap30')
  out[[i]] <- sum(is.na(vapdf[!is.na(vapdf[,observs[[i]]]) & vapdf$losfoll %in% 0, ages[[i]]]))
}
names(out) <- c('age21', 'age22', 'age23', 'age24', 'age28', 'age30')
out
#age23 missing 8, age24 missing 4
#can't fill in missingness as missing date of collection


##SAVE
write_dta(data=vapdf, path='//path/to/ieu/project/folder/working/data/vaping_transitions/vaping_categories.dta')

