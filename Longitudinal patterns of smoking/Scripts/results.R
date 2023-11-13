##LOAD PACKAGES
library(xlsx)
library(dplyr)



##DATA
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/ests.rda')
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/allests.rda")
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/longests.rda")
sig <- unique(allests[allests$sig %in% T,'labs']) #410
exclude <- unique(allests[allests$exclude %in% T,'labs']) #122
#exclude[exclude %in% sig]
low_cell <- unique(allests[allests$low_cell %in% T,'labs']) #226 in total, #116 excluded, 110 remaining



##SUPPLEMENTARY MATERIALS 2
allexp <- data.frame(labs = pltdf$labs,
                     long_labs = pltdf$long_labs,
                     C2VC1_abs_log_OR = ests$C2VC1$abs_log_OR, C2VC1_OR = ests$C2VC1$OR, C2VC1_SE = ests$C2VC1$SE, C2VC1_sig = ests$C2VC1$sig, C2VC1_PAF = ests$C2VC1$PAF, 
                     C5VC1_abs_log_OR = ests$C5VC1$abs_log_OR, C5VC1_OR = ests$C5VC1$OR, C5VC1_SE = ests$C5VC1$SE, C5VC1_sig = ests$C5VC1$sig, C5VC1_PAF = ests$C5VC1$PAF, 
                     C3VC1_abs_log_OR = ests$C3VC1$abs_log_OR, C3VC1_OR = ests$C3VC1$OR, C3VC1_SE = ests$C3VC1$SE, C3VC1_sig = ests$C3VC1$sig, C3VC1_PAF = ests$C3VC1$PAF, 
                     C4VC1_abs_log_OR = ests$C4VC1$abs_log_OR, C4VC1_OR = ests$C4VC1$OR, C4VC1_SE = ests$C4VC1$SE, C4VC1_sig = ests$C4VC1$sig, C4VC1_PAF = ests$C4VC1$PAF,
                     
                     C2VC4_abs_log_OR = ests$C2VC4$abs_log_OR, C2VC4_OR = ests$C2VC4$OR, C2VC4_SE = ests$C2VC4$SE, C2VC4_sig = ests$C2VC4$sig, C2VC4_PAF = ests$C2VC4$PAF, 
                     C5VC4_abs_log_OR = ests$C5VC4$abs_log_OR, C5VC4_OR = ests$C5VC4$OR, C5VC4_SE = ests$C5VC4$SE, C5VC4_sig = ests$C5VC4$sig, C5VC4_PAF = ests$C5VC4$PAF,
                     C3VC4_abs_log_OR = ests$C3VC4$abs_log_OR, C3VC4_OR = ests$C3VC4$OR, C3VC4_SE = ests$C3VC4$SE, C3VC4_sig = ests$C3VC4$sig, C3VC4_PAF = ests$C3VC4$PAF,
                     
                     C2VC3_abs_log_OR = ests$C2VC3$abs_log_OR, C2VC3_OR = ests$C2VC3$OR, C2VC3_SE = ests$C2VC3$SE, C2VC3_sig = ests$C2VC3$sig, C2VC3_PAF = ests$C2VC3$PAF,
                     C5VC3_abs_log_OR = ests$C5VC3$abs_log_OR, C5VC3_OR = ests$C5VC3$OR, C5VC3_SE = ests$C5VC3$SE, C5VC3_sig = ests$C5VC3$sig, C5VC3_PAF = ests$C5VC3$PAF, 
                     
                     C2VC5_abs_log_OR = ests$C2VC5$abs_log_OR, C2VC5_OR = ests$C2VC5$OR, C2VC5_SE = ests$C2VC5$SE, C2VC5_sig = ests$C2VC5$sig, C2VC5_PAF = ests$C2VC5$PAF)

excldf <- data.frame(labs = exclude) #nrow = 122
excldf$long_labs <- pltdf$long_labs[match(exclude,pltdf$labs)]
excldf$zeros <- ifelse(excldf$labs %in% unique(allests[allests$zeros %in% T,'labs']), T, F) #58
excldf$low_n <- ifelse(excldf$labs %in% unique(allests[allests$low_n %in% T,'labs']), T, F) #71
excldf$largese <- ifelse(excldf$labs %in% unique(allests[allests$largese %in% T,'labs']), T, F) #10

write.xlsx(pltdf[,c('sub_grp','grp','labs','long_labs','ages','ages_sub','sum','sum_unexp','sum_exp','prop_exp','labs_unexp','labs_exp','sum_cl1','sum_cl2','sum_cl3','sum_cl4','sum_cl5')], sheetName="Labels and Ns", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/all_results.xlsx')
write.xlsx(allexp[!allexp$labs %in% exclude,], sheetName="All exposures", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/all_results.xlsx', append=T)
write.xlsx(allexp[!allexp$labs %in% exclude & allexp$labs %in% sig & allexp$labs %in% low_cell,], sheetName="Significant exposures, low cells", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/all_results.xlsx', append=T)
write.xlsx(allexp[!allexp$labs %in% exclude & allexp$labs %in% sig & !allexp$labs %in% low_cell,], sheetName="Significant exposures, no low cells", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/all_results.xlsx', append=T)
write.xlsx(excldf, sheetName="Excluded", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/all_results.xlsx', append=T)
rm(allexp, excldf, ests)



##CONSISTENT HITS
allhits <- list()
getallhits <- function(x){
  out <- list()
  for(i in names(x)){
    out$labs[[i]] <- sapply(x[[i]],function(x){unique(x$labs)})
    out$OR_range[[i]] <- sapply(x[[i]],function(x){range(x$OR)})
    out$CI_range[[i]] <- sapply(x[[i]],function(x){range(x$LCI,x$UCI)})
  }
  return(out)
}
allhits$allcomp <- getallhits(longests)
cons_hits <- allhits$allcomp$labs
save(cons_hits, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/cons_hits.rda')
cons_hits <- unique(unlist(cons_hits)) #221
rm(getallhits)

#sapply(allhits$allcomp$labs, length)
#nonsmokers occasional short_term late_onset 
#85         90         30        161 
#sapply(lapply(allhits$allcomp$labs, function(x) unique(pltdf$grp[pltdf$labs %in% x])), length)
#nonsmokers occasional short_term late_onset 
#31         36         22         54
#OR < 1 = reduced exposure in reference group, reference group less likely to have exposure



##GET TOP HITS FUNCTION (ORs)
hitit <- function(df, ordvar, outvars){
  hits <- split(df, df$comparison)
  hits <- lapply(hits, function(x){x <- x[order(abs(x[,ordvar]),decreasing=T),]})
  hits <- lapply(hits, head, 5)
  hits <- do.call(rbind,hits)
  topOR_precursor <- unique(hits$labs)
  hits <- hits[,outvars]
  return(hits)
}

## TOP OR HITS
tophits <- list()

#with low cells
hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_precursor <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_concurrent <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

#without low cells
hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor (no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_precursor_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent (no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_concurrent_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

#consistent
hits <- hitit(allests[allests$labs %in% cons_hits & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor (consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_precursor_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$labs %in% cons_hits & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_log_OR', c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent (consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$OR_concurrent_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

##TOP HITS PAF
#with low cells
hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & allests$modif %in% T & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="precursor (PAF)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_precursor <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & allests$modif %in% T & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="concurrent (PAF)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_concurrent <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]

#without low cells
hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$modif %in% T & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="precursor (PAF no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_precursor_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$modif %in% T & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="concurrent (PAF no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_concurrent_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]

#consistent
hits <- hitit(allests[allests$labs %in% cons_hits & allests$modif %in% T & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="precursor (PAF consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_precursor_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]

hits <- hitit(allests[allests$labs %in% cons_hits & allests$modif %in% T & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], 'abs_PAF', c('long_labs','PAF','PAR','prop_exp'))
write.xlsx(hits, sheetName="concurrent (PAF consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$PAF_concurrent_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$modif %in% T & pltdf$long_labs %in% hits$long_labs]
rm(hitit)



##GET TOP HITS FUNCTION 2 (CIs)
hitit2 <- function(df, outvars){
  df$ci_range <- df$UCI - df$LCI
  hits <- split(df, df$comparison)
  hits <- lapply(hits, function(x){x <- x[order(x[,'ci_range'],decreasing=F),]})
  hits <- lapply(hits, head, 5)
  hits <- do.call(rbind,hits)
  topCI_precursor <- unique(hits$labs)
  hits <- hits[,outvars]
  return(hits)
}

## TOP OR HITS
#with low cells
hits <- hitit2(allests[allests$exclude %in% F & allests$sig %in% T & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor (CI)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_precursor <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit2(allests[allests$exclude %in% F & allests$sig %in% T & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent (CI)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_concurrent <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

#without low cells
hits <- hitit2(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor (CI no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_precursor_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit2(allests[allests$exclude %in% F & allests$sig %in% T & !(allests$labs %in% low_cell) & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent (CI no low cell)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_concurrent_nolow <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

#consistent
hits <- hitit2(allests[allests$labs %in% cons_hits & allests$ages_sub %in% c('Gestation','Early Childhood','Late Childhood'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="precursor (CI consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_precursor_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]

hits <- hitit2(allests[allests$labs %in% cons_hits & allests$ages_sub %in% c('Adolescence','Adulthood','Longitudinal'),], c('long_labs','OR','LCI','UCI'))
write.xlsx(hits, sheetName="concurrent (CI consistent)", append = T, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/tophits.xlsx')
tophits$CI_concurrent_cons <- pltdf$labs[pltdf$labs %in% sig & !(pltdf$labs %in% exclude) & pltdf$long_labs %in% hits$long_labs]
rm(hitit2)



#MOST IMPORTANT HITS (high OR, high PAF, all comps)
allimportant <- table(unlist(tophits))[order(table(unlist(tophits)), decreasing=T)] #165
allimportant2 <- table(c(unlist(tophits),cons_hits))[order(table(c(unlist(tophits),cons_hits)), decreasing=T)] #256
vimportant <- allimportant[allimportant > 5] #11 c('drug_24', 'evercana_16', 'sportclubs_11', 'friends_smk_16', 'friends_smk_18', 'evercana_17', 'evercana_28', 'income_25', 'm_feltdepr_2', 'sex', 'slp_wkdays_11_gr')
save(allimportant, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/allimportant.rda')
save(allimportant2, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/allimportant2.rda')



#N sig
nsig <- split(allests[allests$exclude %in% F ,], allests[allests$exclude %in% F,'labs'])
nsig <- sapply(nsig, function(x){sum(x$sig)}) #402
table(nsig)
# 0  1  2  3  4  5  6  7  8  9 10 
#44 22 40 44 55 38 47 35 51 23  3
write.xlsx(table(nsig), file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/nsig.xlsx')
names(nsig[nsig %in% 10]) #3 c('evercana_16','friends_smk_16','friends_smk_18')
names(nsig[nsig %in% 9]) #23



#N by sub_grp/grp
subgrp_ns <- data.frame(sub_grp = unique(pltdf$sub_grp),
                        sapply(sapply(allhits$allcomp$labs, function(x) table(pltdf$sub_grp[match(x, pltdf$labs)])),
                               function(x) x[match(unique(pltdf$sub_grp), names(x))])
)
subgrp_ns$avail <- table(pltdf$sub_grp)[unique(pltdf$sub_grp)]
subgrp_ns$avail_excl <- table(pltdf$sub_grp[!pltdf$labs %in% exclude])[unique(pltdf$sub_grp[!pltdf$labs %in% exclude])][subgrp_ns$sub_grp]
write.xlsx(subgrp_ns, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/subgrp_ns.xlsx')

grp_ns <- data.frame(grp = unique(pltdf$grp),
                     sapply(sapply(allhits$allcomp$labs, function(x) table(pltdf$grp[match(x, pltdf$labs)])),
                            function(x) x[match(unique(pltdf$grp), names(x))])
)
grp_ns$avail <- table(pltdf$grp)[unique(pltdf$grp)]
grp_ns$avail_excl <- table(pltdf$grp[!pltdf$labs %in% exclude])[unique(pltdf$grp[!pltdf$labs %in% exclude])][grp_ns$grp]
write.xlsx(grp_ns, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/grp_ns.xlsx')





#N by comparision
n_comp <- allests[allests$sig %in% T & allests$exclude %in% F,]
n_comp <- split(n_comp, n_comp$comparison)
sapply(n_comp, nrow)
format(sapply(n_comp, nrow)/length(unique(allests$labs[allests$exclude %in% F])), digits=2)



#most exposed
pltdf_imp <- pltdf[pltdf$labs %in% allimportant,]
pltdf_imp <- pltdf_imp[order(pltdf_imp$prop_exp, decreasing=T),]
head(pltdf_imp[,c('labs','sum','sum_exp', 'prop_exp')],20)



#WRITE UP
hits <- data.frame(labs = unique(unlist(allhits$allcomp$labs)))
hits$nonsmokers <- hits$labs %in% allhits$allcomp$labs$nonsmokers
hits$occasional <- hits$labs %in% allhits$allcomp$labs$occasional
hits$short_term <- hits$labs %in% allhits$allcomp$labs$short_term
hits$late_onset <- hits$labs %in% allhits$allcomp$labs$late_onset
hits[,'n'] <- rowSums(hits[,c('nonsmokers','occasional','short_term','late_onset')])

pastit <- function(df_or, df_ci){
  out <- list()
  for(i in hits$labs){
    if(i %in% colnames(df_or)) {
      out[[i]] <- paste0('OR: ',
                         df_or[1,i],
                         ' - ',
                         df_or[2,i],
                         ', CI: ',
                         df_ci[1,i],
                         ' - ',     
                         df_ci[2,i])
    } else {
      out[[i]] <- NA
    }
  }
  return(unlist(out))
}

hits$nonsmokers <- pastit(allhits$allcomp$OR_range$nonsmokers, allhits$allcomp$CI_range$nonsmokers)
hits$occasional <- pastit(allhits$allcomp$OR_range$occasional, allhits$allcomp$CI_range$occasional)
hits$short_term <- pastit(allhits$allcomp$OR_range$short_term, allhits$allcomp$CI_range$short_term)
hits$late_onset <- pastit(allhits$allcomp$OR_range$late_onset, allhits$allcomp$CI_range$late_onset)

hits$lab_grp <- pltdf$lab_grp[match(hits$labs,pltdf$labs)]
hits$grp <- pltdf$grp[match(hits$labs,pltdf$labs)]
hits <- split(hits, hits$lab_grp)
hits <- lapply(hits, function(x) split(x, x$grp))

hits <- lapply(hits, function(x){
  lapply(x, function(y){
  y <- y[,!colnames(y) %in% c('lab_grp','grp')]
  y <- y[order(y$n, decreasing = T),]
  return(y)})
})
rm(pastit)

#hits$`Familial substance use`[c('m_pregsmk','mgm_pregsmk','m_eversmk','m_regsmk','m_dailsmk','m_currsmk','m_stopsmk','p_currsmk','p_dailsmk','hh_smk','m_pregcana','m_cana','m_alc')]
#hits$`Mental health and other factors`['m_cotinine']
#hits$`Familial sociodemographics`[c('hh_income','m_homown','m_marst','m_econact','p_econact','m_qual','p_qual','parent_sc','m_IMD','m_townsend','m_urbrur','m_mhprob','m_gooddays','m_feltdepr','p_mhprob','mg_mhprob')]
#hits$`Lifestyle factors`[c('friends_smk','friends_cana','friends_alc','friends_drug')]
#hits$`Lifestyle factors`[c('bmi','exercise','mvpa','sportclubs','timesleep','everalc','evercana','everoffdrug','everdrug')]
#hits$`Sociodemographic factors`[c('sex','eduasp','qual','eduact','econact','sc','job','income','IMD','townsend','urbrur')]
#hits$`Mental health and other factors`[c('dawba','cisr','mfq','wemwbs', 'mhprob', 'mhmeds','preg','baby','parent','ACEs','trauma')]