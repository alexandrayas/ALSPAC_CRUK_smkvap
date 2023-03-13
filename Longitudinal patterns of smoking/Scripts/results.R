##LOAD PACKAGES
library(haven)
library(xlsx)
library(dplyr)



##DATA
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/ests.rda')
load("C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/allests.rda")
#rm(list=ls()[! ls() %in% c("pltdf","allests","ests")])


##SUPPLEMENTARY MATERIALS 2
allexp <- data.frame(labs = pltdf$labs,
                     C1VC5_abs_log_OR = ests$C1VC5$abs_log_OR, C1VC5_OR = ests$C1VC5$OR, C1VC5_SE = ests$C1VC5$SE, C1VC5_sig = ests$C1VC5$sig, C1VC5_PAF = ests$C1VC5$PAF, 
                     C2VC5_abs_log_OR = ests$C2VC5$abs_log_OR, C2VC5_OR = ests$C2VC5$OR, C2VC5_SE = ests$C2VC5$SE, C2VC5_sig = ests$C2VC5$sig, C2VC5_PAF = ests$C2VC5$PAF, 
                     C3VC5_abs_log_OR = ests$C3VC5$abs_log_OR, C3VC5_OR = ests$C3VC5$OR, C3VC5_SE = ests$C3VC5$SE, C3VC5_sig = ests$C3VC5$sig, C3VC5_PAF = ests$C3VC5$PAF, 
                     C4VC5_abs_log_OR = ests$C4VC5$abs_log_OR, C4VC5_OR = ests$C4VC5$OR, C4VC5_SE = ests$C4VC5$SE, C4VC5_sig = ests$C4VC5$sig, C4VC5_PAF = ests$C4VC5$PAF,
                     C1VC4_abs_log_OR = ests$C1VC4$abs_log_OR, C1VC4_OR = ests$C1VC4$OR, C1VC4_SE = ests$C1VC4$SE, C1VC4_sig = ests$C1VC4$sig, C1VC4_PAF = ests$C1VC4$PAF,
                     C2VC4_abs_log_OR = ests$C2VC4$abs_log_OR, C2VC4_OR = ests$C2VC4$OR, C2VC4_SE = ests$C2VC4$SE, C2VC4_sig = ests$C2VC4$sig, C2VC4_PAF = ests$C2VC4$PAF, 
                     C3VC4_abs_log_OR = ests$C3VC4$abs_log_OR, C3VC4_OR = ests$C3VC4$OR, C3VC4_SE = ests$C3VC4$SE, C3VC4_sig = ests$C3VC4$sig, C3VC4_PAF = ests$C3VC4$PAF,
                     C1VC3_abs_log_OR = ests$C1VC3$abs_log_OR, C1VC3_OR = ests$C1VC3$OR, C1VC3_SE = ests$C1VC3$SE, C1VC3_sig = ests$C1VC3$sig, C1VC3_PAF = ests$C1VC3$PAF, 
                     C2VC3_abs_log_OR = ests$C2VC3$abs_log_OR, C2VC3_OR = ests$C2VC3$OR, C2VC3_SE = ests$C2VC3$SE, C2VC3_sig = ests$C2VC3$sig, C2VC3_PAF = ests$C2VC3$PAF,
                     C1VC2_abs_log_OR = ests$C1VC2$abs_log_OR, C1VC2_OR = ests$C1VC2$OR, C1VC2_SE = ests$C1VC2$SE, C1VC2_sig = ests$C1VC2$sig, C1VC2_PAF = ests$C1VC2$PAF)
sig <- unique(allests[allests$sig %in% T,'labs']) #390
exclude <- unique(allests[allests$exclude %in% T,'labs']) #80
low_cell <- unique(allests[allests$low_cell %in% T,'labs']) #239 in total, #75 excluded, 164 remaining
#sig[!sig %in% exclude] #number of significant not excluded = 378

excldf <- data.frame(labs = exclude)
excldf$zeros <- ifelse(excldf$labs %in% unique(allests[allests$zeros %in% T,'labs']), T, F) #63
excldf$low_n <- ifelse(excldf$labs %in% unique(allests[allests$low_n %in% T,'labs']), T, F) #58
excldf$largese <- ifelse(excldf$labs %in% unique(allests[allests$largese %in% T,'labs']), T, F) #9

write.xlsx(pltdf, sheetName="Labels and Ns", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Reports/exposures.xlsx')
write.xlsx(allexp[!allexp$labs %in% exclude,], sheetName="All exposures", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Reports/exposures.xlsx', append=T)
write.xlsx(allexp[!allexp$labs %in% exclude & allexp$labs %in% sig & !allexp$labs %in% low_cell,], sheetName="Significant exposures, no low cells", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Reports/exposures.xlsx', append=T)
write.xlsx(allexp[!allexp$labs %in% exclude & allexp$labs %in% sig & allexp$labs %in% low_cell,], sheetName="Significant exposures, low cells", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Reports/exposures.xlsx', append=T)
write.xlsx(excldf, sheetName="Excluded", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Reports/exposures.xlsx', append=T)
rm(allexp, excldf, sig, exclude, low_cell)



##TOP HITS OR
#with low cells counts (108 unique)
hits <- allests[allests$exclude %in% F & allests$sig %in% T,]
hits <- split(hits, hits$comparison)
hits <- lapply(hits, function(x){x <- x[order(abs(x$abs_log_OR),decreasing=T),]})
hits <- lapply(hits, function(x){split(x, x$ages_sub)})
hits <- lapply(hits, lapply, head, 5)
hits <- lapply(hits, function(x){do.call(rbind, x)})
hits <- do.call(rbind, hits)
#spurious <- unique(hits$labs[hits$labs %in% low_cell])
#split(pltdf[pltdf$labs %in% spurious,'labs'],pltdf[pltdf$labs %in% spurious,'ages_sub'])
hits <- hits[,c('ages_sub','long_labs','OR','SE')]
hits <- split(hits, hits$ages_sub)
hits <- lapply(hits, function(x) x[!(names(x) %in% 'ages_sub')])
hits <- do.call(cbind, hits)
rownames(hits) <- NULL
write.xlsx(hits, file="C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/HITS top 5 OR sig with low cell counts.xlsx")

#no low cells counts (90 unique)+1NA
hits <- allests[allests$exclude %in% F & allests$sig %in% T & allests$low_cell %in% F,]
hits <- split(hits, hits$comparison)
hits <- lapply(hits, function(x){x <- x[order(abs(x$abs_log_OR),decreasing=T),]})
hits <- lapply(hits, function(x){split(x, x$ages_sub)})
hits <- lapply(hits, lapply, head, 5)
hits <- lapply(hits, function(x){
  out <- list()
  for(i in names(x)){
    if(nrow(x[[i]]) < 5){
    out[[i]] <- x[[i]]
    out[[i]][c((nrow(out[[i]])+1):5),] <- NA
    out[[i]][,'ages_sub'] <- rep(i, 5)
    } else {
    out[[i]] <- x[[i]]
  }}
  return(out)
})
hits <- lapply(hits, function(x){do.call(rbind, x)})
hits <- do.call(rbind, hits)
hits <- hits[,c('ages_sub','long_labs','OR','SE')]
hits <- split(hits, hits$ages_sub)
hits <- lapply(hits, function(x) x[!(names(x) %in% 'ages_sub')])
hits <- do.call(cbind, hits)
rownames(hits) <- NULL
write.xlsx(hits, file="C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/HITS top 5 OR sig without low cell count.xlsx")



##TOP HITS PAF
#with low cells (85 unique)+1NA
hits2 <- allests[allests$exclude %in% F & allests$sig %in% T & allests$modif %in% T,]
hits2 <- split(hits2, hits2$comparison)
hits2 <- lapply(hits2, function(x){x <- x[order(abs(x$PAF),decreasing=T),]})
hits2 <- lapply(hits2, function(x){split(x, x$ages_sub)})
hits2 <- lapply(hits2, lapply, head, 5)
hits2 <- lapply(hits2, function(x){
  out <- list()
  for(i in names(x)){
    if(nrow(x[[i]]) < 5){
      out[[i]] <- x[[i]]
      out[[i]][c((nrow(out[[i]])+1):5),] <- NA
      out[[i]][,'ages_sub'] <- rep(i, 5)
    } else {
      out[[i]] <- x[[i]]
    }}
  return(out)
})
hits2 <- lapply(hits2, function(x){do.call(rbind, x)})
hits2 <- do.call(rbind, hits2)
hits2 <- hits2[,c('ages_sub','long_labs','PAF','PAR')]
hits2 <- split(hits2, hits2$ages_sub)
hits2 <- lapply(hits2, function(x) x[!(names(x) %in% 'ages_sub')])
hits2 <- do.call(cbind, hits2)
rownames(hits2) <- NULL
write.xlsx(hits2, file="C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/HITS top 5 PAF sig with low cell count.xlsx")

#no low cells (86 unique)+1NA
hits2 <- allests[allests$exclude %in% F & allests$sig %in% T & allests$modif %in% T & allests$low_cell %in% F,]
hits2 <- split(hits2, hits2$comparison)
hits2 <- lapply(hits2, function(x){x <- x[order(abs(x$PAF),decreasing=T),]})
hits2 <- lapply(hits2, function(x){split(x, x$ages_sub)})
hits2 <- lapply(hits2, lapply, head, 5)
hits2 <- lapply(hits2, function(x){
  out <- list()
  for(i in names(x)){
    if(nrow(x[[i]]) < 5){
      out[[i]] <- x[[i]]
      out[[i]][c((nrow(out[[i]])+1):5),] <- NA
      out[[i]][,'ages_sub'] <- rep(i, 5)
    } else {
      out[[i]] <- x[[i]]
    }}
  return(out)
})
hits2 <- lapply(hits2, function(x){do.call(rbind, x)})
hits2 <- do.call(rbind, hits2)
hits2 <- hits2[,c('ages_sub','long_labs','PAF','PAR')]
hits2 <- split(hits2, hits2$ages_sub)
hits2 <- lapply(hits2, function(x) x[!(names(x) %in% 'ages_sub')])
hits2 <- do.call(cbind, hits2)
rownames(hits2) <- NULL
write.xlsx(hits2, file="C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/HITS top 5 PAF sig without low cell count.xlsx")



##OVERLAP
#with low cell counts
#all
unique(c(hits$labs, hits2$labs)) #153
#overlap
unique(hits$labs)[unique(hits$labs) %in% unique(hits2$labs)] #40
#highest OR but not highest PAF
unique(hits$labs)[!unique(hits$labs) %in% unique(hits2$labs)] #68
#highest PAF but not highest OR
unique(hits2$labs)[!unique(hits2$labs) %in% unique(hits$labs)] #45

#no low cell counts
unique(c(hits$labs, hits2$labs)) #120
#overlap
unique(hits$labs)[unique(hits$labs) %in% unique(hits2$labs)] #58
#highest OR but not highest PAF
unique(hits$labs)[!unique(hits$labs) %in% unique(hits2$labs)] #33
#highest PAF but not highest OR
unique(hits2$labs)[!unique(hits2$labs) %in% unique(hits$labs)] #29



##STATS
stat <- allests[allests$exclude %in% F & allests$sig %in% T,] #include non-sig???
stat <- split(stat, stat$comparison)
stat <- lapply(stat, function(x){split(x, x$ages_sub)})

statit <- function(dat){
  mean <- list()
  range <- list()
  for(i in names(dat)){
    mean[[i]] <- lapply(dat[[i]], function(x){mean(x$OR)})
    range[[i]] <- lapply(dat[[i]], function(x){range(x$OR)})
  }
  out <- list(mean=mean, range=range)
  return(out)
}
stat <- statit(stat)



##N SIG
sign <- function(dat){
  nsig <- vector()
  for(i in unique(dat$labs)){
    nsig[i] <- sum(dat$sig[dat$labs %in% i])
  }
  nsig <- data.frame(cbind(pltdf[pltdf$labs %in% dat$labs,c('sub_grp','grp','labs','ages_sub','ages')],n=nsig))
  return(nsig)
}

##N SIG WITH ORs
sign <- function(dat){
  nsig <- vector()
  OR <- vector()
  min <- vector()
  max <- vector()
  for(i in unique(dat$labs)){
    nsig[i] <- sum(dat$sig[dat$labs %in% i])
    OR[i] <- round(mean(dat$OR[dat$labs %in% i]),2)
    min[i] <- range(dat$OR[dat$labs %in% i])[1]
    max[i] <- range(dat$OR[dat$labs %in% i])[2]
  }
  out <- data.frame(cbind(pltdf[pltdf$labs %in% dat$labs,c('sub_grp','grp','labs','ages_sub','ages')], OR=OR, min=min, max=max, n=nsig))
  return(out)
}

#with low cell counts (149 unique)
nsig_nosmk <- sign(allests[allests$exclude %in% F & allests$comparison %in% levels(allests$comparison)[1:4],])
nsig_stsmk <- sign(allests[allests$exclude %in% F & allests$comparison %in% levels(allests$comparison)[4:7],])
nsig_ocsmk <- sign(allests[allests$exclude %in% F & allests$comparison %in% levels(allests$comparison)[c(3,7:9)],])
nsig_lasmk <- sign(allests[allests$exclude %in% F & allests$comparison %in% levels(allests$comparison)[c(2,6,9:10)],])
nsig_easmk <- sign(allests[allests$exclude %in% F & allests$comparison %in% levels(allests$comparison)[c(1,5,8,10)],])

#no low cell counts (93 unique)
nsig_nosmk <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F & allests$comparison %in% levels(allests$comparison)[1:4],])
nsig_stsmk <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F & allests$comparison %in% levels(allests$comparison)[4:7],])
nsig_ocsmk <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F & allests$comparison %in% levels(allests$comparison)[c(3,7:9)],])
nsig_lasmk <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F & allests$comparison %in% levels(allests$comparison)[c(2,6,9:10)],])
nsig_easmk <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F & allests$comparison %in% levels(allests$comparison)[c(1,5,8,10)],])

#list labs
labs_sig <- list(
  gestation = list(
    nsig_nosmk$labs[nsig_nosmk$ages_sub %in% 'Gestation' & nsig_nosmk$n %in% 4],
    nsig_stsmk$labs[nsig_stsmk$ages_sub %in% 'Gestation' & nsig_stsmk$n %in% 4],
    nsig_ocsmk$labs[nsig_ocsmk$ages_sub %in% 'Gestation' & nsig_ocsmk$n %in% 4],
    nsig_lasmk$labs[nsig_lasmk$ages_sub %in% 'Gestation' & nsig_lasmk$n %in% 4],
    nsig_easmk$labs[nsig_easmk$ages_sub %in% 'Gestation' & nsig_easmk$n %in% 4]
  ), 
  early_childhood = list(
    nsig_nosmk$labs[nsig_nosmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_nosmk$n %in% 4],
    nsig_stsmk$labs[nsig_stsmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_stsmk$n %in% 4],
    nsig_ocsmk$labs[nsig_ocsmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_ocsmk$n %in% 4],
    nsig_lasmk$labs[nsig_lasmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_lasmk$n %in% 4],
    nsig_easmk$labs[nsig_easmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_easmk$n %in% 4]
  ),
  late_childhood = list(
    nsig_nosmk$labs[nsig_nosmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_nosmk$n %in% 4],
    nsig_stsmk$labs[nsig_stsmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_stsmk$n %in% 4],
    nsig_ocsmk$labs[nsig_ocsmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_ocsmk$n %in% 4],
    nsig_lasmk$labs[nsig_lasmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_lasmk$n %in% 4],
    nsig_easmk$labs[nsig_easmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_easmk$n %in% 4]
  ),
  adolescence = list(
    nsig_nosmk$labs[nsig_nosmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_nosmk$n %in% 4],
    nsig_stsmk$labs[nsig_stsmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_stsmk$n %in% 4],
    nsig_ocsmk$labs[nsig_ocsmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_ocsmk$n %in% 4],
    nsig_lasmk$labs[nsig_lasmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_lasmk$n %in% 4],
    nsig_easmk$labs[nsig_easmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_easmk$n %in% 4]
  ),
  adult_longit = list(
    nsig_nosmk$labs[nsig_nosmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_nosmk$n %in% 4],
    nsig_stsmk$labs[nsig_stsmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_stsmk$n %in% 4],
    nsig_ocsmk$labs[nsig_ocsmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_ocsmk$n %in% 4],
    nsig_lasmk$labs[nsig_lasmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_lasmk$n %in% 4],
    nsig_easmk$labs[nsig_easmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_easmk$n %in% 4]
  ))
labs_sig <- lapply(labs_sig, setNames, c('nosmk','stsmk','ocsmk','lasmk','easmk'))
#allests[allests$labs %in% 'sex' & allests$comparison %in% levels(allests$comparison)[4:7],c('labs','OR','comparison','sig')]
#rm(nsig_nosmk, nsig_stsmk, nsig_ocsmk, nsig_lasmk, nsig_easmk)



#list all
labs_sig <- list(
  gestation = list(
    nsig_nosmk[nsig_nosmk$ages_sub %in% 'Gestation' & nsig_nosmk$n %in% 4,],
    nsig_stsmk[nsig_stsmk$ages_sub %in% 'Gestation' & nsig_stsmk$n %in% 4,],
    nsig_ocsmk[nsig_ocsmk$ages_sub %in% 'Gestation' & nsig_ocsmk$n %in% 4,],
    nsig_lasmk[nsig_lasmk$ages_sub %in% 'Gestation' & nsig_lasmk$n %in% 4,],
    nsig_easmk[nsig_easmk$ages_sub %in% 'Gestation' & nsig_easmk$n %in% 4,]
  ), 
  early_childhood = list(
    nsig_nosmk[nsig_nosmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_nosmk$n %in% 4,],
    nsig_stsmk[nsig_stsmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_stsmk$n %in% 4,],
    nsig_ocsmk[nsig_ocsmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_ocsmk$n %in% 4,],
    nsig_lasmk[nsig_lasmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_lasmk$n %in% 4,],
    nsig_easmk[nsig_easmk$ages_sub %in% 'Early Childhood (age 0-7)' & nsig_easmk$n %in% 4,]
  ),
  late_childhood = list(
    nsig_nosmk[nsig_nosmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_nosmk$n %in% 4,],
    nsig_stsmk[nsig_stsmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_stsmk$n %in% 4,],
    nsig_ocsmk[nsig_ocsmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_ocsmk$n %in% 4,],
    nsig_lasmk[nsig_lasmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_lasmk$n %in% 4,],
    nsig_easmk[nsig_easmk$ages_sub %in% 'Late Childhood (age 8-12)' & nsig_easmk$n %in% 4,]
  ),
  adolescence = list(
    nsig_nosmk[nsig_nosmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_nosmk$n %in% 4,],
    nsig_stsmk[nsig_stsmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_stsmk$n %in% 4,],
    nsig_ocsmk[nsig_ocsmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_ocsmk$n %in% 4,],
    nsig_lasmk[nsig_lasmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_lasmk$n %in% 4,],
    nsig_easmk[nsig_easmk$ages_sub %in% 'Adolescence (age 13-18)' & nsig_easmk$n %in% 4,]
  ),
  adult_longit = list(
    nsig_nosmk[nsig_nosmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_nosmk$n %in% 4,],
    nsig_stsmk[nsig_stsmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_stsmk$n %in% 4,],
    nsig_ocsmk[nsig_ocsmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_ocsmk$n %in% 4,],
    nsig_lasmk[nsig_lasmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_lasmk$n %in% 4,],
    nsig_easmk[nsig_easmk$ages_sub %in% 'Adulthood and Longitudinal' & nsig_easmk$n %in% 4,]
  ))
labs_sig <- lapply(labs_sig, setNames, c('nosmk','stsmk','ocsmk','lasmk','easmk'))



#with low cell counts
nsig <- sign(allests[allests$exclude %in% F,])
tapply(nsig$n, nsig$ages_sub, mean)[order(tapply(nsig$n, nsig$ages_sub, mean),decreasing=T)]
tapply(nsig$n, nsig$sub_grp, mean)[order(tapply(nsig$n, nsig$sub_grp, mean),decreasing=T)]
nsig$labs[nsig$n %in% 10] #1
nsig$labs[nsig$n %in% 9] #17
nsig$labs[nsig$n %in% 0] #58

#no low cell counts
nsig <- sign(allests[allests$exclude %in% F & allests$low_cell %in% F,])
tapply(nsig$n, nsig$ages_sub, mean)[order(tapply(nsig$n, nsig$ages_sub, mean),decreasing=T)]
tapply(nsig$n, nsig$sub_grp, mean)[order(tapply(nsig$n, nsig$sub_grp, mean),decreasing=T)]
nsig$labs[nsig$n %in% 10] #1
nsig$labs[nsig$n %in% 9] #12
nsig$labs[nsig$n %in% 0] #25


#most important hits (high OR, high PAF, all comps)
important <- unique(hits$labs)[unique(hits$labs) %in% unique(hits2$labs)] #58 without low cells, 51 with low cells
important2 <- unique(unname(unlist(unlist(labs_sig,recursive = F)))) #93 without low cells, 149 with low cells
vimportant <- important[important %in% important2]
allimportant <- unique(c(important, important2))
save(allimportant, file = 'C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/allimportant.rda')
