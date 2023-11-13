##LOAD
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/outs.rda')



##LOW CELLS
checkit <- function(x){
  cells <- list()
  for(i in pltdf$labs){
    cells[[i]] <- as.numeric(x[[i]][grep('CELL',x[[i]]$param),'est'])
  }
  out <- data.frame(do.call('rbind',cells))
  colnames(out) <- paste0('CELL_',c(1:10))
  return(out)
}
cells <- checkit(outs)
cells$n <- rowSums(cells<0.01)
low_cell <- rownames(cells[cells$n > 0,]) #226

cells$n <- NULL
cells <- cells * pltdf$sum
pltdf$sum_cl1 <- rowSums(cells[,c('CELL_1','CELL_6')])
pltdf$sum_cl2 <- rowSums(cells[,c('CELL_2','CELL_7')])
pltdf$sum_cl3 <- rowSums(cells[,c('CELL_3','CELL_8')])
pltdf$sum_cl4 <- rowSums(cells[,c('CELL_4','CELL_9')])
pltdf$sum_cl5 <- rowSums(cells[,c('CELL_5','CELL_10')])
save(pltdf, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')



##ESTIMATES
estit <- function(x,a,b,c,d){
  #a = exposed in positive (bad) outcome
  #b = exposed in negative (good) outcome
  #c = unexposed in positive (bad) outcome
  #d = unexposed in negative (good) outcome
  columns <- c('labs','labs_unexp','labs_exp','n','n_unexp','n_exp','R_unexp','R_exp','prop_exp','PAR','PAF','OR','SE')
  out <- data.frame(matrix(nrow = nrow(pltdf), ncol = length(columns))) 
  colnames(out) <- columns
  rownames(out) <- pltdf$labs
  out$labs <- pltdf$labs
  out$labs_unexp <- pltdf$labs_unexp
  out$labs_exp <- pltdf$labs_exp
  for(i in out$labs){
    if("0.000" %in% x[[i]][,'est'] | 0 %in% x[[i]][,'est']){
      out[i, -1] <- NA
    } else {
      cells <- x[[i]][match(c(c,d,a,b), x[[i]]$param),'est'] * pltdf[pltdf$labs %in% i,'sum']
      names(cells) <- c(c,d,a,b)
      out[i,'n'] <- sum(cells)
      out[i,'n_unexp'] <- sum(cells[c(c,d)])
      out[i,'n_exp'] <- sum(cells[c(a,b)])
      out[i,'R_unexp'] <- cells[c]/(cells[c] + cells[d])
      out[i,'R_exp'] <- cells[a]/(cells[a] + cells[b])
      out[i,'prop_exp'] <- (((cells[a] + cells[b])) / sum(cells))*100
      out[i,'PAR'] <- ((cells[a]+cells[b])*(out[i,'R_exp']-out[i,'R_unexp'])/(cells[c]+cells[a]))*100
      out[i,'PAF'] <- out[i,'prop_exp']*((out[i,'R_exp']/out[i,'R_unexp'])-1)/1+out[i,'prop_exp']*((out[i,'R_exp']/out[i,'R_unexp'])-1)
      out[i,'OR'] <- (cells[a] * cells[d])/(cells[c] * cells[b])
      out[i,'SE'] <- sqrt((1/cells[c])+(1/cells[a])+(1/cells[d])+(1/cells[b]))
    }}
  out$attr_R <- out$R_exp-out$R_unexp
  out$attr_R_perc <- ((out$R_exp-out$R_unexp)/out$R_exp)*100
  out$RR <- out$R_exp / out$R_unexp
  out$log_OR <- log(out$OR)
  out$abs_log_OR <- abs(out$log_OR)
  out$log_LCI <- out$log_OR - 1.96 * out$SE
  out$log_UCI <- out$log_OR + 1.96 * out$SE
  out$LCI <- exp(out$log_LCI)
  out$UCI <- exp(out$log_UCI)
  out$sig <- ifelse(out$LCI < 1 & out$UCI > 1, F, T)
  out$zeros <- ifelse(is.na(out$RR), T, F)
  out[,sapply(out,is.numeric)] <- round(out[,sapply(out,is.numeric)],2)
  out <- cbind(out, pltdf[,c('long_labs','long_labs_noages','sub_grp','grp','lab_grp','ages','ages_sub')])
  return(out)
}

#CELL_1, CELL_6 = non-smoking
#CELL_2, CELL_7 = early-onset smoking
#CELL_3, CELL_8 = short-term smoking
#CELL_4, CELL_9 = occasional smoking
#CELL_5, CELL_10 = late-onset smoking
#CELL_1, CELL_2, CELL_3, CELL_4, CELL_5 = unexposed
#CELL_6, CELL_7, CELL_8, CELL_9,CELL_10 = exposed
ests <- list()
ests$C2VC1 <- estit(outs,'CELL_7','CELL_6','CELL_2','CELL_1')
ests$C5VC1 <- estit(outs,'CELL_10','CELL_6','CELL_5','CELL_1')
ests$C3VC1 <- estit(outs,'CELL_8','CELL_6','CELL_3','CELL_1')
ests$C4VC1 <- estit(outs,'CELL_9','CELL_6','CELL_4','CELL_1')

ests$C2VC4 <- estit(outs,'CELL_7','CELL_9','CELL_2','CELL_4')
ests$C5VC4 <- estit(outs,'CELL_10','CELL_9','CELL_5','CELL_4')
ests$C3VC4 <- estit(outs,'CELL_8','CELL_9','CELL_3','CELL_4')

ests$C2VC3 <- estit(outs,'CELL_7','CELL_8','CELL_2','CELL_3')
ests$C5VC3 <- estit(outs,'CELL_10','CELL_8','CELL_5','CELL_3')

ests$C2VC5 <- estit(outs,'CELL_7','CELL_10','CELL_2','CELL_5')
save(ests, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/ests.rda')



##LONG DF
allests <- do.call(rbind, ests)
allests$comparison <- rep(c('Early-onset smoking vs Non-smoking',
                            'Late-onset smoking vs Non-smoking',
                            'Short-term smoking vs Non-smoking',
                            'Occasional smoking vs Non-smoking',
                            'Early-onset smoking vs Occasional smoking',
                            'Late-onset smoking vs Occasional smoking',
                            'Short-term smoking vs Occasional smoking',
                            'Early-onset smoking vs Short-term smoking',
                            'Late-onset smoking vs Short-term smoking',
                            'Early-onset smoking vs Late-onset smoking'),
                          each=nrow(pltdf))
allests$comparison <- factor(allests$comparison, levels=unique(allests$comparison))
allests$ref_cl <- factor(allests$comparison, levels=levels(allests$comparison), labels=c(rep('Non-smoking',4),rep('Occasional smoking',3),rep('Short-term smoking',2),'Late-onset smoking'))
allests$comp_cl <- factor(allests$comparison, levels=levels(allests$comparison), labels=c('Early-onset smoking','Late-onset smoking','Short-term smoking','Occasional smoking','Early-onset smoking','Late-onset smoking','Short-term smoking','Early-onset smoking','Late-onset smoking','Early-onset smoking'))

allests$abs_PAF <- abs(allests$PAF)
allests$RR_inv[!is.na(allests$RR)] <- ifelse(allests$RR[!is.na(allests$RR)] < 1, (1/allests$RR[!is.na(allests$RR)]), allests$RR[!is.na(allests$RR)])
allests$evalue <- allests$RR_inv + sqrt(allests$RR_inv * (allests$RR_inv-1))

low_sampsize <- unique(allests[!is.na(allests$n) & allests$n < 100, 'labs']) #8
low_cases <- unique(c(allests[!is.na(allests$n_unexp) & allests$n_unexp < 25, 'labs'], 
                      allests[!is.na(allests$n_exp) & allests$n_exp < 25, 'labs'])) #69
largese <- unique(allests[!is.na(allests$SE) & allests$SE > 0.7, 'labs']) #10
zeros <- unique(allests[allests$zeros %in% T, 'labs']) #51
exclude <- unique(c(low_sampsize, low_cases, zeros)) #122
table(largese %in% exclude)
modif <- c("Parental smoking","Parental substance use","Parental mental health", "Peer substance use",
           "Other substance use","Mental health and wellbeing","BMI and Diet","Physical activity and Sleep",
           "Parental SEP","Parental education","Education","Employment","Neighbourhood deprivation","ACEs and Trauma") 
#not included: sex, ethnicity, cotinine, pregnancy and parenthood
allests$low_n <- ifelse(allests$labs %in% unique(c(low_sampsize,low_cases)), T, F)
allests$largese <- ifelse(allests$SE > 0.7, T, F)
allests$exclude <- ifelse(allests$labs %in% exclude, T, F)
allests$modif <- ifelse(allests$sub_grp %in% modif, T, F)
pltdf$modif <- ifelse(pltdf$sub_grp %in% modif, T, F)
allests$low_cell <- ifelse(allests$labs %in% low_cell, T, F)
save(allests, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/allests.rda')
save(pltdf, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
rm(low_sampsize,low_cases,largese,zeros,modif,checkit,cells,low_cell)




##ALL 25 COMPARISONS
longests <- list()
longests$nonsmokers <- cbind(rbind(
  estit(outs,'CELL_7','CELL_6','CELL_2','CELL_1'),
  estit(outs,'CELL_10','CELL_6','CELL_5','CELL_1'),
  estit(outs,'CELL_9','CELL_6','CELL_4','CELL_1'),
  estit(outs,'CELL_8','CELL_6','CELL_3','CELL_1')
), vs = rep(c('early onset','late onset','occasional','short term'),each=nrow(pltdf)))
longests$occasional <- cbind(rbind(
  estit(outs,'CELL_7','CELL_9','CELL_2','CELL_4'),
  estit(outs,'CELL_10','CELL_9','CELL_5','CELL_4'),
  estit(outs,'CELL_8','CELL_9','CELL_3','CELL_4'),
  estit(outs,'CELL_6','CELL_9','CELL_1','CELL_4')
), vs = rep(c('early onset','late onset','short term','non smoker'),each=nrow(pltdf)))
longests$short_term <- cbind(rbind(
  estit(outs,'CELL_7','CELL_8','CELL_2','CELL_3'),
  estit(outs,'CELL_10','CELL_8','CELL_5','CELL_3'),
  estit(outs,'CELL_9','CELL_8','CELL_4','CELL_3'),
  estit(outs,'CELL_6','CELL_8','CELL_1','CELL_3')
), vs = rep(c('early onset','late onset','occasional','non smoker'),each=nrow(pltdf)))
longests$late_onset <- cbind(rbind(
  estit(outs,'CELL_7','CELL_10','CELL_2','CELL_5'),
  estit(outs,'CELL_9','CELL_10','CELL_4','CELL_5'),
  estit(outs,'CELL_8','CELL_10','CELL_3','CELL_5'),
  estit(outs,'CELL_6','CELL_10','CELL_1','CELL_5')
), vs = rep(c('early onset','occasional','short term','non smoker'),each=nrow(pltdf)))
longests$early_onset <- cbind(rbind(
  estit(outs,'CELL_10','CELL_7','CELL_5','CELL_2'),
  estit(outs,'CELL_9','CELL_7','CELL_4','CELL_2'),
  estit(outs,'CELL_8','CELL_7','CELL_3','CELL_2'),
  estit(outs,'CELL_6','CELL_7','CELL_1','CELL_2')
), vs = rep(c('late onset','occasional','short term','non smoker'),each=nrow(pltdf)))
longests <- lapply(longests, function(x){x[,c('labs','OR','LCI','UCI','vs','sig','ages_sub')]})
longests <- lapply(longests, function(x){split(x, x$labs)})

subit <- function(x){
  sub <- lapply(longests, function(df){
    out <- vector()
    for(i in names(df)){
      out[i] <- ifelse(!i %in% unique(pltdf$exclude), sum(df[[i]]$sig), NA)
    }
    out <- out %in% 4
  })
  for(i in names(x)){
    x[[i]] <- x[[i]][sub[[i]]]
    x[[i]] <- split(x[[i]], pltdf[match(names(x[[i]]),pltdf$labs),'ages_sub'])
  }
  return(x)
}

longests <- subit(longests)
#sapply(longests, function(x){sapply(x,length)})
#                nonsmokers occasional short_term late_onset early_onset
#Gestation                3          6          1          1          10
#Early Childhood         10          8          1          1          30
#Late Childhood           8          4          1          0           9
#Adolescence             36         10         11          9          26
#Adulthood               26         12          6          7           7
#Longitudinal             4          3          0          0           2
save(longests, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/longests_25.rda')
rm(subit,longests)



#10 COMPARISONS
longests <- list(
  nonsmokers = cbind(rbind(ests$C2VC1,ests$C5VC1,ests$C3VC1,ests$C4VC1),
                     vs = rep(c('early onset','late onset','short term','occasional'),each=nrow(pltdf))),
  
  occasional = cbind(rbind(ests$C2VC4,ests$C5VC4,ests$C3VC4),
                     vs = rep(c('early onset','late onset','short term'),each=nrow(pltdf))),
  
  short_term = cbind(rbind(ests$C2VC3,ests$C5VC3),
                     vs = rep(c('early onset','late onset'),each=nrow(pltdf))),
  
  late_onset = cbind(rbind(ests$C2VC5),
                     vs = rep(c('early onset'),each=nrow(pltdf)))
)
longests <- lapply(longests, function(x){x[,c('labs','OR','LCI','UCI','vs','sig','ages_sub')]})
longests <- lapply(longests, function(x){split(x, x$labs)})

subit <- function(x, n){    
  out <- vector()
  for(i in names(x)){
    out[i] <- ifelse(!i %in% exclude, sum(x[[i]]$sig), NA)
  }
  out <- out %in% n
  x <- x[out]
  return(x)
}

longests$nonsmokers <- subit(longests$nonsmokers, 4)
longests$occasional <- subit(longests$occasional, 3)
longests$short_term <- subit(longests$short_term, 2)
longests$late_onset <- subit(longests$late_onset, 1)
#sapply(longests,length)
#nonsmokers occasional short_term late_onset 
#85         90         30        161
save(longests, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/longests.rda')
rm(subit,estit,exclude)
