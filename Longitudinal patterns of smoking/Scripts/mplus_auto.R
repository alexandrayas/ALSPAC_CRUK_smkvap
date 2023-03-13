##LOAD
library(haven)
library(MplusAutomation)
library(labelled)
library(glue)



##DATA
alldf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/all_measures.dta')
alldf <- to_factor(alldf, levels='labels', sort_levels='auto')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/pltdf.rda')
labs <- pltdf$labs
setwd('3step')
#setwd('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/3step')

#SWAP ORDER OF PREGNANCT/PARENTHOOD VARIABLES

##MODELS
mplusit <- function(labs, dat){
  
  ms <- list()
  m.fits <- list()
  outs <- list()
  
  for(i in 1:length(labs)){
    
    ms[[labs[i]]] <- mplusObject(
      
      TITLE = glue("MplusAuto 3step {labs[i]}"),
      DATA = "listwise=on;",
      
      VARIABLE = glue("
      usevar = Cl {labs[i]};
      nominal = Cl;
      categorical = {labs[i]};
      classes = class(5);
    "),
      
      DEFINE = glue("
      if ({labs[i]} eq 1) then {labs[i]} = 0;
      if ({labs[i]} eq 2) then {labs[i]} = 1;
    "),
      
      ANALYSIS = "
      Type = mixture;
      proc = 4 (starts);
      starts = 500 100;
      algorithm = integration;
    ",
      
      MODEL = glue("
      %overall%

      [{labs[i]}$1] (x_thr);

      [class#1]  (int_x0c1);
      [class#2]  (int_x0c2);
      [class#3]  (int_x0c3);
      [class#4]  (int_x0c4);

      class#1 on {labs[i]} (x_eff1);
      class#2 on {labs[i]} (x_eff2);
      class#3 on {labs[i]} (x_eff3);
      class#4 on {labs[i]} (x_eff4);
    
      %class#1%
        [Cl#1@8.745 Cl#2@6.189 Cl#3@2.536 Cl#4@5.019];
      %class#2%
        [Cl#1@2.577 Cl#2@5.191 Cl#3@2.564 Cl#4@1.171];
      %class#3%
        [Cl#1@-6.164 Cl#2@-1.333 Cl#3@1.275 Cl#4@-1.827];
      %class#4%
        [Cl#1@0.076 Cl#2@-0.229 Cl#3@1.089 Cl#4@2.731];
      %class#5%
        [Cl#1@-13.096 Cl#2@-7.577 Cl#3@-3.739 Cl#4@-5.817];
    
    "),
      
      MODELCONSTRAINT = "
      new (x_prev);
        x_prev = exp((-1)*x_thr)/(1 + exp((-1)*x_thr));

      new (temp_0c1 temp_0c2 temp_0c3 temp_0c4 px0_c1 px0_c2 px0_c3 px0_c4 px0_c5);
        temp_0c1 = exp(int_x0c1);
        temp_0c2 = exp(int_x0c2);
        temp_0c3 = exp(int_x0c3);
        temp_0c4 = exp(int_x0c4);
        px0_c1 = temp_0c1/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c2 = temp_0c2/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c3 = temp_0c3/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c4 = temp_0c4/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c5 = 1/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);

      new (temp_1c1 temp_1c2 temp_1c3 temp_1c4 px1_c1 px1_c2 px1_c3 px1_c4 px1_c5);
        temp_1c1 = exp(int_x0c1 + x_eff1);
        temp_1c2 = exp(int_x0c2 + x_eff2);
        temp_1c3 = exp(int_x0c3 + x_eff3);
        temp_1c4 = exp(int_x0c4 + x_eff4);
        px1_c1 = temp_1c1/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c2 = temp_1c2/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c3 = temp_1c3/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c4 = temp_1c4/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c5 = 1/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);

      new(cell_1 cell_2 cell_3 cell_4 cell_5 cell_6 cell_7 cell_8 cell_9 cell_10);
        cell_1 = (1 - x_prev)*px0_c1;
        cell_2 = (1 - x_prev)*px0_c2;
        cell_3 = (1 - x_prev)*px0_c3;
        cell_4 = (1 - x_prev)*px0_c4;
        cell_5 = (1 - x_prev)*px0_c5;
        cell_6 = x_prev*px1_c1;
        cell_7 = x_prev*px1_c2;
        cell_8 = x_prev*px1_c3;
        cell_9 = x_prev*px1_c4;
        cell_10 = x_prev*px1_c5;
      ",
      
      rdata = dat)
    
    m.fits[[labs[i]]] <- mplusModeler(object=ms[[labs[i]]],
                                      modelout=paste0("5cl_llca_3step_",labs[i], ".inp"), 
                                      run = 1L, hashfilename = FALSE, writeData = 'always')
    
    outs[[labs[i]]] <- m.fits[[i]]$results$parameters$unstandardized[m.fits[[i]]$results$parameters$unstandardized$paramHeader %in% c('New.Additional.Parameters'),c('param','est','se','est_se','pval')]
    
  }
  return(outs)
}

outs <- mplusit(labs[1:50],alldf)
outs <- c(outs, mplusit(labs[51:100],alldf))
outs <- c(outs, mplusit(labs[101:150],alldf))
outs <- c(outs, mplusit(labs[151:200],alldf))
outs <- c(outs, mplusit(labs[201:250],alldf))
outs <- c(outs, mplusit(labs[251:300],alldf))
outs <- c(outs, mplusit(labs[301:350],alldf))
outs <- c(outs, mplusit(labs[351:400],alldf))
outs <- c(outs, mplusit(labs[401:450],alldf))
outs <- c(outs, mplusit(labs[451:500],alldf))
outs <- c(outs, mplusit(labs[501:length(labs)],alldf))
save(outs, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/outs.rda')



##ESTIMATES
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/outs.rda')

estit <- function(x,a,b,c,d){
  #a = exposed in worse group
  #b = exposed in better group
  #c = unexposed in worse group
  #d = unexposed in better group
  columns <- c('labs','labs_unexp','labs_exp','sum','n_unexp','n_exp','R_unexp','R_exp','prop_exp','PAR','PAF','OR','SE')
  out <- data.frame(matrix(nrow = nrow(pltdf), ncol = length(columns))) 
  colnames(out) <- columns
  rownames(out) <- pltdf$labs
  out$labs <- pltdf$labs
  out$labs_unexp <- pltdf$labs_1s
  out$labs_exp <- pltdf$labs_2s
  for(i in out$labs){
    if("0.000" %in% x[[i]][,'est'] | 0 %in% x[[i]][,'est']){
      out[i, -1] <- NA
    } else {
      cells <- x[[i]][x[[i]]$param %in% c(c,d,a,b),'est'] * pltdf[pltdf$labs %in% i,'sum']
      names(cells) <- c(c,d,a,b)
      out[i,'sum'] <- sum(cells)
      out[i,'n_unexp'] <- sum(cells[c(c,d)])
      out[i,'n_exp'] <- sum(cells[c(a,b)])
      out[i,'R_unexp'] <- cells[c]/(cells[c] + cells[d])
      out[i,'R_exp'] <- cells[a]/(cells[a] + cells[b])
      out[i,'prop_exp'] <- ((cells[a] + cells[b])) / sum(cells)
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
  out <- cbind(out, pltdf[,c('long_labs','sub_grp','grp','ages','ages_sub')])
  return(out)
}


ests <- list()
ests$C1VC5 <- estit(outs,'CELL_6','CELL_10','CELL_1','CELL_5')
ests$C2VC5 <- estit(outs,'CELL_7','CELL_10','CELL_2','CELL_5')
ests$C3VC5 <- estit(outs,'CELL_8','CELL_10','CELL_3','CELL_5')
ests$C4VC5 <- estit(outs,'CELL_9','CELL_10','CELL_4','CELL_5')
ests$C1VC4 <- estit(outs,'CELL_6','CELL_9','CELL_1','CELL_4')
ests$C2VC4 <- estit(outs,'CELL_7','CELL_9','CELL_2','CELL_4')
ests$C3VC4 <- estit(outs,'CELL_8','CELL_9','CELL_3','CELL_4')
ests$C1VC3 <- estit(outs,'CELL_6','CELL_8','CELL_1','CELL_3')
ests$C2VC3 <- estit(outs,'CELL_7','CELL_8','CELL_2','CELL_3')
ests$C1VC2 <- estit(outs,'CELL_6','CELL_7','CELL_1','CELL_2')
save(ests, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/ests.rda')



##ALL ESTIMATES DF
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/ests.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/outs.rda')

allests <- do.call(rbind, ests)
allests$comparison <- rep(c('Early-onset Smokers vs Non-smokers',
                            'Late-onset Smokers vs Non-smokers',
                            'Occasional Smokers vs Non-smokers',
                            'Short-term Smokers vs Non-smokers',
                            'Early-onset Smokers vs Short-term Smokers',
                            'Late-onset Smokers vs Short-term Smokers',
                            'Occasional Smokers vs Short-term Smokers',
                            'Early-onset Smokers vs Occasional Smokers',
                            'Late-onset Smokers vs Occasional Smokers',
                            'Early-onset Smokers vs Late-onset Smokers'),
                          each=nrow(pltdf))
allests$comparison <- factor(allests$comparison, levels=unique(allests$comparison))

low_sampsize <- unique(pltdf[pltdf$sum < 500, 'labs']) #7
low_cases <- unique(c(pltdf[pltdf$n_1s < 50, 'labs'], pltdf[pltdf$n_2s < 50, 'labs'])) #54
largese <- unique(allests[!is.na(allests$SE) & allests$SE > 0.7, 'labs']) #9
zeros <- unique(allests[allests$zeros %in% T, 'labs']) #63
exclude <- unique(c(low_sampsize, low_cases, largese, zeros)) #80
modif <- c("Parental smoking","Parental substance use","Parental mental health", "Peer substance use",
           "Other substance use","Mental health and wellbeing","BMI and Diet","Physical activity and Sleep",
           "Parental SEP","Parental education","Cotinine","Education","Employment","Neighbourhood deprivation") 
#not included: sex, ethnicity, ACEs, trauma, pregnancy and parenthood (28 removed, 488 labs remaining)
allests$low_n <- ifelse(allests$labs %in% unique(c(low_sampsize,low_cases)), T, F)
allests$largese <- ifelse(allests$SE > 0.7, T, F)
allests$exclude <- ifelse(allests$labs %in% exclude, T, F)
allests$modif <- ifelse(allests$sub_grp %in% modif, T, F)

#low cells
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
low_cell <- rownames(cells[cells$n > 0,])
allests$low_cell <- ifelse(allests$labs %in% low_cell, T, F)
save(allests, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/allests.rda')
rm(low_sampsize,low_cases,largese,zeros,exclude,modif,checkit,cells,low_cell)



#class counts
cells$n <- NULL
cells <- cells * pltdf$sum
pltdf$sum_cl1 <- rowSums(cells[,c('CELL_1','CELL_6')])
pltdf$sum_cl2 <- rowSums(cells[,c('CELL_2','CELL_7')])
pltdf$sum_cl3 <- rowSums(cells[,c('CELL_3','CELL_8')])
pltdf$sum_cl4 <- rowSums(cells[,c('CELL_4','CELL_9')])
pltdf$sum_cl5 <- rowSums(cells[,c('CELL_5','CELL_10')])
save(pltdf, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/pltdf.rda')


