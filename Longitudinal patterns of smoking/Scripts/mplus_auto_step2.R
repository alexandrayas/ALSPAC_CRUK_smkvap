##STEP 2
ms2 <- mplusObject(
  
  TITLE = "MplusAuto Step 2",
  
  VARIABLE = "
      nominal=N;
      usevar=n;
      classes = c(5);
    ",
  
  ANALYSIS = "
      type = mixture;
      proc = 4 (starts);
      starts = 500 100;
    ",
  
  MODEL = 
    glue("
  %C#1%
  [n#1@{logit_cprobs[1,1]}];
  [n#2@{logit_cprobs[1,2]}];
  [n#3@{logit_cprobs[1,3]}];
  [n#4@{logit_cprobs[1,4]}];
  
  %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  
  %C#3%
  [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
   
  %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
      
  %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  "),
  
  usevariables = colnames(savedata), 
  rdata = savedata)

mfit2 <- mplusModeler(object=ms2,
                       modelout=paste0("5cl_llca_step2", ".inp"),
                       dataout=paste0("5cl_llca_step2", ".dat"),
                       run = 1L, hashfilename = FALSE, writeData = 'always')
#ERROR: No PROPORTION OF DATA PRESENT sections found within COVARIANCE COVERAGE OF DATA output.


##COMBINE DATASETS
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
labs <- pltdf$labs

expdf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_measures.dta')
expdf <- to_factor(expdf, levels='labels', sort_levels='auto')
savedata$qlet <- as.character(factor(savedata$qlet, levels=c(1:2), labels=c('A','B')))
expdf <- expdf[,c('aln','qlet',labs)]


alldf <- merge(savedata, expdf, by=c('aln','qlet'))
alldf[,smk_labs] <- sapply(alldf[,smk_labs], factor, levels=c(0:2), labels=c('Non-smoker','Occasional smoker','Regular smoker'))
alldf <- alldf[,c('aln','N',smk_labs,labs,'qlet')]
alldf$Cl <- factor(alldf$N, levels = c(1:5), labels=c('Non-smoking','Early onset smoking','Short-term smoking','Occasional smoking','Late onset smoking'))
write_dta(data=alldf, path='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/all_measures.dta')
rm(mfit2,ms2,expdf,pltdf,smk_labs)