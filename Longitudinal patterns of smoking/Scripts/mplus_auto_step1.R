##PACKAGES
library(haven)
library(MplusAutomation)
library(labelled)
library(glue)



##DATA
smokdf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/smoking_categories.dta')
smokdf <- data.frame(smokdf)
smk_labs <- c('smk13','smk14','smk15','smk16','smk17','smk18','smk20','smk21','smk22','smk23','smk24','smk28')
smokdf[,smk_labs] <- sapply(smokdf[,smk_labs], function(x){as.numeric(factor(x, labels=(0:2)))})
smokdf[is.na(smokdf)] <- -9999



##STEP 3
ms1 <- mplusObject(
  
  TITLE = "MplusAuto Step 1",
  DATA = "listwise is off;",
  
  VARIABLE = "
      auxiliary = aln qlet;
      usevar = smk13 smk14 smk15 smk16 smk17 smk18 
      smk20 smk21 smk22 smk23 smk24 smk28;
      categorical = smk13 smk14 smk15 smk16 smk17 smk18 
      smk20 smk21 smk22 smk23 smk24 smk28;
      useobservations = miss_longit eq 0;
      classes = c(5);
      Missing are all (-9999);
    ",
  
  ANALYSIS = "
      type = mixture;
      proc = 4 (starts);
      starts = 500 100;
    ",

  SAVEDATA = "
  file = '5cl_llca_step1_savedata.dat';
  save = cprob;
  missflag= -9999;
  ",
  
  rdata = smokdf)

mfit1 <- mplusModeler(object=ms1,
                       modelout=paste0("5cl_llca_step1", ".inp"),
                       dataout=paste0("5cl_llca_step1", ".dat"),
                       run = 1L, hashfilename = FALSE, writeData = 'always')



#EXTRACT DATA AND LOGITS
logit_cprobs <- as.data.frame(mfit1[["results"]][["class_counts"]][["logitProbs.mostLikely"]])
#   1     2      3      4      5
#1  7.577 -5.519  1.760  3.838 0
#2 -6.189  2.556 -1.170 -3.653 0
#3  0.229  0.304  2.960  1.318 0
#4  1.333 -4.830 -0.493  2.608 0
#5 -5.191 -2.614 -4.020 -2.628 0

savedata <- as.data.frame(mfit1[["results"]][["savedata"]])
colnames(savedata) <- tolower(colnames(savedata))
colnames(savedata)[colnames(savedata)=="c"] <- "N"
savedata[savedata == -9999] <- NA
rm(mfit1,ms1,smokdf)