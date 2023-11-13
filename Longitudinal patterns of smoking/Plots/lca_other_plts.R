##LOAD
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(reshape2)
library(xlsx)

load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/allests.rda")
load("C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/allimportant2.rda")
#range(allests[allests$exclude %in% F, c('log_OR','log_UCI','log_LCI')]) #-2.86  4.73



#DATA
cldf <- read.table('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/Step 1/5cl_llca_step1_savedata.dat')
cldf[cldf == -9999] <- NA
colnames(cldf) <- c('smk13','smk14','smk15','smk16','smk17','smk18','smk20','smk21','smk22','smk23','smk24','smk28','aln','qlet','cprob1','cprob2','cprob3','cprob4','cprob5','Cl')

ns <- data.frame(t(sapply(cldf[,c('smk13','smk14','smk15','smk16','smk17','smk18','smk20','smk21','smk22','smk23','smk24','smk28')],function(x){prop.table(table(x, useNA='always'))})))
colnames(ns) <- c('Regular smoking','Occasional smoking','Non-smoking','Missing')
ns$age <- c(13,14,15,16,17,18,20,21,22,23,24,28)
ns <- melt(ns,id.vars='age')
ns$value <- ns$value*100

cldf <- cbind(data.frame(smk = do.call('c', cldf[,grep('smk',colnames(cldf))]), 
                         age = rep(c(13:18,20:24,28),each=nrow(cldf))),
              sapply(cldf[,grep('cprob',colnames(cldf))],rep,12))
cldf <- melt(cldf, id.vars = c("smk", "age"))
cldf$variable <- factor(cldf$variable, levels=c('cprob1','cprob2','cprob3','cprob4','cprob5'), labels=c('Non-smoking','Early-onset smoking','Short-term smoking','Occasional smoking','Late-onset smoking'))
cldf$variable <- factor(cldf$variable, levels=c('Early-onset smoking','Late-onset smoking','Short-term smoking','Occasional smoking','Non-smoking'))
cldf$smk <- factor(cldf$smk, levels=c(0:2), labels=c('Regular smoking','Occasional smoking','Non-smoking'))

clprop <- split(cldf, cldf$variable)
clprop <- lapply(clprop, function(x){
  do.call('rbind',tapply(x$smk, x$age, function(x){prop.table(table(x, useNA='always'))}))
})
clprop <- data.frame(do.call('rbind', clprop))
clprop <- clprop*100
colnames(clprop) <- c('Regular smoking','Occasional smoking','Non-smoking','Missing')
clprop$age <- rep(c(13,14,15,16,17,18,20,21,22,23,24,28), 5)
clprop$class <- rep(c('Non-smoking','Early-onset smoking','Short-term smoking','Occasional smoking','Late-onset smoking'),each=12)
clprop <- clprop[,c('class','age','Regular smoking','Occasional smoking','Non-smoking','Missing')]
write.xlsx(clprop, sheetName="non-smoking", file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Reports/lcprops.xlsx')





#TOPGRP HITS AND FUNCTION
tophitsplt <- function(df){
  ggplot(df, aes(x = interaction(long_labs,ages), y = log_OR, ymin=log_LCI, ymax=log_UCI, pch = comp_cl)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(size=3) +
    geom_errorbar(lwd=.2) +
    facet_grid(ref_cl ~ grp, scales="free_x", space="free_x") +
    scale_y_continuous(limits=c(-3,5), breaks = seq(-3,5,2)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    scale_x_discrete(labels=function(x) sub('(\\..).*', '\\2', x)) +
    scale_shape_manual(values=c(1:4)) +
    theme_bw() +
    theme(legend.position = 'bottom', legend.spacing.x = unit(0.3, 'cm'),
          text = element_text(size=25, family="serif"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=20),
          strip.background = element_rect(colour="black", fill="white"), strip.text.x = element_blank()) +
    guides(colour=guide_legend(nrow=4, byrow=F)) +
    labs(x = '\nRisk factor', y = "log(OR)\n", pch = 'Reference :')
}

topgrp <- table(pltdf$grp[pltdf$labs %in% names(allimportant2)])[order(table(pltdf$grp[pltdf$labs %in% names(allimportant2)]), decreasing=T)]
topgrp1 <- topgrp[c('m_pregsmk','m_dailsmk','p_dailsmk','hh_smk','m_alc','m_cana','m_mhprob')]
topgrp2 <- topgrp[c('m_marst','m_homown','m_townsend','m_IMD')]
topgrp3 <- topgrp[c('townsend','IMD','urbrur')]
topgrp4 <- topgrp[c('everalc','evercana','everdrug','friends_smk','friends_alc','friends_drug')]
topgrp5 <- topgrp[c('bmi','cisr','eduasp','qual','trauma','parent')]

top_labs <- c(
  'm_pregsmk' = 'Maternal smoking in pregnancy', 
  'm_dailsmk' = 'Maternal daily smoking', 'p_dailsmk' = 'Paternal daily smoking',
  'hh_smk' = 'Household smoking','m_alc' = 'Maternal alcohol consumption',
  'm_cana' = 'Maternal cannabis use','m_mhprob' = 'Maternal mental health condition',
  'm_marst' = 'Maternal marital status','m_homown' = 'Maternal home ownership',
  'm_townsend' = 'G0 Townsend score','m_IMD' = 'G0 IMD score',
  'townsend' = 'Townsend score','IMD' = 'IMD score','urbrur' = 'Urban/Rural postcode',
  'everalc' = 'Alcohol consumption','evercana' = 'Cannabis use','everdrug' = 'Drug use',
  'friends_smk' = 'Peer smoking','friends_alc' = 'Peer alcohol consumption','friends_drug' = 'Peer drug use',
  'bmi' = 'BMI','cisr' = 'CIS-R','eduasp' = 'Educational aspirations','qual' = 'Qualifications',
  'trauma' = 'Trauma','parent' = 'Parenthood'
)
allests


sum(topgrp1) #45
topgrp1 <- names(topgrp1)
nrow(pltdf[pltdf$grp %in% topgrp1,]) #60

sum(topgrp2) #27
topgrp2 <- names(topgrp2)
nrow(pltdf[pltdf$grp %in% topgrp2,]) #52

sum(topgrp3) #18
topgrp3 <- names(topgrp3)
nrow(pltdf[pltdf$grp %in% topgrp3,]) #51

sum(topgrp4) #43
topgrp4 <- names(topgrp4)
nrow(pltdf[pltdf$grp %in% topgrp4,]) #51

sum(topgrp5) #33
topgrp5 <- names(topgrp5)
nrow(pltdf[pltdf$grp %in% topgrp5,]) #48

sum(topgrp[!names(topgrp) %in% c(topgrp1,topgrp2,topgrp3,topgrp4)]) #123



##PLOTS
p <- list(
  #LATENT CLASSES
  ggarrange(
    #SMOKING PREVALENCE BY AGE
    ggplot(ns, aes(x = age, y = value, pch = variable)) +
      geom_point(size=7) +
      geom_line() +
      scale_x_continuous(limits=c(12,30), breaks = seq(12,30,2)) +
      scale_y_continuous(limits=c(0,80), breaks = seq(0,80,20)) +
      theme_bw() +
      theme(legend.position = 'bottom', legend.spacing.x = unit(0.3, 'cm'), text = element_text(size=30, family="serif"),
            strip.background = element_rect(colour="black", fill="white"), strip.text.x = element_blank()) +
      guides(colour=guide_legend(nrow=4, byrow=F)) +
      labs(x = '\nAge', y = "Percentage\n", pch = '')
    ,
    ggplot(cldf[!is.na(cldf$smk),], aes(fill=smk, y=value, x=age)) + 
      geom_bar(position="fill", stat="identity") +
      facet_wrap(vars(variable), nrow=3) +
      scale_x_continuous(limits=c(12,30), breaks = seq(12,30,2)) +
      scale_fill_manual(values = grey(0:2/3)) +
      theme_bw() +
      theme(legend.position = 'bottom', text = element_text(size=30, family="serif"), 
            strip.background = element_rect(colour="black", fill="white"),
            plot.margin = unit(c(5.5, 5.5, 7.5, 5.5),'points')) +
      labs(x = '\nAge', y = 'Probability\n', fill = '') +
      guides(fill=guide_legend(nrow=1, byrow=TRUE))
    ,
    ncol=1, labels = c("A", "B"), font.label = list(size = 22, family="serif"), heights = c(1,3))
  ,
  #TOP HITS PLOTS
  tophitsplt(allests[allests$exclude %in% F & allests$grp %in% topgrp1,])
  ,
  tophitsplt(allests[allests$exclude %in% F & allests$grp %in% topgrp2,])
  ,
  tophitsplt(allests[allests$exclude %in% F & allests$grp %in% topgrp3,])
  ,
  tophitsplt(allests[allests$exclude %in% F & allests$grp %in% topgrp4,])
  ,
  ##SAMPLE SIZE
  #all
  ggplot(pltdf, aes(x = prop_exp, y = sum)) +
    geom_point(colour = 'black', size=6, pch=1) +
    scale_x_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
    scale_y_continuous(limits=c(0,5000), breaks = seq(0,5000,500)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'none', text = element_text(size = 25, family="serif"), strip.background = element_rect(colour="black", fill="white")) +
    labs(x = '\nProportion exposed (%)', y = 'Sample size\n') +
    geom_text_repel(data=pltdf[pltdf$sum > 4800,], aes(label = long_labs), size=7, max.overlaps = 25) +
    geom_text_repel(data=pltdf[pltdf$sum < 1000,], aes(label = long_labs), size=7, max.overlaps = 25) +
    geom_text_repel(data=pltdf[pltdf$prop_exp > 85,], aes(label = long_labs), size=7, max.overlaps = 25) +
    geom_text_repel(data=pltdf[pltdf$prop_exp < 1,], aes(label = long_labs), size=7, max.overlaps = 25)
  ,
  #facet
  ggplot(allests[allests$exclude %in% F,], aes(x = prop_exp, y = n, pch = comp_cl)) +
    geom_point(colour = 'black', size=6) +
    facet_wrap(vars(ref_cl), nrow=4) +
    scale_x_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
    scale_y_continuous(limits=c(0,4000), breaks = seq(0,4000,500)) +
    scale_shape_manual(values=c(1:4)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', text = element_text(size = 25, family="serif"), strip.background = element_rect(colour="black", fill="white")) +
    labs(x = '\nProportion exposed (%)', y = 'Sample size\n', pch = 'Comparison :') +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$ref_cl %in% 'Non-smoking' & allests$n < 1000,], aes(label = long_labs), size=7, max.overlaps = 25) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$ref_cl %in% 'Non-smoking' & allests$n > 3500,], aes(label = long_labs), size=7, max.overlaps = 25)
  ,
  #facet PAF by OR 
  ggplot(allests[allests$exclude %in% F,], aes(x = PAF, y = log_OR, pch = comp_cl, size = sum)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(colour = 'black', size=6, stroke = 2) +
    facet_wrap(vars(ref_cl), nrow=5) +
    scale_x_continuous(limits=c(-200,2000), breaks = seq(-200,2000,200)) +
    scale_y_continuous(limits=c(-3,5), breaks = seq(-3,5,2)) +
    scale_shape_manual(values=c(1:4)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', text = element_text(size = 25, family="serif"), strip.background = element_rect(colour="black", fill="white")) +
    labs(x = '\nPAF', y = "log(OR)\n", pch = 'Comparison :', ) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$PAF > 200,], aes(label = long_labs), colour = "grey20", size=7, max.overlaps = 25) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$PAF < -100,], aes(label = long_labs), colour = "grey20", size=7, max.overlaps = 25)
  ,
  #facet prop_exp by OR 
  ggplot(allests[allests$exclude %in% F,], aes(x = prop_exp, y = log_OR, pch = comp_cl, size = sum)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(colour = 'black', size=6) +
    facet_wrap(vars(ref_cl), nrow=5) +
    scale_x_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
    scale_y_continuous(limits=c(-3,5), breaks = seq(-3,5,2)) +
    scale_shape_manual(values=c(1:4)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', text = element_text(size = 25, family="serif"), strip.background = element_rect(colour="black", fill="white")) +
    labs(x = '\nProportion exposed (%)', y = "log(OR)\n", pch = 'Comparison :', ) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$prop_exp > 95,], aes(label = long_labs), size=7, max.overlaps = 25) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$prop_exp < 1,], aes(label = long_labs), size=7, max.overlaps = 25)
  ,
  #facet E-value by OR 
  ggplot(allests[allests$exclude %in% F,], aes(x = evalue, y = log_OR, pch = comp_cl, size = sum)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(colour = 'black', size=6) +
    facet_wrap(vars(ref_cl), nrow=5) +
    scale_x_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
    scale_y_continuous(limits=c(-3,5), breaks = seq(-3,5,2)) +
    scale_shape_manual(values=c(1:4)) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', text = element_text(size = 25, family="serif"), strip.background = element_rect(colour="black", fill="white")) +
    labs(x = '\nE-value', y = "log(OR)\n", pch = 'Comparison :', ) +
    geom_text_repel(data=allests[allests$exclude %in% F & allests$evalue > 10,], aes(label = long_labs), size=7, max.overlaps = 25)
)

save(p, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/various_plots.rda')