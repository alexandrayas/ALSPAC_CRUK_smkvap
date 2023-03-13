##LOAD
library(xlsx)
library(haven)
library(ggplot2)
library(ggrepel)
library(plotly)
library(reshape2)
library(labelled)



##ADD NS TO ESTS
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/pltdf.rda')
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/ests.rda')
load("C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Outputs/allests.rda")
ests <- lapply(ests, function(x){cbind(x,text = paste("Risk factor: ", x$long_labs, "\nUnexposed label: ", x$labs_unexp, "\nExposed label: ", x$labs_exp, "\nSample size: ", x$sum, "\nN Unexposed: ", x$n_unexp, "\nN Exposed: ", x$n_exp,
                                                      "\nOR: ", x$OR, "\nSE: ", x$SE, "\nSignificant:", x$sig, sep=""))})
allests$text <- do.call(rbind, ests)[,'text']
exclude <- unique(allests[allests$exclude %in% T,'labs'])
largese <- unique(allests[allests$largese %in% T,'labs'])
largeci <- unique(c(allests$labs[!allests$labs %in% c(exclude,largeci) & allests$UCI > 10],
                    allests$labs[!allests$labs %in% c(exclude,largeci) & allests$LCI < -10]))






##PLOTS
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colours <- setNames(gg_color_hue(length(levels(pltdf$sub_grp))), levels(pltdf$sub_grp))

#range(allests[!allests$labs %in% c(exclude,largeci), c('OR','UCI','LCI')])

explt_age <- function(df, tit, cutoff){
  ggplot(df, aes(x = grp, y = est, ymin=lci, ymax=uci, colour = sub_grp, size = sum)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(alpha=0.75) +
    geom_errorbar(lwd=1) +
    facet_wrap(vars(comparison), nrow=5) +
    scale_y_continuous(limits=c(-5,5), breaks = seq(-5,5,2)) +
    scale_colour_manual(values = colours, limits = force) +
    scale_size(range = c(0.1, 3), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title.y=element_text(size=15), axis.text.y=element_text(size=15), axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = '', y = "Log odds", colour = 'Category', ) +
    geom_text_repel(data=df[df$est > cutoff[1],], aes(x=grp, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20) +
    geom_text_repel(data=df[!df$est > cutoff[1] & df$uci > cutoff[1],], aes(x=grp, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20) +
    geom_text_repel(data=df[df$est < cutoff[2],], aes(x=grp, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20) +
    geom_text_repel(data=df[!df$est < cutoff[2] & df$lci < cutoff[2],], aes(x=grp, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20)
}

smk_5llca <- as.data.frame(read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/smk_5llca.dta'))
smk_5llca[,grep('smk',colnames(smk_5llca))] <- data.frame(sapply(smk_5llca[,grep('smk',colnames(smk_5llca))], to_factor))
smk_5llca[,grep('cprob',colnames(smk_5llca))] <- data.frame(sapply(smk_5llca[,grep('cprob',colnames(smk_5llca))], as.numeric))
cldf <- data.frame(smk = do.call('c', smk_5llca[,grep('smk',colnames(smk_5llca))]), age = rep(c(13:18,20:24,28),each=nrow(smk_5llca)))
cldf <- cbind(cldf, sapply(smk_5llca[,grep('cprob',colnames(smk_5llca))],rep,12))
cldf <- melt(cldf, id.vars = c("smk", "age"))
cldf$variable <- factor(cldf$variable, levels=c('cprob1','cprob2','cprob3','cprob4','cprob5'), labels=c('Early-onset smokers','Late-onset smokers','Occasional smokers','Short-term smokers','Non-smokers'))

p <- list(
  
  ggplot(cldf[!is.na(cldf$smk),], aes(fill=smk, y=value, x=age)) + 
    geom_bar(position="fill", stat="identity") +
    facet_wrap(vars(variable), nrow=3) +
    scale_x_continuous(limits=c(12,30), breaks = seq(12,30,2)) +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title=element_text(size=15), axis.text=element_text(size=15), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    labs(x = 'Age', y = 'Probability', fill = 'Self-report smoking :') +
    guides(fill=guide_legend(nrow=1, byrow=TRUE))
  ,
  explt_age(df = allests[allests$ages_sub %in% 'Gestation' & !allests$labs %in% c(exclude,largeci),], cutoff = c(1.5,-1.5)),
  explt_age(df = allests[allests$ages_sub %in% 'Early Childhood (age 0-7)' & !allests$labs %in% c(exclude,largeci),], cutoff = c(1.5,-1.5)),
  explt_age(df = allests[allests$ages_sub %in% 'Late Childhood (age 8-12)' & !allests$labs %in% c(exclude,largeci),], cutoff = c(1.5,-1.5)),
  explt_age(df = allests[allests$ages_sub %in% 'Adolescence (age 13-18)' & !allests$labs %in% c(exclude,largeci),], cutoff = c(1.5,-1.5)),
  explt_age(df = allests[allests$ages_sub %in% 'Adulthood and Longitudinal' & !allests$labs %in% c(exclude,largeci),], cutoff = c(1.5,-1.5)),
  
  ggplot(allests[allests$labs %in% largese,], aes(x = interaction(labs,sub_grp), y = est, ymin=lci, ymax=uci, colour = sub_grp, size = sum, text=text)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    geom_point(alpha=0.75) +
    geom_errorbar(lwd=.5) +
    facet_wrap(vars(comparison), nrow=5) +
    scale_y_continuous(limits=c(-25,25), breaks = seq(-25,25,5)) +
    scale_colour_manual(values = colours, limits = force) +
    scale_size(range = c(0.1, 2), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title.y=element_text(size=15), axis.text.y=element_text(size=15), axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    guides(colour=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = '', y = "Log odds", colour = 'Category') +
    geom_text_repel(data=allests[allests$labs %in% largese & (allests$est > 1.5 | allests$est < -1.5),], aes(label=labs), size=5, max.overlaps = 20)
  ,
  ggplot(pltdf, aes(x = grp, y = sum, colour = sub_grp, size = 2)) +
    geom_point(size=3, pch=16) +
    facet_wrap(vars(ages_sub), nrow=3) +
    scale_y_continuous(limits=c(0,5000), breaks = seq(0,5000,500)) +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title=element_text(size=15), axis.text.y=element_text(size=15), axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = '', y = "N", colour = 'Category') +
    geom_text_repel(data=pltdf[pltdf$sum < 2500,], aes(x=grp, y=sum, colour=sub_grp, label=labs), size=5, max.overlaps = 20) +
    geom_text_repel(data=pltdf[pltdf$sum > 4700,], aes(x=grp, y=sum, colour=sub_grp, label=labs), size=5, max.overlaps = 20)
  ,
  ggplot(allests[!allests$labs %in% c(exclude,largese),], aes(x = sum, y = est, colour = sub_grp, size = 2)) +
    geom_point(size=3, pch=16) +
    facet_wrap(vars(comparison), nrow=5) +
    scale_x_continuous(limits=c(0,4000), breaks = seq(0,4000,1000)) +
    scale_y_continuous(limits=c(-3,5), breaks = seq(-3,5,2)) +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title=element_text(size=15), axis.text=element_text(size=15), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = 'N', y = "Log odds", colour = 'Category') +
    geom_text_repel(data=allests[!allests$labs %in% c(exclude,largese) & (allests$est < -1.5 | allests$est > 1.5),], aes(x=sum, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20)
  ,
  ggplot(allests[allests$labs %in% largese,], aes(x = sum, y = est, colour = sub_grp, size = 2)) +
    geom_point(size=3, pch=16) +
    facet_wrap(vars(comparison), nrow=5) +
    scale_x_continuous(limits=c(0,4000), breaks = seq(0,4000,1000)) +
    scale_y_continuous(limits=c(-15,15), breaks = seq(-15,15,2)) +
    theme_bw() +
    theme(legend.position = 'bottom', axis.title=element_text(size=15), axis.text=element_text(size=15), strip.text = element_text(size = 20), strip.background = element_rect(colour="black", fill="white"), legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = 'N', y = "Log odds", colour = 'Category') +
    geom_text_repel(data=allests[allests$labs %in% largese & (allests$est < -1.5 | allests$est > 1.5),], aes(x=sum, y=est, colour=sub_grp, label=labs), size=5, max.overlaps = 20)
  
)

save(p, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Figures/p.rda')



##INTERACTIVE PLOTS
setwd("C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Figures/Interactive/")

explt_int <- function(df, tit){
  breaks <- c(0.1,0.3,1,3,10)
  ggplot(df, aes(x = grp, y = OR, ymin = LCI, ymax = UCI, colour = sub_grp, size = sum, text = text)) +
    geom_point(alpha=0.9) +
    geom_errorbar(lwd=.5) +
    geom_hline(yintercept = 1, color = "grey40", linetype = "dashed") + 
    scale_y_continuous(trans='log',breaks=breaks) +
    scale_size(range = c(0.1, 2), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")),size=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = '', y = "Odds ratio", title=tit, colour = 'Category')
}

p <- list()
p$C1VC5 <- explt_int(df = ests$C1VC5[!ests$C1VC5$labs %in% c(exclude,largeci),], tit = 'Early-onset Smokers vs Non-smokers')
p$C2VC5 <- explt_int(df = ests$C2VC5[!ests$C2VC5$labs %in% c(exclude,largeci),], tit = 'Late-onset Smokers vs Non-smokers')
p$C3VC5 <- explt_int(df = ests$C3VC5[!ests$C3VC5$labs %in% c(exclude,largeci),], tit = 'Occasional Smokers vs Non-smokers')
p$C4VC5 <- explt_int(df = ests$C4VC5[!ests$C4VC5$labs %in% c(exclude,largeci),], tit = 'Short-term Smokers vs Non-smokers')
p$C1VC4 <- explt_int(df = ests$C1VC4[!ests$C1VC4$labs %in% c(exclude,largeci),], tit = 'Early-onset Smokers vs Short-term Smokers')
p$C2VC4 <- explt_int(df = ests$C2VC4[!ests$C2VC4$labs %in% c(exclude,largeci),], tit = 'Late-onset Smokers vs Short-term Smokers')
p$C3VC4 <- explt_int(df = ests$C3VC4[!ests$C3VC4$labs %in% c(exclude,largeci),], tit = 'Occasional Smokers vs Short-term Smokers')
p$C1VC3 <- explt_int(df = ests$C1VC3[!ests$C1VC3$labs %in% c(exclude,largeci),], tit = 'Early-onset Smokers vs Occasional Smokers')
p$C2VC3 <- explt_int(df = ests$C2VC3[!ests$C2VC3$labs %in% c(exclude,largeci),], tit = 'Late-onset Smokers vs Occasional Smokers')
p$C1VC2 <- explt_int(df = ests$C1VC2[!ests$C1VC2$labs %in% c(exclude,largeci),], tit = 'Early-onset Smokers vs Late-onset Smokers')

p <- lapply(p, ggplotly, tooltip="text")
for(i in names(p)){
  htmlwidgets::saveWidget(p[[i]], selfcontained=T, file=paste0(i,".html"))
}
rm(i)

explt_int_age <- function(df, tit, breaks){
  ggplot(df, aes(x = grp, y = OR, ymin=LCI, ymax=UCI, colour = sub_grp, size = sum, text = text)) +
    geom_point(alpha=0.75) +
    geom_errorbar(lwd=.5) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    facet_wrap(vars(comparison), nrow=2, scales = "free_x") +
    scale_y_continuous(trans='log',breaks=breaks) +
    scale_colour_manual(values = colours, limits = force) +
    scale_size(range = c(0.1, 2), name="Sample \nsize") +
    theme_bw() +
    theme(legend.position = 'bottom', axis.ticks.x = element_blank(), axis.text.x = element_blank(), panel.spacing = unit(0.2, "lines"),
          strip.background = element_rect(colour="black", fill="white"), strip.text = element_text(size = 7)) +
    guides(colour=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = ""))) +
    labs(x = '', y = "Odds ratio", title=tit, colour = 'Category')
}
#WARNING: Transformation introduced infinite values in continuous y-axis
p <- list()
p$gest <- explt_int_age(df = allests[allests$ages_sub %in% 'Gestation' & !allests$labs %in% c(exclude,largeci),], 'Gestation', c(0.1,0.3,1,3,10))
p$earlychildhood <- explt_int_age(df = allests[allests$ages_sub %in% 'Early Childhood (age 0-7)' & !allests$labs %in% c(exclude,largeci),], 'Early Childhood (age 0-7)', c(0.1,0.3,1,3,10))
p$latechildhood <- explt_int_age(df = allests[allests$ages_sub %in% 'Late Childhood (age 8-12)' & !allests$labs %in% c(exclude,largeci),], 'Late Childhood (age 8-12)', c(0.1,0.3,1,3,10))
p$adolescence <- explt_int_age(df = allests[allests$ages_sub %in% 'Adolescence (age 13-18)' & !allests$labs %in% c(exclude,largeci),], 'Adolescence (age 13-18)', c(0.1,0.3,1,3,10))
p$adulthoodandlongit <- explt_int_age(df = allests[allests$ages_sub %in% 'Adulthood and Longitudinal' & !allests$labs %in% c(exclude,largeci),], 'Adulthood and Longitudinal', c(0.1,0.3,1,3,10))
p$largecise <- explt_int_age(df = allests[!allests$labs %in% c(exclude) & allests$labs %in% c(largese,largeci),], 'Large CI or SE', c(0.1,100,1000,10000,65000))

p <- lapply(p, ggplotly, tooltip="text")
for(i in names(p)){
  htmlwidgets::saveWidget(p[[i]], selfcontained=T, paste0(i,".html"))
}
rm(i)



##PLOTS SHOWING LARGE/SPURIOUS ASSOCIATIONS
df <- allests[allests$labs %in% c(largese),]
pdf('LOR_largeodds.pdf', width=8.5, height=13, pointsize=10)
ggplot(df, aes(x = interaction(labs,sub_grp), y = est, ymin=lci, ymax=uci, colour = sub_grp, size = sum, text=text)) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
  geom_point(alpha=0.75) +
  geom_errorbar(lwd=.5) +
  facet_wrap(vars(comparison), nrow=5) +
  scale_y_continuous(limits=c(-25,25), breaks = seq(-25,25,5)) +
  scale_colour_manual(values = colours, limits = force) +
  scale_size(range = c(0.1, 2), name="Sample \nsize") +
  theme_bw() +
  theme(legend.position = 'bottom', axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(colour="black", fill="white")) +
  guides(colour=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = ""))) +
  labs(x = '', y = "Log odds", colour = 'Category') +
  geom_text_repel(aes(label=labs), size=3, max.overlaps = 20) 
dev.off()

df <- allests[allests$labs %in% c(largeci),]
pdf('LOR_largecis.pdf', width=8.5, height=13, pointsize=10)
ggplot(df, aes(x = interaction(labs,sub_grp), y = est, ymin=lci, ymax=uci, colour = sub_grp, size = sum, text=text)) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
  geom_point(alpha=0.75) +
  geom_errorbar(lwd=.5) +
  facet_wrap(vars(comparison), nrow=5) +
  scale_y_continuous(limits=c(-3,3), breaks = seq(-3,3,1)) +
  scale_colour_manual(values = colours, limits = force) +
  scale_size(range = c(0.1, 2), name="Sample \nsize") +
  theme_bw() +
  theme(legend.position = 'bottom', axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(colour="black", fill="white")) +
  guides(colour=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = ""))) +
  labs(x = '', y = "Log odds", colour = 'Category', ) +
  geom_text_repel(aes(label=labs), size=3, max.overlaps = 20) 
dev.off()



##SAMPLE SIZE PLOTS
df <- do.call("rbind", replicate(5, pltdf[,c('grp','sub_grp','labs','ages_sub')], simplify = FALSE))
df$sum <- unlist(pltdf[,c('sum_cl1','sum_cl2','sum_cl3','sum_cl4','sum_cl5')])
df$class <- rep(c('Early-onset smoker','Late-onset smoker','Occasional smoker','Short-term smoker','Non-smoker'), each=nrow(pltdf))
df$class <- factor(df$class, levels=c('Early-onset smoker','Late-onset smoker','Occasional smoker','Short-term smoker','Non-smoker'))

png('sampesize_cat.png', width=3000, height=1500, pointsize=25)
ggplot(df, aes(x = interaction(labs,sub_grp), y = sum, colour = sub_grp)) +
  geom_point(alpha=0.75, size=8) +
  facet_wrap(vars(class), nrow=1) +
  scale_y_continuous(limits=c(0,3000), breaks = seq(0,3000,500)) +
  scale_size(range = c(0.1, 2), name="Sample \nsize") +
  theme_bw() +
  theme(legend.position = 'bottom', text = element_text(size = 30), axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(colour="black", fill="white")) +
  guides(colour=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = ""))) +
  labs(x = '', y = "Sample size", colour = 'Category') 
dev.off()

png('sampesize_age.png', width=3000, height=1500, pointsize=25)
ggplot(df, aes(x = interaction(labs,sub_grp), y = sum, colour = ages_sub)) +
  geom_point(alpha=0.75, size=8) +
  facet_wrap(vars(class), nrow=1) +
  scale_y_continuous(limits=c(0,3000), breaks = seq(0,3000,500)) +
  scale_size(range = c(0.1, 2), name="Sample \nsize") +
  theme_bw() +
  theme(legend.position = 'bottom', text = element_text(size = 30), axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(colour="black", fill="white")) +
  guides(colour=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = "")), size=guide_legend(nrow=3, byrow=TRUE, override.aes = aes(label = ""))) +
  labs(x = '', y = "Sample size", colour = 'Age') 
dev.off()
