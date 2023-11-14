##LOAD
library(ggplot2)
library(plotly)
library(ggpubr)



##ADD NS TO ESTS
load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/ests.rda')
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/allests.rda")
low_cell <- unique(allests[allests$low_cell %in% T,'labs'])
exclude <- unique(allests[allests$exclude %in% T,'labs'])

ests <- lapply(ests, function(x){cbind(x, low_cell = ifelse(x$labs %in% low_cell, T, F))})

comp_cl <- split(allests$comp_cl, allests$comparison)
ref_cl <- split(allests$ref_cl, allests$comparison)
ests <- lapply(1:10, function(x){cbind(ests[[x]], comp_cl = comp_cl[[x]], ref_cl = ref_cl[[x]])})
names(ests) <- levels(allests$comparison)

ests <- lapply(ests, function(x){cbind(x,text = paste("Risk factor: ", x$long_labs, "\nUnexposed label: ", x$labs_unexp, "\nExposed label: ", x$labs_exp, 
                                                      "\nReference: ", x$ref_cl, "\nComparison: ", x$comp_cl,
                                                      "\nSample size: ", x$n, "\nN Unexposed: ", x$n_unexp, "\nN Exposed: ", x$n_exp, 
                                                      "\nOR: ", x$OR, "\nLCI: ", x$LCI, "\nUCI: ", x$UCI, "\nPAF: ", x$PAF, "\n% exposed: ", x$prop_exp, 
                                                      "\nCI does not overlap null: ", x$sig, "\nSmall latent cell counts: ", x$low_cell, sep=""))})
allests$text <- do.call('rbind',ests)$text



##COLOURS
gg_color_hue <- function(n) {
  hues = seq(0, 360, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colours <- setNames(gg_color_hue(length(table(allests$sub_grp))), names(table(allests$sub_grp)))

pchs <- c('Early-onset smoking' = 16, 'Late-onset smoking' = 17, 'Short-term smoking' = 15, 'Occasional smoking' = 4)


##INTERACTIVE PLOTS
#BY COMPARISON
explt_int <- function(df){
  df <- df[with(df, order(df$sub_grp, df$grp, df$ages)),]
  df$x <- 1:nrow(df)
  ggplot(df, aes(x = x, y = log_OR, ymin = log_LCI, ymax = log_UCI, colour = sub_grp, pch = comp_cl, size = n, text = text)) +
    geom_point(alpha=0.75) +
    geom_errorbar(lwd=.5, width=.1) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
    scale_colour_manual(values = colours) +
    scale_shape_manual(values = pchs) +
    scale_size(range = c(0.1, 2), name="Sample \nsize") +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(-3, 5, 0.5), expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = 'none', panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    labs(x = '', y = "log(Odds ratio)")
}

p <- list()
p$C2VC1 <- explt_int(df = ests$`Early-onset smoking vs Non-smoking`[!ests$`Early-onset smoking vs Non-smoking`$labs %in% exclude,])
p$C5VC1 <- explt_int(df = ests$`Late-onset smoking vs Non-smoking`[!ests$`Late-onset smoking vs Non-smoking`$labs %in% exclude,])
p$C3VC1 <- explt_int(df = ests$`Short-term smoking vs Non-smoking`[!ests$`Short-term smoking vs Non-smoking`$labs %in% exclude,])
p$C4VC1 <- explt_int(df = ests$`Occasional smoking vs Non-smoking`[!ests$`Occasional smoking vs Non-smoking`$labs %in% exclude,])
p$C2VC4 <- explt_int(df = ests$`Early-onset smoking vs Occasional smoking`[!ests$`Early-onset smoking vs Occasional smoking`$labs %in% exclude,])
p$C5VC4 <- explt_int(df = ests$`Late-onset smoking vs Occasional smoking`[!ests$`Late-onset smoking vs Occasional smoking`$labs %in% exclude,])
p$C3VC4 <- explt_int(df = ests$`Short-term smoking vs Occasional smoking`[!ests$`Short-term smoking vs Occasional smoking`$labs %in% exclude,])
p$C2VC3 <- explt_int(df = ests$`Early-onset smoking vs Short-term smoking`[!ests$`Early-onset smoking vs Short-term smoking`$labs %in% exclude,])
p$C5VC3 <- explt_int(df = ests$`Late-onset smoking vs Short-term smoking`[!ests$`Late-onset smoking vs Short-term smoking`$labs %in% exclude,])
p$C2VC5 <- explt_int(df = ests$`Early-onset smoking vs Late-onset smoking`[!ests$`Early-onset smoking vs Late-onset smoking`$labs %in% exclude,])



#BY EXPOSURE GRP
explt_int_grp <- function(df){
  df <- df[with(df, order(df$sub_grp, df$grp, df$ages)),]
  df <- split(df, df$comparison)
  df <- lapply(df, function(x) cbind(x, 'x' = 1:nrow(x)))
  df <- do.call('rbind',df)
  ggplot(df, aes(x = x, y = log_OR, ymin = log_LCI, ymax = log_UCI, colour = sub_grp, pch = comp_cl, size = n, text = text)) +
    geom_point(alpha=0.75) +
    geom_errorbar(lwd=.2, width=.1) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") + 
    facet_wrap(vars(ref_cl), nrow=4) +
    scale_colour_manual(values = colours) +
    scale_shape_manual(values = pchs) +
    scale_size(range = c(0.1, 2), name="Sample \nsize") +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(-3, 5, 0.5), expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = 'none', panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), panel.spacing = unit(0.2, "lines"),
          strip.background = element_rect(colour="black", fill="white"), strip.text = element_text(size = 10)) +
    labs(x = '', y = "log(Odds ratio)")
}

p$fam_peer_smk <- explt_int_grp(df = allests[allests$lab_grp %in% 'Family and peer smoking' & !allests$labs %in% exclude,])
p$fam_peer_sub <- explt_int_grp(df = allests[allests$lab_grp %in% 'Family and peer substance use' & !allests$labs %in% exclude,])
p$fam_socdem <- explt_int_grp(df = allests[allests$lab_grp %in% 'Family sociodemographic factors' & !allests$labs %in% exclude,])
p$lifestyle <- explt_int_grp(df = allests[allests$lab_grp %in% 'Individual lifestyle factors' & !allests$labs %in% exclude,])
p$socdem <- explt_int_grp(df = allests[allests$lab_grp %in% 'Individual sociodemographic factors' & !allests$labs %in% exclude,])
p$mh_oth <- explt_int_grp(df = allests[allests$lab_grp %in% 'Mental health and other factors' & !allests$labs %in% exclude,])



#LEGEND
getleg <- function(df){
  df <- df[with(df, order(df$sub_grp, df$grp, df$ages)),]
  df$x <- 1:nrow(df)
  out <- get_legend(
    ggplot(df, aes(x = x, y = log_OR, ymin = log_LCI, ymax = log_UCI, colour = sub_grp, pch = comp_cl, size = n, text = text)) +
      geom_point(alpha=0.75) +
      geom_errorbar(lwd=.2, width=.1) +
      facet_wrap(vars(ref_cl), nrow=4) +
      scale_colour_manual(values = colours) +
      scale_shape_manual(values = pchs) +
      scale_size(range = c(0.1, 2), name="Sample size : ") +
      theme_bw() +
      theme(plot.margin = margin(0,-10,0,-10, unit="cm"),
            legend.position = 'right', 
            legend.key.size = unit(0.05, 'cm'),
            legend.text = element_text(size=3),
            legend.title = element_text(size=4),
            legend.margin = margin(0,-10,0,-10, unit="cm"),
            legend.spacing.x = unit(0.2, 'cm')) +
      guides(colour=guide_legend(nrow=6, byrow=TRUE, override.aes = aes(label = "")), 
             size=guide_legend(nrow=1, byrow=TRUE, override.aes = aes(label = "")),
             pch=guide_legend(nrow=1, byrow=TRUE, override.aes = aes(label = ""))) +
      labs(colour = 'Category : ', pch = 'Comparison : ')
  )
  return(out)
}
leg <- getleg(allests[!allests$labs %in% exclude,])

#PLOTLY AND SAVE
p <- lapply(p, ggplotly, tooltip="text")
p$leg <- as_ggplot(leg)
save(p, file='C:/Users/qg21962/OneDrive - University of Bristol/Documents/CRUK smoking vaping/Longitudinal smoking/Figures/int_plts_all.rda')
