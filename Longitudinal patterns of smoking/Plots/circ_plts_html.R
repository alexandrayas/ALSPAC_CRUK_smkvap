##LOAD PACKAGES AND DATA
suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(grid))
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/allests.rda")
allests <- allests[allests$exclude %in% F,]
allests[allests$sig %in% F, c('log_LCI','log_UCI')] <- NA
allests$grp <- factor(allests$grp)

## 0. pre set up
label_column <- which(colnames(allests) %in% 'long_labs')
section_column <- which(colnames(allests) %in% 'grp')
estimate_column <- which(colnames(allests) %in% 'log_OR')
lower_ci <- which(colnames(allests) %in% 'log_LCI')
upper_ci <- which(colnames(allests) %in% 'log_UCI')

track_axis_reference <- 0 
track_height <- 0.15
circle_size <- 15 #smaller is larger
ci_lwd <- 2
point_cex <- 0.15
label_cex <- 0.15
y_axis_label_cex <- 0.07


setupplt <- function(dat){
  
  cols <- c("#B60000","#B66D00","#91B600","#24B600","#00B648","#00B6B6","#0048B6","#2400B6","#9100B6","#B6006D")
  dat$col <- rep(cols, times=table(dat$comparison))
  dat$fill <- dat$col
  dat$fill[dat$sig %in% F] <- "white"
  
  track_data <- dat
  track_data$grp <- droplevels(track_data$grp)
  track_data <- split(track_data, track_data$comparison)
  data <- track_data[[1]]
  
  axis_min <- list()
  axis_max <- list()
  for(i in 1:10){
    min <- round(min(track_data[[i]][[lower_ci]], track_data[[i]][[upper_ci]], track_data[[i]][[estimate_column]], na.rm=T), 2)
    axis_min[[i]] <- round(min + (min * 0.1), 1)
    max <- round(max(track_data[[i]][[lower_ci]], track_data[[i]][[upper_ci]], track_data[[i]][[estimate_column]], na.rm=T), 2)
    axis_max[[i]] <- round(max + (max * 0.01), 1)
  }
  
  ## 1. set parameter - this sets the paramater of the sections of the circos plot so that you can plot within individual sections
  npercat <- as.vector(table(data[[section_column]]))
  ## 2. organise the data
  ## 3. add column of positions of where each data point will be in th track
  ## 4. standardise data$x so that the axis is from 0-1 (individual x axis for each section)
  ## 5. add column that codes sections as numbers 
  getaxis <- function(dat){
    dat <- dat[order(dat[[section_column]], dat[[label_column]], dat[['ages']]),]
    dat$x <- with(dat, ave(seq_along(dat[[section_column]]), dat[[section_column]], FUN = seq_along))
    dat$section_numbers = factor(dat[[section_column]], labels = 1:nlevels(dat[[section_column]]))
    for (i in 1:nrow(dat)){
      dat$n[i]<-as.numeric(nrow(subset(dat, dat[[section_column]] == dat[[section_column]][i])))
      dat$ncat[i]<- dat$x[i]/dat$n[i]
    }
    return(dat)
  }
  data <- getaxis(data)
  track_data <- lapply(track_data, getaxis)
  
  ## 6. set gap for axis - spacing between sections
  gap = c(rep(1, nlevels(data[[section_column]])-1), 10)
  ## 7. clear plotting area
  circlize::circos.clear()
  ## 8. initiate blank page to plot on top of
  graphics::par(mar = c(0.035,0.2,0.01,0.3) * circle_size, cex = 3, xpd = NA) #c(bottom, left, top, right)
  ## 9. create circle parameters
  circlize::circos.par(cell.padding = c(0, 0, 0, 0),
                       start.degree = 90, gap.degree = gap, 
                       track.margin = c(0.01, 0.01), track.height = 0.10,
                       points.overflow.warning = FALSE, clock.wise = TRUE)
  ## 10. initiate circle
  circlize::circos.initialize(factors = data$section_numbers, xlim = c(0, 1), sector.width = npercat)
  ## 11. create and plot section headers
  circlize::circos.trackPlotRegion(factors = data$section_numbers,
                                   track.index = 1, track.height = 0.05,
                                   x = data$ncat, ylim = c(0, 1), 
                                   panel.fun = function(x, y){
                                     chr = circlize::get.cell.meta.data("sector.index")
                                     xlim = circlize::get.cell.meta.data("xlim")
                                     ylim = circlize::get.cell.meta.data("ylim")
                                     circlize::circos.rect(xlim[1], 0, xlim[2], 1, 
                                                           border = NA, col = "snow2")
                                     circlize::circos.text(mean(xlim), mean(ylim), 
                                                           chr, cex = 0.1, 
                                                           facing = "outside", niceFacing = TRUE, 
                                                           col = "white")
                                   }, bg.border = NA)
  ## 12. add labels
  circlize::circos.trackText(factors = data$section_numbers, track.index = 1, 
                             x = data$ncat, y = data[[estimate_column]] * 0 + 1.5, 
                             labels = data[[label_column]], 
                             facing = "reverse.clockwise", niceFacing = TRUE, 
                             adj = c(1, 1), col = "black", cex = label_cex)
  
  out <- list(data=data, track_data=track_data, axis_min=axis_min, axis_max=axis_max)
  return(out)
}

## 13. add confidence intervals
ciplt <- function(dat, n){
  axis_min <- dat$axis_min[[n]]
  axis_max <- dat$axis_max[[n]]
  dat <- dat$track_data[[n]]
  ntracks <- c(1:4,1:3,1:2,1)
  n <- ntracks[n]
  for(i in 1:nlevels(dat[[section_column]])){
    track.index <- n + 1
    data_sub = subset(dat, section_numbers == i)
    circlize::circos.trackPlotRegion(factors = data_sub$section_numbers, 
                                     track.index = track.index, track.height = track_height,
                                     x = data_sub$ncat, y = data_sub[[estimate_column]], 
                                     ylim = c(axis_min, axis_max), 
                                     bg.border = NA, bg.col = NA,
                                     panel.fun = function(x, y) {
                                       circlize::circos.lines(x = x,
                                                              y = y * 0 + track_axis_reference,
                                                              col = "darkgrey", 
                                                              lwd = 1.5,
                                                              lty = 1)
                                       circlize::circos.segments(x0 = data_sub$ncat, x1 = data_sub$ncat, 
                                                                 y0 = data_sub[[estimate_column]] * 0 - -(data_sub[[lower_ci]]), 
                                                                 y1 = data_sub[[estimate_column]] * 0 + data_sub[[upper_ci]], 
                                                                 col = data_sub$col, lwd = ci_lwd, lty = 1,
                                                                 straight = T, sector.index = i)})
  }
}

## 14. layer on top of the confidence intervals the effect estimates
pntplt <- function(dat, n){
  dat <- dat$track_data[[n]]
  ntracks <- c(1:4,1:3,1:2,1)
  n <- ntracks[n]
  circlize::circos.trackPoints(factors = dat$section_numbers, track.index = n + 1,
                               x = dat$ncat, y = dat[[estimate_column]],
                               cex = point_cex, pch = 21,
                               col = dat$col, bg = dat$fill)
}

## 15. add axis labels
axisplt <- function(dat, n){
  axis_min <- dat$axis_min[[n]]
  axis_max <- dat$axis_max[[n]]
  ntracks <- c(1:4,1:3,1:2,1)
  n <- ntracks[n]
  circlize::circos.yaxis(side = "left", labels.cex = y_axis_label_cex,
                         sector.index = 1, track.index = n + 1,
                         at = c(axis_min,  track_axis_reference,  axis_max),
                         tick = TRUE, tick.length = 0.01)
}

## 16. legend
legplt <- function(dat, ns){
  
  legend_sig <- ComplexHeatmap::Legend(at = paste0(' ', c('CI do not overlap null','CI overlap null')), 
                                       pch = c(19, 21),
                                       labels_gp = grid::gpar(fontsize = 5),
                                       nrow = 2, type = "points",
                                       border = NA, background = NA,
                                       size = grid::unit(2, "mm"), 
                                       grid_height	= grid::unit(3, "mm"),
                                       grid_width = grid::unit(3, "mm"),
                                       direction = "vertical", row_gap = unit(1, "mm"), gap = unit(2, "mm"))
  
  legend_points <- ComplexHeatmap::Legend(at = paste0(' ', names(dat$track_data[ns])), 
                                          pch = 19,
                                          labels_gp = grid::gpar(fontsize = 5),
                                          ncol = 1, type = "points",
                                          border = NA, background = NA,
                                          legend_gp = grid::gpar(col = sapply(dat$track_data[ns], function(x){unique(x$col)})), 
                                          size = grid::unit(2, "mm"), 
                                          grid_height	= grid::unit(3, "mm"),
                                          grid_width = grid::unit(3, "mm"),
                                          direction = "vertical", row_gap = unit(1, "mm"), gap = unit(2, "mm"))
  
  legend <- ComplexHeatmap::packLegend(legend_sig, legend_points, direction = "vertical", row_gap = unit(5, "mm"), gap = grid::unit(5, "mm"))
  legend_height <- legend@grob[["vp"]][["height"]]
  legend_width <- legend@grob[["vp"]][["width"]]
  
  grid::pushViewport(grid::viewport(x = grid::unit(0.9, "npc"),
                                    y = grid::unit(0.5, "npc"),
                                    width = legend_width,
                                    height = legend_height
  ))
  grid::grid.draw(legend)
  grid::upViewport()
}

## 17. overall plot function
circlplt <- function(dat, ns){
  dat <- setupplt(dat)
  for(i in ns){
    ciplt(dat, i)
    pntplt(dat, i)
    axisplt(dat, i)
  }
  legplt(dat, ns)
}
