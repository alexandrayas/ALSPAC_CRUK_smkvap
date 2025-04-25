# Load packages
library(haven)
library(labelled)
library(sticky)
library(ggplot2)
library(gtsummary)
library(dplyr)
library(tidyr)

# Load data and define labels

## Load data
subdf <- read_dta('//path/to/files/vaping_sub_measures.dta')
subdf <- to_factor(subdf, sort_levels='auto')
subdf <- data.frame(subdf)
subdf <- sticky_all(subdf)
subdf$id <- 1:nrow(subdf)

load('//path/to/files/vaping_impdfs.rda')
impdfs <- lapply(impdfs, function(x){
  x[,'id'] <- 1:nrow(x)
  return(x)
})

load('//path/to/files/vaping_imp.rda')
impdf <- complete(imp, action = "long")
impdf$id <- impdf[,'.id']

## Outcomes
outcomes <- c('nonuse', 'ecig', 'exclecig', 'dual')

## 5 timepoints
vaps <- c('vap22', 'vap23', 'vap24', 'vap28', 'vap30')

# Nicotine use changes per participant

df_long <- subdf %>%
  pivot_longer(cols = starts_with("vap"), names_to = "wave", values_to = "status") %>%
  arrange(id, wave) %>%
  filter(!is.na(status))  # Remove missing values

df_transitions <- df_long %>%
  group_by(id) %>%
  mutate(transition = status != lag(status)) %>%
  summarise(num_transitions = sum(transition, na.rm = TRUE), .groups = "drop")

subdf$n_trans <- df_transitions$num_transitions

# Get number of each first events and number of reported events

## number of each first event ignoring missing timepoints
table(subdf$firstevent)
table(subdf$ecig)['1']

## Mean first events across 100 imputed datasets

### non-use
round(mean(sapply(impdfs, function(x) table(x$nonuse))['1',]),0)
### exclusive e-cig use
round(mean(sapply(impdfs, function(x) table(x$exclecig))['1',]),0)
### dual use
round(mean(sapply(impdfs, function(x) table(x$dual))['1',]),0)
### any ecig use
round(mean(sapply(impdfs, function(x) table(x$ecig))['1',]),0)
### censored
round(mean(sapply(impdfs, function(x) table(x$firstevent))['Censored',]),0)

# Derive "any" outcomes independant of time
getanyoutcome <- function(df, outcome, varlab){
  df[,paste0('any',varlab)] <- NA
  df[,paste0('timetofirst',varlab)] <- NA
  df[,paste0('timetolast',varlab)] <- NA
  for(i in 1:nrow(df)){
    df[i,paste0('any',varlab)] <- ifelse((df$vap22[i] %in% outcome | 
                                            df$vap23[i] %in% outcome | 
                                            df$vap24[i] %in% outcome | 
                                            df$vap28[i] %in% outcome | 
                                            df$vap30[i] %in% outcome), 1, 0)
    
    df[i,paste0('timetofirst',varlab)] <- ifelse(df$vap22[i] %in% outcome, 1,
                                                 ifelse(df$vap23[i] %in% outcome, 2, 
                                                        ifelse(df$vap24[i] %in% outcome, 3, 
                                                               ifelse(df$vap28[i] %in% outcome, 4, 
                                                                      ifelse(df$vap30[i] %in% outcome, 5, NA)))))
    
    df[i,paste0('timetolast',varlab)] <- ifelse(df$vap30[i] %in% outcome, 5,
                                                ifelse(df$vap28[i] %in% outcome, 4, 
                                                       ifelse(df$vap24[i] %in% outcome, 3, 
                                                              ifelse(df$vap23[i] %in% outcome, 2, 
                                                                     ifelse(df$vap22[i] %in% outcome, 1, NA)))))
  }
  return(df)
}

rungetanyoutcome <- function(df){
  df <- getanyoutcome(df, 'Non-use', 'nonuse')
  df <- getanyoutcome(df, 'Exclusive e-cigarette use', 'exclecig')
  df <- getanyoutcome(df, 'Dual use', 'dual')
  df <- getanyoutcome(df, 'Exclusive smoking', 'smk')
  return(df)
}
subdf <- rungetanyoutcome(subdf)
impdfs <- lapply(impdfs, rungetanyoutcome)

# Number of participants per N transitions
anyoutcomes <- c('anynonuse', 'anyexclecig', 'anydual')
subdf$nevents <- rowSums(subdf[,anyoutcomes])
table(subdf$nevents)

impdfs <- lapply(impdfs, function(x){
  x$nevents <- rowSums(x[,anyoutcomes])
  return(x)
})
impnevents <- lapply(impdfs, function(x) table(x$nevents))
getnevents <- function(){
  out <- list('0' = list(),'1' = list(),'2' = list(),'3' = list())
  for(i in names(out)){
    for(j in 1:length(impnevents)){
      out[[i]][[j]] <- impnevents[[j]][i]
    }
    out[[i]] <- unlist(out[[i]])
    out[[i]] <- mean(out[[i]])
  }
  return(out)
}
sapply(getnevents(),round,0)
rm(getnevents)

# Set up data to look at transitions

## Get all reported events
getwhenoutcomes <- function(df){
  
  out <- list()
  
  for(i in 1:nrow(df)){
    out[[i]] <- data.frame(time = 1:5, smk = NA, dual = NA, exclecig = NA, nonuse = NA)
    
    for(k in 1:5){
      var <- vaps[k]
      out[[i]][out[[i]]$time %in% k,'smk'] <- ifelse(df[i, var] %in% 'Exclusive smoking', T, F)
      out[[i]][out[[i]]$time %in% k,'dual'] <- ifelse(df[i, var] %in% 'Dual use', T, F)
      out[[i]][out[[i]]$time %in% k,'exclecig'] <- ifelse(df[i, var] %in% 'Exclusive e-cigarette use', T, F)
      out[[i]][out[[i]]$time %in% k,'nonuse'] <- ifelse(df[i, var] %in% 'Non-use', T, F)
    }}
  
  return(out)
}
allmat <- getwhenoutcomes(subdf)
allmat_imp <- lapply(impdfs, getwhenoutcomes)

gettranstimes <- function(df){
  out <- lapply(df[-1], function(x) df$time[x %in% T])
  out <- out[lapply(out,length)>0]
  return(out)
}
allmat <- lapply(allmat, gettranstimes)
allmat_imp <- lapply(allmat_imp, function(x) lapply(x, gettranstimes))

rm(getwhenoutcomes, gettranstimes, vaps, outcomes)

# Number of all possible transitions

## Derive transitions
gettrans <- function(from,to,data){
  
  event <- c('smk','dual','exclecig','nonuse')
  
  ### Get index for other intermediate events to consider
  ignind1 <- which(!event %in% c(from,to))[1]
  ignind2 <- which(!event %in% c(from,to))[2]
  
  ### Subset data to get from and to timepoints
  if(length(data[[from]]) %in% 1 & length(data[[to]]) %in% 1){
    data <- data
    
  } else if(length(data[[from]]) %in% 1 & length(data[[to]]) > 1){
    data[[to]] <- data[[to]][data[[to]] > data[[from]]]
    data[[to]] <- data[[to]][length(data[[to]])]
    
  } else if(length(data[[from]]) > 1 & length(data[[to]]) %in% 1){
    data[[from]] <- data[[from]][data[[from]] < data[[to]]]
    data[[from]] <- data[[from]][length(data[[from]])]
    data[[from]]
  } else if(length(data[[from]]) > 1 & length(data[[to]]) > 1){
    data[[from]] <- data[[from]][1]
    data[[to]] <- data[[to]][sapply(data[[from]], function(x) x < data[[to]])]
    data[[to]] <- data[[to]][length(data[[to]])]
  }
  
  ### Get TRUE/FALSE for specified transition
  (from %in% names(data)) & (to %in% names(data)) & !(event[ignind1] %in% names(data)) & !(event[ignind2] %in% names(data)) & 
    ('TRUE' %in% sapply(data[[from]], function(x) x < data[[to]])) |
    
    (from %in% names(data)) & (to %in% names(data)) & (event[ignind1] %in% names(data)) & !(event[ignind2] %in% names(data)) & 
    ('TRUE' %in% sapply(data[[from]], function(x) x < data[[to]])) & 
    !('TRUE' %in% sapply(data[[event[ignind1]]], function(x) x < data[[to]] & x > data[[from]])) | 
    
    (from %in% names(data)) & (to %in% names(data)) & !(event[ignind1] %in% names(data)) & (event[ignind2] %in% names(data)) & 
    ('TRUE' %in% sapply(data[[from]], function(x) x < data[[to]])) & 
    !('TRUE' %in% sapply(data[[event[ignind2]]], function(x) x < data[[to]] & x > data[[from]])) | 
    
    (from %in% names(data)) & (to %in% names(data)) & (event[ignind1] %in% names(data)) & (event[ignind2] %in% names(data)) & 
    ('TRUE' %in% sapply(data[[from]], function(x) x < data[[to]])) & 
    !('TRUE' %in% sapply(data[[event[ignind1]]], function(x) x < data[[to]] & x > data[[from]])) & 
    !('TRUE' %in% sapply(data[[event[ignind2]]], function(x) x < data[[to]] & x > data[[from]])) 
  
}

rungettrans <- function(mat, df){
  
  trans <- data.frame()[1:length(mat),]
  rownames(trans) <- 1:length(mat)
  
  ## Run gettrans for each and every transition
  trans$smktodual <- sapply(mat, function(x) gettrans('smk','dual',x))
  trans$smktoexclecig <- sapply(mat, function(x) gettrans('smk','exclecig',x))
  trans$smktononuse <- sapply(mat, function(x) gettrans('smk','nonuse',x))
  trans$dualtosmk <- sapply(mat, function(x) gettrans('dual','smk',x))
  trans$dualtoexclecig <- sapply(mat, function(x) gettrans('dual','exclecig',x))
  trans$dualtononuse <- sapply(mat, function(x) gettrans('dual','nonuse',x))
  trans$exclecigtosmk <- sapply(mat, function(x) gettrans('exclecig','smk',x))
  trans$exclecigtodual <- sapply(mat, function(x) gettrans('exclecig','dual',x))
  trans$exclecigtononuse <- sapply(mat, function(x) gettrans('exclecig','nonuse',x))
  trans$nonusetosmk <- sapply(mat, function(x) gettrans('nonuse','smk',x))
  trans$nonusetodual <- sapply(mat, function(x) gettrans('nonuse','dual',x))
  trans$nonusetoexclecig <- sapply(mat, function(x) gettrans('nonuse','exclecig',x))
  
  ## Add any missing first events (where exclusive smoking was not reported before first event)
  trans$smktodual <- ifelse(df$firstevent %in% 'Dual use', T, trans$smktodual)
  trans$smktoexclecig <- ifelse(df$firstevent %in% 'Exclusive e-cigarette use', T, trans$smktoexclecig)
  trans$smktononuse <- ifelse(df$firstevent %in% 'Non-use', T, trans$smktononuse)
  
  return(trans)
}
trans <- rungettrans(allmat, subdf)
imptrans <- lapply(1:100, function(x) rungettrans(allmat_imp[[x]], impdfs[[x]]))

# Convert to matrix
gettransmat <- function(df){
  sums <- lapply(df, sum)
  mat <- matrix(ncol=4,nrow=4)
  colnames(mat) <- c('smk','dual','exclecig','nonuse')
  rownames(mat) <- c('smk','dual','exclecig','nonuse')
  for(i in rownames(mat)){
    for(j in colnames(mat)){
      if(i == j){
        mat[i,j] <- NA
      } else {
        mat[i,j] <- sums[[paste0(i,'to',j)]]
      }
    }
  }
  return(mat)
}
transmat <- gettransmat(trans)
imptransmat <- lapply(imptrans, gettransmat)
rm(rungettrans, gettrans, gettransmat)

# Get mean Ns for each transition
getimptransmean <- function(df){
  
  means <- list()
  mat <- matrix(ncol=4,nrow=4)
  colnames(mat) <- c('smk','dual','exclecig','nonuse')
  rownames(mat) <- c('smk','dual','exclecig','nonuse')
  
  for(i in rownames(mat)){
    for(j in colnames(mat)){
      means[[paste0(i,'to',j)]] <- sapply(df, function(x){x[i,j]})
      mat[i,j] <- mean(means[[paste0(i,'to',j)]])
    }
  }
  return(mat)
}
round(getimptransmean(imptransmat),0)
