# Load packages
library(haven)
library(labelled)
library(gtsummary)
library(mice)
library(ggmice)
library(ggplot2)

# Load selection data and define labels

#data (3290 participants answered questions on smoking at 21 yrs (YPA) and not withdrawn consent)
alldf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/vaping_transitions/vaping_all_measures.dta')
alldf <- to_factor(alldf, sort_levels='auto')
alldf$id <- paste0(alldf$aln, alldf$qlet)

#13 main exposures
selection_exp_labs <- c('sex', 'ethnicity', 'hhincome_11', 'parent_dailsmk_12', 'parent_mh_12',
                        'friends_smk_16', 'eduasp_16', 'alc_16', 'cana_16', 'drug_16', 'exerc_18',
                        'bmi_18_gr', 'mfq_18_gr')

#22 auxiliary variables (maybe add in measures collected after 21 years too?)
selection_aux_labs <- c('hhincome_8', 'hhincome_18', 
                        'parent_dailsmk_2', 'parent_currsmk_18',
                        'parent_mh_1', 'parent_mhmeds_18', 
                        'friends_smk_14', 'friends_smk_18',
                        'eduasp_14', 'qual_18', 
                        'everalc_14', 'alc_18',
                        'evercana_14', 'cana_18',
                        'everdrug_14', 'drug_18',
                        'exerc_16', 'exerc_22',
                        'bmi_16_gr', 'bmi_24_gr',
                        'mfq_14_gr', 'mfq_21_gr')

df <- data.frame(alldf[,c('smk_21',selection_exp_labs,selection_aux_labs)])
head(df)
nrow(df)

# Selection correlation matrix

sel_corr <- plot_corr(df, rotate = T) +
  theme(axis.text.x = element_text(hjust=0), 
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        text=element_text(size=10, family = 'Times New Roman'))

ggsave(sel_corr, width=8,   height=8, filename = '//path/to/files/selection_corr_mat_plt.png')

# Selection predictor matrix

## By default, mice() uses 'pmm' (predictive mean matching) for numeric data, 'logreg' (logistic regression imputation) for binary data/factors with 2 levels, 'polyreg' (polytomous regression imputation) for unordered categorical data/factors with > 2 levels, and 'polr' (proportional odds model) for ordered factors with > 2 levels. I added a Shapiro-Wilk Normality Test to use 'norm' imputation method for normally distributed numeric variables instead of 'pmm'.
##May want to use 'norm' method to impute all numeric variables.

getmethod <- function(df){
  out <- rep('', ncol(df))
  names(out) <- colnames(df)
  for(i in colnames(df)){
    if(is.numeric(df[,i]) == T){
      out[i] <- ifelse(shapiro.test(df[,i])$p.value < 0.01, 'pmm', 'norm')
    } else if(is.factor(df[,i]) == T & is.ordered(df[,i]) == F){
      out[i] <- ifelse(length(levels(df[,i])) > 2, 'polyreg', 'logreg')
    } else if(is.factor(df[,i]) == T & is.ordered(df[,i]) == T){
      out[i] <- ifelse(length(levels(df[,i])) > 2, 'polr', NA)
    }
  }
  return(out)
}
fcs_method <- getmethod(df)
fcs_method[c('smk_21','sex')] <- ''

predm <- quickpred(df)

predm[c('smk_21','sex'),] <- 0
predm[selection_exp_labs[!selection_exp_labs %in% 'sex'], c('smk_21',selection_exp_labs)] <- 1
diag(predm) <- 0

sel_pred <- plot_pred(predm, rotate = T, label = F, method = fcs_method) +
  theme(axis.text.x = element_text(hjust=0), 
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        text=element_text(size=10, family = 'Times New Roman'))

ggsave(sel_pred, width=8,   height=8, filename = '//path/to/files/selection_pred_mat_plt.png')

# Load analytic data and define exposures

# 858 participants reported smoking in past month at 21 yrs (YPA)
subdf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/vaping_transitions/vaping_sub_measures.dta')
subdf <- to_factor(subdf, sort_levels='auto')

# 5 outcome timepoints
vaps <- c('vap22','vap23','vap24','vap28','vap30')

# 4 baseline exposures
baseline_exp <- c('sex', 'ethnicity', 'hhincome_11', 'parent_dailsmk_12')

# 11 main exposures
outcome_exp_labs <- c('bmi_18_gr', 'exerc_18', 'qual_20', 'bingalc_20', 'cana_20', 'drug_20',
                      'friends_smk_20', 'mfq_21_gr', 'townsend_21_gr', 'parent_21', 'freqsmk_21')

# 24 auxiliary variables
outcome_aux_labs <- c('hhincome_8', 'hhincome_18', 
                      'parent_dailsmk_2', 'parent_currsmk_18', 
                      'bmi_16_gr', 'bmi_24_gr',
                      'exerc_16', 'exerc_22',
                      'eduasp_16', 'qual_18', 
                      'alc_16', 'bingalc_18',
                      'cana_16', 'cana_18',
                      'drug_16', 'drug_18',
                      'friends_smk_16', 'friends_smk_18', 
                      'mfq_14_gr', 'mfq_18_gr', 
                      'townsend_18_gr', 'townsend_20_gr',
                      'parent_20','parent_22')

df <- data.frame(subdf[,c(vaps,baseline_exp,outcome_exp_labs,outcome_aux_labs)])
df_w <- cbind(df, 'weights' = subdf$weights)
head(df_w)
nrow(df_w)

# Correlations between variables

ana_corr <- plot_corr(df_w, rotate = T) +
  theme(axis.text.x = element_text(hjust=0), 
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        text=element_text(size=10, family = 'Times New Roman'))

ggsave(ana_corr, width=8,   height=8, filename = '//path/to/files/analytic_corr_mat_plt.png')

# Predictor matrix

derivlabs <- c('timeto','firstevent','dual','exclecig','nonuse')
df_w[,derivlabs] <- NA

method <- getmethod(df_w)
method[c('sex','weights')] <- ''

predm <- quickpred(df_w)
predm[c(derivlabs,vaps,baseline_exp,outcome_exp_labs)[!c(derivlabs,vaps,baseline_exp,outcome_exp_labs) %in% 'sex'], c(derivlabs,vaps,baseline_exp,outcome_exp_labs)] <- 1
predm[,'weights'] <- 1
predm[c('sex','weights',derivlabs),] <- 0
predm[vaps,c(derivlabs)] <- 0
predm[,'firstevent'] <- 0
diag(predm) <- 0

ana_pred <- plot_pred(predm, rotate = T, label = F, method = method) +
  theme(axis.text.x = element_text(hjust=0), 
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        text=element_text(size=10, family = 'Times New Roman'))

ggsave(ana_pred, width=8,   height=8, filename = '//path/to/files/analytic_pred_mat_plt.png')

# Predictors of complete cases

#There are 173 complete cases for the baseline confounders and main exposures.
#There are 168 complete cases when also including the derived first reported transition from smoking (55 lost to follow up).
#There are 95 complete cases when also including all 5 nicotine use status variables (so 78 cc missing at least one nicotine use variable)

df$firstevent <- subdf$firstevent
df$timeto <- subdf$timeto
df$cc <- ifelse(apply(df[,c(baseline_exp,outcome_exp_labs)],1,anyNA)==F,1,0)
table(df$cc)
df$vaps_cc <- ifelse(apply(df[,vaps],1,anyNA)==F,1,0)

var_label(df) <- list('firstevent'='First reported transition',
                      'timeto'='Time to first transition')

#look at who is missing a nicotine use variable
nonexclsmk <- c('Dual use','Exclusive e-cigarette','Non-use')
df2 <- df[df$cc %in% 1 & df$vaps_cc %in% 0,]
df2$when <- ifelse(df2$vap22 %in% nonexclsmk, 1, NA)
df2$when <- ifelse(is.na(df2$when) & !is.na(df2$vap22) & (df2$vap23 %in% nonexclsmk), 2, df2$when)
df2$when <- ifelse(is.na(df2$when) & !is.na(df2$vap22) & !is.na(df2$vap23) & (df2$vap24 %in% nonexclsmk), 3, df2$when)
df2$when <- ifelse(is.na(df2$when) & !is.na(df2$vap22) & !is.na(df2$vap23) & !is.na(df2$vap24) & (df2$vap28 %in% nonexclsmk), 4, df2$when)

ana_pred_cc <- tbl_uvregression(
  df[,c('cc','firstevent','timeto',baseline_exp,outcome_exp_labs,outcome_aux_labs)], 
  y = cc, 
  method = glm, method.args = list(family = binomial), 
  exponentiate = TRUE) %>%
  bold_labels() %>%
  italicize_levels()

ana_pred_cc |> 
  as_gt() |> 
  gt::gtsave(filename = '//path/to/files/analytic_pred_cc_tab.docx')