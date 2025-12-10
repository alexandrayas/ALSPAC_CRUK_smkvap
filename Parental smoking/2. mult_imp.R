# Load packages

library(haven)
library(labelled)
library(mice)
library(ggmice)
library(ggplot2)
library(gtsummary)
library(patchwork)

# Load data and define exposures

data <- read_dta('//path/to/files/parent_smk_all_measures.dta')
data <- to_factor(data, sort_levels='auto')
data <- data.frame(data)
data <- remove_attributes(data, 'format.stata')

# Subset to complete cases for parental smoking (12y) 
data <- data[!is.na(data$parent_dailsmk_12),]
nrow(data) #6729

# Main variable labels
main_vars <- c('parent_dailsmk_12','subst_13','mfq_14_8cut','currsmk_16')

baseconf <- c('m_pregsmk', 'parent_age_0', 'parent_sc_0', 'parent_hiqual_0',
              'hoodqual_0', 'townsend_8', 'hhincome_8', 'm_homown_10', 
              'parent_epds_8', 'parent_mhp_8')

medoutconf <- c('sex', 'ethnicity',
                'parent_cruel_9', 'monitor_13', 'peer_subst_13', 'bullying_13')

# Auxiliary variable labels (no earlier parental monitoring variable available)
aux_vars <- c('parent_dailsmk_2', 'peer_subst_10', 'subst_10', 
              'hoodstress_3', 'm_homown_0', 'hhincome_3', 'm_townsend_0',
              'mfq_10_8cut', 'currsmk_14',
              'm_epds_0', 'p_epds_0', 'parent_mhp_1',
              'm_ethnicity', 'p_ethnicity', 'parent_cruel_1', 'bullying_8')

# Get all variables
allvars <- c(main_vars,baseconf,medoutconf)

# Add interaction terms
data$XMint <- (as.numeric(data$parent_dailsmk_12)-1) * (as.numeric(data$mfq_14_8cut)-1)
data$XYint <- (as.numeric(data$parent_dailsmk_12)-1) * (as.numeric(data$currsmk_16)-1)

# Predictor matrix

# Includes 38 variables (1 exposure, 1 intermediate confounder, 1 mediator, 1 outcome, 1 exposure-mediation [X-M] interaction term, 1 exposure-outcome [X-Y] interaction term, 10 baseline confounders, 6 mediator-outcome confounders, and 16 auxiliary variables)

# Default imputation methods
init <- mice(data[,c(main_vars,'XMint','XYint',baseconf,medoutconf,aux_vars)], maxit = 0)
def_methods <- init$method

# Manual imputation methods
getmethod <- function(df){
  out <- rep(NA, ncol(df))
  names(out) <- colnames(df)
  for(i in colnames(df)){
    if(is.numeric(df[,i]) == T & length(table(df[,i])) > 2){
      #cannot use shapiro-wilk test for sample sizes > 5000
      #out[i] <- ifelse(shapiro.test(df[,i])$p.value < 0.01, 'pmm', 'norm')
      out[i] <- 'norm'
    } else if(length(table(df[,i])) == 2){
      out[i] <- 'logreg'
    } else if(is.factor(df[,i]) == T & is.ordered(df[,i]) == F){
      out[i] <- ifelse(length(levels(df[,i])) > 2, 'polyreg', 'logreg')
    } else if(is.factor(df[,i]) == T & is.ordered(df[,i]) == T){
      out[i] <- ifelse(length(levels(df[,i])) > 2, 'polr', NA)
    }
  }
  return(out)
}
# Use norm for numeric, logreg for binary, and polyreg for other categorical variables
imp_methods <- getmethod(data[,c(main_vars,'XMint','XYint',baseconf,medoutconf,aux_vars)])

# Don't impute parental smoking, sex or binary SMFQ scores
imp_methods[c('sex','parent_dailsmk_12')] <- ''

# Check distributions/normality of numeric variables
num_vars <- imp_methods[imp_methods %in% 'norm']
gethist <- function(var, dat, binw){
  ggplot(data = dat, aes(x = .data[[var]])) +
    geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
    theme_minimal()
}
hists <- lapply(names(num_vars), gethist, data, 1)
wrap_plots(hists, ncol = 2)

# Use pmm for EPDS and neighbourhood quality/stress scores as not normally distributed
imp_methods[c('m_epds_0','p_epds_0','parent_epds_8','hoodqual_0','hoodstress_3')] <- 'pmm'

# Passive imputation for interactions
imp_methods["XMint"] <- "~I( (as.numeric(parent_dailsmk_12)-1) * (as.numeric(mfq_14_8cut)-1) )"
imp_methods["XYint"] <- "~I( (as.numeric(parent_dailsmk_12)-1) * (as.numeric(currsmk_16)-1) )"

# Compare default to manual methods
cbind(def_methods, imp_methods)

# Construct predictor matrix
predm <- quickpred(data[,c(main_vars,'XMint','XYint',baseconf,medoutconf,aux_vars)])
predm[,] <- 1
predm[c('sex','parent_dailsmk_12'),] <- 0
predm[,'XYint'] <- 0
predm['mfq_14_8cut','XYint'] <- 1
predm['mfq_14_8cut','XMint'] <- 0
predm[c('XMint','XYint'),] <- 0
diag(predm) <- 0

# Plot imputation matrix
pred_mat <- plot_pred(predm, rotate = T, label = F, method = imp_methods) +
  theme(axis.text.x = element_text(size=8, hjust=0), axis.text.y = element_text(size=8))
ggsave(plot = pred_mat, filename="//path/to/files/pred_mat.png", width = 10, height = 8, dpi = 350)

# Run imputation
imp <- mice(data = data[,c(main_vars,'XMint','XYint',baseconf,medoutconf,aux_vars)], 
            m = 50, maxit = 25, donors = 10, seed = 237657, 
            method = imp_methods, predictorMatrix = predm)

# Save mice object
save(imp, file="//path/to/files/parent_smk_imp.rda")

# Save imputed data in long format
long_imp <- complete(imp, action = "long", include = F) #excludes original dataset
colnames(long_imp)[colnames(long_imp) %in% c('.imp','.id')] <- c('imp','id')

# Change binary factors to numeric in imputed datasets
binary_vars <- colnames(long_imp)[sapply(long_imp, function(x) length(levels(x))) %in% 2]
long_imp[,binary_vars] <- sapply(long_imp[,binary_vars], function(x) as.numeric(x)-1)

# Save imputed data in long format (with numeric binary variables for use in stata)
write_dta(data=long_imp, path="//path/to/files/parent_smk_long_imp.dta")

# Get complete records dataset
data_cc <- data[complete.cases(data[,allvars]),]

# Change binary factors to numeric in complete records dataset
data_cc[,binary_vars] <- sapply(data_cc[,binary_vars], function(x) as.numeric(x)-1)

# Save complete records dataset (with numeric binary variables for use in stata)
write_dta(data=data_cc, path="//path/to/files/parent_smk_data_cc.dta")
