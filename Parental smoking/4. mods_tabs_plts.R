# Load packages
library(haven)
library(labelled)
library(mice)
#devtools::install_github("ellessenne/mice.mcerror")
library(mice.mcerror)
library(sticky)
library(gtsummary)
library(tidyverse)
library(biostat3) #for lincom()
library(data.table) #for manual pooling
library(gt)
library(extrafont)
font_import()
loadfonts(device = "win")
loadfonts(device = "postscript")
library(naniar)
library(ggmice)

# G-computation effects description
g_med_effs_df <- data.frame(
  `Effect` = c(
    "Total causal effect (TCE)",
    "Natural direct effect (NDE)",
    "Natural indirect effect (NIE)",
    "Proportion mediated (PM)",
    "Controlled direct effect (CDE)"
  ),
  `Description` = c(
    "The value the outcome would take if everybody had been exposed to parental smoking (X=1) versus everyone having no parents who smoke (X=0) where offspring depression varies naturally.",
    "The direct (unmediated) effect of parental smoking on offspring smoking when offspring depressive symptoms takes the value it would take in the absence of parental smoking. In other words, the difference between potential outcomes if all had a parent who smokes but offspring depression is fixed at level under no parental smoking vs. if everyone had no parents who smoke.",
    "Difference between TCE and NDE, capturing the effect of parental on offspring smoking that operates by changing offspring depressive symptoms.",
    "Proportion of the total effect of parental smoking on offspring smoking explained by offspring depression (NIE / TCE).",
    "Difference between potential outcomes for parental smoking vs no parental smoking, if offspring depressive symptoms is fixed to low (m=0) for everyone in this case."
  ),
  `Formula` = c(
    "log{E[Y{X=1,M(X=1)}] / (1-E[Y{X=1,M(X=1)}])} - log{E[Y{X=0,M(X=0)}] / (1-E[Y{X=0,M(X=0)}])}",
    "log{E[Y{X=1,M(X=0)}] / (1-E[Y{X=1,M(X=0)}])} - log{E[Y{X=0,M(X=0)}] / (1-E[Y{X=0,M(X=0)}])}",
    "TCE - NDE",
    "NIE / TCE",
    "log(E{Y(X=1,M=m)} / [1-E{Y(X=1,M=m)}]) - log(E{Y(X=0,M=m)} / [1-E{Y(X=0,M=m)}])"
    )
)

g_med_effs_tab <- g_med_effs_df %>%
  gt() %>%
  cols_label(
    `Effect` = "Effect Type",
    `Description` = "Explanation",
    `Formula` = "Formula"
  ) %>%
  cols_width(everything() ~ px(400)) %>%
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_column_labels())

# Get all data
alldf <- read_dta('//path/to/files/parent_smk_all_measures.dta')
alldf <- to_factor(alldf, sort_levels='auto')
alldf <- data.frame(alldf)

# Subset to complete records for parental smoking (12y) 
df <- alldf[!is.na(alldf$parent_dailsmk_12),]

# Load imputed data
load('//path/to/files/parent_smk_imp.rda')
imp_list <- lapply(1:imp$m, function(x) complete(imp,x))

# Main variable labels
exposure <- 'parent_dailsmk_12'
intconf <- 'subst_13'
mediator <- 'mfq_14_8cut'
outcome <- 'currsmk_16'

# Baseline confounders
baseconf <- c('m_pregsmk', 'parent_age_0', 'parent_sc_0', 'parent_hiqual_0',
              'hoodqual_0', 'townsend_8', 'hhincome_8', 'm_homown_10', 
              'parent_epds_8', 'parent_mhp_8')

# Mediator-outcome confounders
medoutconf <- c('sex', 'ethnicity', 
                'parent_cruel_9', 'peer_subst_13', 'monitor_13', 'bullying_13')

# Get all variables used
allvars <- c(exposure,intconf,mediator,outcome,baseconf,medoutconf)
confvars <- c(baseconf, medoutconf)

# Auxiliary variables
aux_vars <- c('parent_dailsmk_2', 'peer_subst_10', 'subst_10', 
              'hoodstress_3', 'm_homown_0', 'hhincome_3', 'm_townsend_0',
              'mfq_10', 'currsmk_14',
              'm_epds_0', 'p_epds_0', 'parent_mhp_1',
              'parent_ethnicity', 'parent_cruel_1', 'bullying_8')


# Add interaction terms
df$XMint <- (as.numeric(df$parent_dailsmk_12)-1) * (as.numeric(df$mfq_14_8cut)-1)
df$XYint <- (as.numeric(df$parent_dailsmk_12)-1) * (as.numeric(df$currsmk_16)-1)


# Labels
varlabs <- list('parent_dailsmk_12' = 'Parental daily smoking (12y)',
                'subst_13' = 'Substance use in past 6 months (13y)',
                'mfq_14_8cut' = 'SMFQ scores, >=8 (14y)',
                'currsmk_14' = 'Current smoking (14y)',
                'currsmk_16' = 'Current smoking (16y)',
                'm_pregsmk' = 'Mother smoked in pregnancy (0y)', 
                'parent_age_0' = 'Oldest parental age at birth', 
                'parent_sc_0' = 'Lowest parental social class (0y)', 
                'parent_hiqual_0' = 'Lowest parental education (0y)', 
                'hoodqual_0' = 'Neighbourhood quality index (0y)', 
                'hoodstress_3' = 'Neighbourhood stress score (3y)', 
                'hhincome_8' = 'Household income per week (8y)', 
                'townsend_8' = 'Townsend deprivation score quintiles (8y)', 
                'm_homown_10' = 'Housing tenure (10y)', 
                'm_epds_0' = 'Maternal EPDS score (0y)',
                'p_epds_0' = 'Paternal EPDS score (0y)',
                'parent_epds_8' = 'Highest parental EPDS score (8y)', 
                'parent_mhp_8' = 'Parental mental health condition (8y)',  
                'sex' = 'Sex assigned at birth', 
                'ethnicity' = 'Ethnicity', 
                'm_ethnicity' = 'Maternal ethnicity',
                'p_ethnicity' = 'Paternal ethnicity',
                'parent_cruel_9' = 'Physical/emotional abuse (9y)',
                'peer_subst_13' = 'Peer substance use (13y)', 
                'monitor_13' = 'Parental monitoring (13y)',
                'bullying_13' = 'Experienced any bullying (13y)',
                'parent_dailsmk_2' = 'Parental daily smoking (2y)', 
                'subst_10' = 'Substance use in past 6 months (10y)', 
                'peer_subst_10' = 'Peer substance use (10y)', 
                'mfq_10_8cut' = 'SMFQ scores, >=8 (10y)', 
                'parent_mhp_1' = 'Parental mental health condition (1y)',
                'm_homown_0' = 'Housing tenure, rented/HA/Other (0y)', 
                'hhincome_3' = 'Household income per week, <200 (3y)', 
                'm_townsend_0' = 'Townsend deprivation score, quintiles (0y)',
                'parent_cruel_1' = 'Physical/emotional abuse (1y)',
                'bullying_8' = 'Experienced any bullying (8y)'
)
var_label(alldf) <- varlabs
var_label(df) <- varlabs
df <- sticky_all(df)
var_label(imp$data) <- varlabs
df_cc <- df[complete.cases(df[,allvars]),]

# Function to make descriptive stats tables
get_desc_tabs <- function(df){
  
  list(
    #overall
    tbl_summary(df[, allvars], statistic = all_continuous() ~ "{mean} ({sd})", 
                missing = 'no', type = all_dichotomous() ~ "categorical") %>%
      bold_labels() %>%
      italicize_levels() %>%
      add_n(statistic = "{N_miss} ({p_miss}%)") %>%
      modify_header(n = "**Missing**"), #, stat_0 = ""
    #by parent_dailsmk_12
    tbl_summary(df[, allvars], statistic = all_continuous() ~ "{mean} ({sd})", 
                missing = 'no', type = all_dichotomous() ~ "categorical", 
                by = parent_dailsmk_12) %>%
      add_p() %>%
      bold_labels() %>%
      italicize_levels(),
    #by mfq_14_8cut
    tbl_summary(df[, allvars], statistic = all_continuous() ~ "{mean} ({sd})", 
                missing = 'no', type = all_dichotomous() ~ "categorical", 
                by = mfq_14_8cut) %>%
      add_p() %>%
      bold_labels() %>%
      italicize_levels()
  )
}

# Get descriptive stats tables
desc_tabs <- list(
  'alldf' = get_desc_tabs(alldf),
  'df' = get_desc_tabs(df),
  'df_cc' = get_desc_tabs(df_cc)
)

# Correlation matrix in analytic sample
corr_ana_samp <- plot_corr(alldf[,allvars], rotate = T, diagonal = F, label = T) +
  theme(text = element_text(size=10), 
        axis.text.x=element_text(size=10, hjust=0, vjust=0.3), 
        axis.text.y = element_text(size=10, vjust=0.3))
corr_ana_samp$layers[[2]]$aes_params$size <- 1.9
ggsave(plot = corr_ana_samp, filename="//path/to/files/corr_ana_samp.png", width = 8, height = 8, dpi = 350)

# Correlation matrix in ALSPAC sample
corr_full_samp <- plot_corr(df[,allvars], rotate = T, diagonal = F, label = T) +
  theme(text = element_text(size=10), 
        axis.text.x=element_text(size=10, hjust=0, vjust=0.3), 
        axis.text.y = element_text(size=10, vjust=0.3))
corr_full_samp$layers[[2]]$aes_params$size <- 1.9
ggsave(plot = corr_full_samp, filename="//path/to/files/corr_full_samp.png", width = 8, height = 8, dpi = 350)

#correlation matrix in complete records
corr_cc_samp <- plot_corr(df_cc[,allvars], rotate = T, diagonal = F, label = T) +
  theme(text = element_text(size=10), 
        axis.text.x=element_text(size=10, hjust=0), 
        axis.text.y = element_text(size=10))
corr_cc_samp$layers[[2]]$aes_params$size <- 1.5
ggsave(plot = corr_cc_samp, filename="//path/to/files/corr_cc_samp.png", width = 8, height = 8, dpi = 350)

# Functions to fit models
fit_intconf_med_models <- function(confs){
  list(
    'cc' = list(
      'parent_dailsmk_12_on_subst_13' = glm(formula=formula(paste0(intconf,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                            family=binomial(link = "logit"), data=df_cc),
      'parent_dailsmk_12_on_mfq_14_8cut' = glm(formula=formula(paste0(mediator,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                               family=binomial(link = "logit"), data=df_cc),
      'subst_13_on_mfq_14_8cut' = glm(formula=formula(paste0(mediator,'~',paste0(c(intconf,exposure,confs), collapse = '+'))), 
                                      family=binomial(link = "logit"), data=df_cc)
      ),
    'imp' = list(
      'parent_dailsmk_12_on_subst_13' = with(imp, glm(formula=formula(paste0(intconf,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                                      family=binomial(link = "logit"))),
      'parent_dailsmk_12_on_mfq_14_8cut' = with(imp, glm(formula=formula(paste0(mediator,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                                         family=binomial(link = "logit"))),
      'subst_13_on_mfq_14_8cut' = with(imp, glm(formula=formula(paste0(mediator,'~',paste0(c(intconf,exposure,confs), collapse = '+'))), 
                                                family=binomial(link = "logit")))
      )
  )
}

fit_outc_models <- function(confs){
  list(
    'cc' = list(
      'parent_dailsmk_12_on_currsmk_16' = glm(formula=formula(paste0(outcome,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                              family=binomial(link = "logit"), data=df_cc),
      'subst_13_on_currsmk_16' = glm(formula=formula(paste0(outcome,'~',paste0(c(intconf,exposure,confs), collapse = '+'))), 
                                     family=binomial(link = "logit"), data=df_cc),
      'mfq_14_8cut_on_currsmk_16' = glm(formula=formula(paste0(outcome,'~',paste0(c(mediator,exposure,confs), collapse = '+'))), 
                                        family=binomial(link = "logit"), data=df_cc),
      'mfq_14_8cut_on_currsmk_16_w_subst_13' = glm(formula=formula(paste0(outcome,'~',paste0(c(mediator,intconf,exposure,confs), collapse = '+'))), 
                                        family=binomial(link = "logit"), data=df_cc)
      ),
    'imp' = list(
      'parent_dailsmk_12_on_currsmk_16' = with(imp, glm(formula=formula(paste0(outcome,'~',paste0(c(exposure,confs), collapse = '+'))), 
                                                        family=binomial(link = "logit"))),
      'subst_13_on_currsmk_16' = with(imp, glm(formula=formula(paste0(outcome,'~',paste0(c(intconf,exposure,confs), collapse = '+'))), 
                                               family=binomial(link = "logit"))),
      'mfq_14_8cut_on_currsmk_16' = with(imp, glm(formula=formula(paste0(outcome,'~',paste0(c(mediator,exposure,confs), collapse = '+'))), 
                                                  family=binomial(link = "logit"))),
      'mfq_14_8cut_on_currsmk_16_w_subst_13' = with(imp, glm(formula=formula(paste0(outcome,'~',paste0(c(mediator,intconf,exposure,confs), collapse = '+'))), 
                                                  family=binomial(link = "logit")))
      )
  )
}

fit_interaction_models <- function(confs){
  list(
    'cc' = glm(formula=formula(paste0(outcome,'~',paste0(c(paste0(exposure,'*',mediator),confs), collapse = '+'))), 
               family=binomial(link = "logit"), data=df_cc),
    'imp' = with(imp, glm(formula=formula(paste0(outcome,'~',paste0(c(paste0(exposure,'*',mediator),confs), collapse = '+'))), 
                          family=binomial(link = "logit"))),
    'imp_list' = lapply(imp_list, function(x) glm(formula=formula(paste0(outcome,'~',paste0(c(paste0(exposure,'*',mediator),confs), collapse = '+'))), 
                                                  family=binomial(link = "logit"), data=x))
  )
}

# Fit models
unadj_intconf_med_mods <- fit_intconf_med_models(NULL)
adj_intconf_med_mods <- fit_intconf_med_models(confvars)
unadj_outc_mods <- fit_outc_models(NULL)
adj_outc_mods <- fit_outc_models(c(confvars))
unadj_inter_mods <- fit_interaction_models(NULL)
adj_inter_mods <- fit_interaction_models(confvars)

# Function to get MC errors (using rules from White, Royston, Wood 2010 Multiple imputation using chained equations: Issues and guidance for practice)
check_mc_errors <- function(fit, terms = NULL, p_bands = TRUE) {
  
  po <- pool(fit) |> summary() # pooled estimates
  mc <- mcerror(fit) |> summary() # MC errors
  mc <- mc$pooled
  
  df <- merge(po, mc, by = "term", suffixes = c("", "_mc"))
  
  # Rule 1: MC error of beta_hat ≲ 10% of its SE
  df$rule1_ratio <- df$estimate_mc / df$std.error
  df$rule1_ok    <- df$rule1_ratio < 0.10
  
  # Rule 2: MC error of test statistic ≲ 0.10
  df$rule2_ok    <- df$statistic_mc < 0.10
  
  # Rule 3: MC error of p-value ≲ 0.01 when p≈0.05, ≲ 0.02 when p≈0.10
  df$p_band <- if (p_bands) ifelse(between(df$p.value, 0.04, 0.06), 0.01,
                                   ifelse(between(df$p.value, 0.09, 0.11), 0.02, NA)) else NA
  df$rule3_ok <- ifelse(is.na(df$p_band), NA, df$p.value_mc <= df$p_band)
  
  # Keep only requested terms (if provided)
  if (!is.null(terms)) df <- df[df$term %in% terms, , drop = FALSE]
  
  out <- df[, c("term",
                "estimate","std.error","p.value",
                "estimate_mc","statistic_mc","p.value_mc",
                "rule1_ratio","rule1_ok","rule2_ok","p_band","rule3_ok")]
  rownames(out) <- NULL
  out
}

# Get MC errors
#!blocked as already checked and takes a while to run
#unadj_intconf_mc_errors <- lapply(unadj_intconf_med_mods$imp, function(x) check_mc_errors(x))
#unadj_outc_mc_errors <- lapply(unadj_outc_mods$imp, function(x) check_mc_errors(x))
#adj_intconf_mc_errors <- lapply(adj_intconf_med_mods$imp, function(x) check_mc_errors(x))
#adj_outc_mc_errors <- lapply(adj_outc_mods$imp, function(x) check_mc_errors(x))
#!

# Function to make basic tables
maketabs <- function(x, expit){
  tbl_regression(x, exponentiate = expit, quiet = T, pvalue_fun = label_style_pvalue(digits = 3),
                 include = any_of(c(exposure,intconf,mediator,'parent_dailsmk_12:mfq_14_8cut'))) %>% 
    bold_labels() %>% italicize_levels()
}

# Get OR tables
unadj_intconf_med_tabs <- lapply(unadj_intconf_med_mods[c('cc','imp')], function(x) lapply(x, maketabs, expit=T))
adj_intconf_med_tabs <- lapply(adj_intconf_med_mods[c('cc','imp')], function(x) lapply(x, maketabs, expit=T)) 
#getting adj_intconf_med_tabs gives warnings: glm.fit: fitted probabilities numerically 0 or 1 occurred
#due to table(df_cc$subst_13, df_cc$m_homown_10) where none of the 60 participants in complete records sample whose mother did not own home had used substances by age 13
unadj_outc_tabs <- lapply(unadj_outc_mods[c('cc','imp')], function(x) lapply(x, maketabs, expit=T))
adj_outc_tabs <- lapply(adj_outc_mods[c('cc','imp')], function(x) lapply(x, maketabs, expit=T))
unadj_inter_tabs <- lapply(unadj_inter_mods[c('cc','imp')], maketabs, expit=T)
adj_inter_tabs <- lapply(adj_inter_mods[c('cc','imp')], maketabs, expit=T)

# Pull estimates from tables
get_regress_ests <- function(mod_list, exp_labs, outc_labs) {
  out <- lapply(mod_list, function(l){lapply(l, function(x){data.frame(x$table_body)})})
  out <- lapply(out, function(x){
    for(i in 1:length(exp_labs)){
      x[[i]]$exposure <- exp_labs[i]
      x[[i]]$outcome <- outc_labs[i]
      x[[i]] <- x[[i]][!is.na(x[[i]]$estimate),]
      }
    x <- do.call('rbind',x)
    return(x)
    })
  out <- Map(function(df, name){
    df$data <- name
    df
  }, out, names(out))
  out <- lapply(out, function(x) x[,c('var_label','exposure','outcome','estimate','conf.low','conf.high','n_obs','data')])
  out <- do.call('rbind',out)
  out
}

unadj_intconf_med_ests <- get_regress_ests(unadj_intconf_med_tabs, 
                                           c('parent_dailsmk_12','parent_dailsmk_12','subst_13'),
                                           c('subst_13','mfq_14_8cut','mfq_14_8cut'))

unadj_outc_ests <- get_regress_ests(unadj_outc_tabs, 
                                    c('parent_dailsmk_12','subst_13','mfq_14_8cut','mfq_14_8cut_w_subst_13'),
                                    rep('currsmk_16',4))

adj_intconf_med_ests <- get_regress_ests(adj_intconf_med_tabs, 
                                         c('parent_dailsmk_12','parent_dailsmk_12','subst_13'),
                                         c('subst_13','mfq_14_8cut','mfq_14_8cut'))

adj_outc_ests <- get_regress_ests(adj_outc_tabs, 
                                  c('parent_dailsmk_12','subst_13','mfq_14_8cut','mfq_14_8cut_w_subst_13'),
                                  rep('currsmk_16',4))

unadj_ests <- rbind(unadj_intconf_med_ests, unadj_outc_ests)
adj_ests <- rbind(adj_intconf_med_ests, adj_outc_ests)
unadj_ests$adj <- F
adj_ests$adj <- T
regress_ests <- rbind(unadj_ests, adj_ests)
regress_ests$adj <- factor(regress_ests$adj, levels=c(F,T))

regress_ests$var_label <- factor(regress_ests$var_label, 
                                 levels = c('Parental daily smoking (12y)','Substance use in past 6 months (13y)','SMFQ scores, >=8 (14y)'))

regress_ests$exposure <- factor(regress_ests$exposure, 
                                levels = c('parent_dailsmk_12','subst_13','mfq_14_8cut','mfq_14_8cut_w_subst_13'),
                                labels = c('Parental daily smoking at age 12','Substance use in past 6 months at age 13','SMFQ scores >=8 at age 14 (not adjusted for early substance use)','SMFQ scores >=8 at age 14 (adjusted for early substance use)'))

regress_ests$outcome <- factor(regress_ests$outcome, 
                               levels = c('subst_13','mfq_14_8cut','currsmk_16'),
                               labels = c('Substance use in past 6 months (13y)','SMFQ scores, >=8 (14y)','Current smoking (16y)'))


# Plot regression estimates
regress_pltit <- function(df){
  
  df <- df %>%
    group_by(var_label, outcome) %>%
    mutate(
      outcome_num = as.numeric(outcome),
      dodge_group = interaction(exposure, adj, drop = TRUE, lex.order = TRUE),
      dodge_group = factor(dodge_group,
                           levels = as.vector(outer(unique(exposure), c(FALSE, TRUE),
                                                    FUN = function(e,a) paste(e,a,sep=".")))
      ),
      n_group = n_distinct(dodge_group),
      position_index = as.numeric(factor(dodge_group, levels = unique(dodge_group))),
      dodge_x = outcome_num + (position_index - (n_group + 1) / 2) * 0.6 / n_group
    ) %>%
    ungroup()
  
  ggplot(df, aes(x = dodge_x, y = log(estimate), 
                 ymin = log(conf.low), ymax = log(conf.high),
                 colour = exposure, shape = adj)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(lwd = 1, width = .05) +
    geom_point(size = 2.5) +
    facet_grid(rows = vars(var_label), switch = "y") +
    scale_x_continuous(breaks = sort(unique(df$outcome_num)), labels = levels(df$outcome)) +
    scale_y_continuous(name = "log(OR)\n", position = "right", sec.axis = dup_axis(name = "Term\n", labels = NULL)) + #breaks = seq(-0.15,1.65,0.6), 
    scale_color_manual(values = c("#999999", "#D55E00", "#Bf6DD1", "#4B0092")) +
    theme_bw() +
    theme(
      text = element_text(size = 14, family = "Calibri"),
      axis.text.x = element_text(angle = 15, hjust = 1),
      axis.ticks.y.left = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.location = "plot",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.spacing.y = unit(0.05, "cm")
    ) +
    labs(x = "\nOutcome", colour = "Primary exposure in model : ", shape = "Baseline adjustment : ") +
    guides(colour = guide_legend(order = 1, nrow = 2), shape = guide_legend(order = 2))
}

regress_plt <- regress_pltit(regress_ests[regress_ests$data %in% 'imp',])
ggsave(plot = regress_plt, filename="//path/to/files/regress_plt.png", width = 9, height = 11, dpi = 350)

# Function to stratify effects
get_strat_effs <- function(mod, expit){
  difference1 <- 'mfq_14_8cut>=8'
  difference2 <- paste0('mfq_14_8cut>=8','+', 'parent_dailsmk_12Yes',':','mfq_14_8cut>=8')
  effx0 <- lincom(mod, difference1, eform=expit)
  effx1 <- lincom(mod, difference2, eform=expit)
  out <- rbind(effx0,effx1)
  out <- cbind('M-Y association when'=c('X=0','X=1'),out)
  rownames(out) <- NULL
  out
}

# Functions to pool effects
pool_imp_strat_effs <- function(df, nm){
  m <- nrow(df)
  pooled_mean <- mean(df$Estimate)
  pooled_mean_exp <- exp(pooled_mean)
  within_var <- mean(df$se^2)
  between_var <- var(df$Estimate)
  total_var <- within_var + (1 + 1/m) * between_var
  pooled_se <- sqrt(total_var)
  ci_lower <- exp(pooled_mean - (1.96 * pooled_se))
  ci_upper <- exp(pooled_mean + (1.96 * pooled_se))
  p_val <- 2 * (1 - pnorm(abs(pooled_mean / pooled_se)))
  data.frame(
    'M-Y association when' = nm,
    'Estimate' = pooled_mean_exp,
    '2.5 %' = ci_lower,
    '97.5 %' = ci_upper,
    'SE' = pooled_se,
    'Pr(>Chisq)' = p_val,
    check.names = FALSE
  )
}

tidy_imp_strat_effs <- function(mod_list){
  out <- lapply(mod_list, get_strat_effs, expit=F)
  out <- do.call('rbind',out)
  out$se <- (out$`97.5 %`-out$Estimate)/1.96
  out <- split(out, out$`M-Y association when`)
  out
}

# Get mediator (mfq_14_8cut) effects stratified by exposure (parent_dailsmk_12)
unadj_imp_strat_effs <- tidy_imp_strat_effs(unadj_inter_mods$imp_list)
adj_imp_strat_effs <- tidy_imp_strat_effs(adj_inter_mods$imp_list)

strat_effs <- list(
  'unadj_cc' = get_strat_effs(unadj_inter_mods$cc, expit=T),
  'unadj_imp' = do.call('rbind',Map(pool_imp_strat_effs, unadj_imp_strat_effs, names(unadj_imp_strat_effs))),
  'adj_cc' = get_strat_effs(adj_inter_mods$cc, expit=T),
  'adj_imp' = do.call('rbind',Map(pool_imp_strat_effs, adj_imp_strat_effs, names(adj_imp_strat_effs)))
)

get_strat_effs_tab <- function(strat_eff_list){
  out_list <- lapply(strat_eff_list, function(df){
    out <- df %>%
      mutate('OR' = sprintf("%.3f", Estimate),
             '95% CI' = sprintf("%.3f, %.3f", `2.5 %`, `97.5 %`),
             'p-value' = sprintf("%.3f", `Pr(>Chisq)`)
             ) %>%
      dplyr::select('OR', '95% CI', 'p-value')
    out <- cbind(out[1,],out[2,])
    colnames(out) <- paste0(
      rep(c('No parental smoking_','Parental smoking_'),each=3),
      rep(c('OR','95% CI','p-value'),2)
      )
    out
    })
  names(out_list$unadj_cc) <- paste0("Complete records_", names(out_list$unadj_cc))
  names(out_list$adj_cc) <- paste0("Complete records_", names(out_list$adj_cc))
  names(out_list$unadj_imp) <- paste0("Imputed data_", names(out_list$unadj_imp))
  names(out_list$adj_imp) <- paste0("Imputed data_", names(out_list$adj_imp))
  out <- list('unadj' = cbind(out_list$unadj_cc, out_list$unadj_imp), 
              'adj' = cbind(out_list$adj_cc, out_list$adj_imp))
  out <- do.call('rbind',out)
  out$adj <- c('Unadjusted','Adjusted')
  out <- cbind('Characteristic'='SMFQ scores, >=8 (14y)',out)
  out
}
strat_effs_tab <- get_strat_effs_tab(strat_effs)

# Get gformula estimates from complete records log files
get_gform_ests <- function(pathtolog){
  
  logout <- readLines(pathtolog)
  effect_lines <- grep("r\\((se_)?(tce|cde|nie|nde)\\)", logout, value = TRUE)
  effect_names <- sub("^\\s*r\\((.*?)\\)\\s*=.*", "\\1", effect_lines)
  effect_vals <- as.numeric(sub(".*=\\s*", "", effect_lines))
  named_effects <- setNames(effect_vals, effect_names)
  
  out <- data.frame(
    Effect = c('TCE','CDE','NDE','NIE'),
    logodds = named_effects[c('tce','cde','nde','nie')],
    SE = named_effects[c('tce','cde','nde','nie')],
    OR = exp(named_effects[c('tce','cde','nde','nie')])
  )
  out$logLCI <- out$logodds - (1.96 * out$SE)
  out$logUCI <- out$logodds + (1.96 * out$SE)
  out$LCI <- exp(out$logLCI)
  out$UCI <- exp(out$logUCI)
  
  return(out)
}
cc_adj_gform_ests <- get_gform_ests("//path/to/files/gformula_adj_cc.log")
cc_adj_gform_ests_postc <- get_gform_ests("//path/to/files/gformula_adj_cc_postc.log")

cc_unadj_gform_ests <- get_gform_ests("//path/to/files/gformula_unadj_cc.log")
cc_unadj_gform_ests_postc <- get_gform_ests("//path/to/files/gformula_unadj_cc_postc.log")

# Get proportion mediated (PM) from complete records log files 
get_gform_pm <- function(pathtolog){
  logout <- readLines(pathtolog)
  pm_lines <- grep("r\\((pm)\\)", logout, value = TRUE)
  pm <- as.numeric(sub(".*=\\s*", "", pm_lines))
  return(pm*100)
}
cc_adj_gform_pm <- get_gform_pm("//path/to/files/gformula_adj_cc.log")
cc_adj_gform_postc_pm <- get_gform_pm("//path/to/files/gformula_adj_cc_postc.log")

cc_unadj_gform_pm <- get_gform_pm("//path/to/files/gformula_unadj_cc.log")
cc_unadj_gform_postc_pm <- get_gform_pm("//path/to/files/gformula_unadj_cc_postc.log")

# Append goformula log files looped over imputed datasets
#!blocked out as only need to run once
#log_dir <- "//path/to/files/imp_log_files"
#append_logs <- function(output_file, fileheader){
#  output_conn <- file(output_file, open = "a")
#  for (i in 1:50) {
#    log_file <- file.path(log_dir, paste0(fileheader, i, ".log"))
#    log_content <- readLines(log_file)
#    writeLines(log_content, con = output_conn)
#  }
#  close(output_conn)
#  rm(i)
#}
#setwd("//path/to/files/Parental smoking/Outputs")
#append_logs("combined_gformula_adj_mi_logs.log","gformula_adj_mi_log_")
#append_logs("combined_gformula_adj_postc_mi_logs.log","gformula_adj_mi_postc_log_")
#append_logs("combined_gformula_unadj_mi_logs.log","gformula_unadj_mi_log_")
#append_logs("combined_gformula_unadj_postc_mi_logs.log","gformula_unadj_mi_postc_log_")
#!

# Extract imputed gformula estimates from appended log files
get_imp_logests <- function(pathtolog){
  logout <- readLines(pathtolog)
  DT_log <- data.table(txt = logout[grepl(pattern = 'Imp number is:  |) =', logout)])
  DT_log <- DT_log[!grepl(". di ", DT_log$txt),]
  DT_log[, grp := cumsum(grepl('Imp number is', txt))]
  DT_log$var2 <- parse_number(DT_log$txt)
  DT_log$var3 <- unlist(str_extract_all(DT_log$txt,  "Imp number|(?<=\\().+?(?=\\))"))
  out <- dcast(grp ~ var3, data = DT_log, value.var = "var2")
  col_order <- c("Imp number", "tce", "se_tce", "nde", "se_nde", "nie", "se_nie", "pm", "se_pm", "cde", "se_cde")
  out <- data.frame(out[, ..col_order])
  return(out)
}
imp_adj_logests <- get_imp_logests("//path/to/files/combined_gformula_adj_mi_logs.log")
imp_adj_logests_postc <- get_imp_logests("//path/to/files/combined_gformula_adj_postc_mi_logs.log")
imp_unadj_logests <- get_imp_logests("//path/to/files/combined_gformula_unadj_mi_logs.log")
imp_unadj_logests_postc <- get_imp_logests("//path/to/files/combined_gformula_unadj_postc_mi_logs.log")

# Helper function to calculate pooled estimates
pool_ests <- function(data, coef_col, se_col) {
  pooled_mean <- mean(data[,coef_col])
  within_var <- mean(data[,se_col]^2)
  between_var <- var(data[,coef_col])
  df_correction <- (nrow(data) + 1) / nrow(data)
  total_var <- within_var + between_var * df_correction
  pooled_se <- sqrt(total_var)
  ci_lower <- pooled_mean - 1.96 * pooled_se
  ci_upper <- pooled_mean + 1.96 * pooled_se
  out <- list(
    logodds = pooled_mean,
    SE = pooled_se,
    logLCI = pooled_mean - (1.96 * pooled_se),
    logUCI = pooled_mean + (1.96 * pooled_se),
    OR = exp(pooled_mean)
  )
  out$LCI = exp(out$logLCI)
  out$UCI = exp(out$logUCI)
  return(unlist(out))
}

# Get pooled mediation model results
pool_gform_imp <- function(logdf){
  imp_gform_ests <- data.frame(
    Effect = c('TCE','CDE','NDE','NIE'),
    do.call('rbind', list(
      pool_ests(logdf, "tce", "se_tce"),
      pool_ests(logdf, "cde", "se_cde"),
      pool_ests(logdf, "nde", "se_nde"),
      pool_ests(logdf, "nie", "se_nie")
    )))
}
imp_adj_gform_ests <- pool_gform_imp(imp_adj_logests)
imp_adj_gform_ests_postc <- pool_gform_imp(imp_adj_logests_postc)
imp_unadj_gform_ests <- pool_gform_imp(imp_unadj_logests)
imp_unadj_gform_ests_postc <- pool_gform_imp(imp_unadj_logests_postc)

# Get pooled proportion mediated (PM)
pool_pm <- function(data) {
  pooled_mean <- mean(data[,'pm'])
  return(pooled_mean*100)
}
imp_adj_gform_pm <- pool_pm(imp_adj_logests)
imp_adj_gform_postc_pm <- pool_pm(imp_adj_logests_postc)
imp_unadj_gform_pm <- pool_pm(imp_unadj_logests)
imp_unadj_gform_postc_pm <- pool_pm(imp_unadj_logests_postc)

# Combine PMs
pms <- list('unadj' = c('cc'=cc_unadj_gform_pm,'cc_postc'=cc_unadj_gform_postc_pm,'imp'=imp_unadj_gform_pm,'imp_postc'=imp_unadj_gform_postc_pm),
            'adj' = c('cc'=cc_adj_gform_pm,'cc_postc'=cc_adj_gform_postc_pm,'imp'=imp_adj_gform_pm,'imp_postc'=imp_adj_gform_postc_pm))

# Make tidy gformula tables
tidy_gform <- function(df, intconflab){
  df <- df %>%
    mutate(
      'OR' = sprintf("%.2f", OR),
      'LCI' = sprintf("%.2f", LCI),
      'UCI' = sprintf("%.2f", UCI)
    ) %>%
    dplyr::select(Effect, 'OR', 'LCI', 'UCI')
  colnames(df) <- paste0(intconflab, '_', colnames(df))
  df
}
cc_adj_gform_tidy <- tidy_gform(cc_adj_gform_ests, 'Adjusted for baseline & mediator-outcome confounders')
cc_adj_gform_postc_tidy <- tidy_gform(cc_adj_gform_ests_postc, 'Adjusted for baseline & mediator-outcome confounders')
imp_adj_gform_tidy <- tidy_gform(imp_adj_gform_ests, 'Adjusted for baseline & mediator-outcome confounders')
imp_adj_gform_postc_tidy <- tidy_gform(imp_adj_gform_ests_postc, 'Adjusted for baseline & mediator-outcome confounders')

cc_unadj_gform_tidy <- tidy_gform(cc_unadj_gform_ests, 'Unadjusted for baseline & mediator-outcome confounders')
cc_unadj_gform_postc_tidy <- tidy_gform(cc_unadj_gform_ests_postc, 'Unadjusted for baseline & mediator-outcome confounders')
imp_unadj_gform_tidy <- tidy_gform(imp_unadj_gform_ests, 'Unadjusted for baseline & mediator-outcome confounders')
imp_unadj_gform_postc_tidy <- tidy_gform(imp_unadj_gform_ests_postc, 'Unadjusted for baseline & mediator-outcome confounders')

make_gform_tabs <- function(unadj, unadj_pc, adj, adj_pc) {
  
  # Combine datasets
  tab <- bind_rows(
    cbind(unadj, adj),
    cbind(unadj_pc, adj_pc)
  )
  # Drop redundant column and rename
  tab$`Adjusted for baseline & mediator-outcome confounders_Effect` <- NULL
  colnames(tab)[colnames(tab) %in% "Unadjusted for baseline & mediator-outcome confounders_Effect"] <- "Effect"
  # Add adjustment grouping label
  tab$intconf <- rep(c(
    "No intermediate confounder",
    "Including early offspring substance use (13y)"
  ), each = 4)
  # Create gt table grouped by adjustment status
  gt_tab <- gt(tab, groupname_col = "intconf") %>%
    # Automatically generate spanners using underscores in colnames
    tab_spanner_delim(delim = "_") %>%
    # Ensure Effect column is nicely labelled and aligned
    cols_label(Effect = "Effect") %>%
    cols_align(align = "left", columns = "Effect") %>%
    cols_align(align = "center", columns = -c(Effect)) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_spanners(),
        cells_column_labels(),
        cells_body(columns = "Effect"),
        cells_row_groups()
      )
    ) %>%
    # Add note below
    tab_source_note(
      md("**TCE** = Total causal effect; **CDE** = Controlled direct effect; **NDE** = Natural direct effect; **NIE** = Natural indirect effect")
    )
  
  return(gt_tab)
}

gform_tabs <- list('cc' = make_gform_tabs(cc_unadj_gform_tidy, cc_unadj_gform_postc_tidy, cc_adj_gform_tidy, cc_adj_gform_postc_tidy),
                   'imp' = make_gform_tabs(imp_unadj_gform_tidy, imp_unadj_gform_postc_tidy, imp_adj_gform_tidy, imp_adj_gform_postc_tidy))

# Gformula plot
gform_plt_ests <- rbind(cc_adj_gform_ests,
                        cc_adj_gform_ests_postc,
                        cc_unadj_gform_ests,
                        cc_unadj_gform_ests_postc,
                        imp_adj_gform_ests,
                        imp_adj_gform_ests_postc,
                        imp_unadj_gform_ests,
                        imp_unadj_gform_ests_postc
)
gform_plt_ests$intconf <- rep(c(F,T), each=4, times=4)
gform_plt_ests$adj <- rep(c(T,F), each=8, times=2)
gform_plt_ests$imp <- rep(c(F,T), each=16)

gform_pltit <- function(df){
  pd <- position_dodge(0.6)
  ggplot(df, aes(x=Effect, y=logodds, ymin=logLCI, ymax=logUCI, shape=intconf, colour=adj)) +
    geom_errorbar(lwd = 0.8, width = .1, position = pd) +
    geom_point(size = 2.2, position = pd) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(-0.1, 1.1, 0.2)) +
    #scale_color_manual(values = c("#999999", "#D55E00")) +
    theme_bw() +
    theme(
      text = element_text(size = 12, family = "Calibri"),
      legend.position = "bottom", 
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 10),
      legend.box = "vertical",
      legend.location = "plot",
      legend.spacing.y = unit(0.05, "cm")
      ) +  
    labs(x = "\nEffect", y = "log(OR)\n", shape = "Intermediate confounder : ", colour = "Adjusted for baseline & mediator-outcome confounders : ")
}

gform_plt <- gform_pltit(gform_plt_ests[gform_plt_ests$imp %in% T,])
ggsave(plot = gform_plt, filename="//path/to/files/gform_plt.png", width = 9, height = 4, dpi = 350)

# Function to remove NAs from upset plot
fix_upset_labs <- function(x) {
  if (!is.null(x$label) && is.character(x$label)) {
    x$label <- sub("_NA$", "", x$label)
  }
  if (!is.null(x$children)) x$children <- lapply(x$children, fix_upset_labs)
  if (!is.null(x$grobs))    x$grobs    <- lapply(x$grobs,    fix_upset_labs)
  if (!is.null(x$grob))     x$grob     <- fix_upset_labs(x$grob)   # some wrappers use $grob
  x
}

#!blocked out as doesn't show in markdown
# Missingness in analytic sample
png("//path/to/files/upset_ana_samp.png",
    width = 9, height = 6, units = "in", res = 350)
upset_ana_samp <- gg_miss_upset(df[,allvars], nsets = length(allvars[sapply(df[,allvars], function(x) sum(is.na(x))>0)]), point.size = 1.4, number.angles = 0, text.scale = 0.7)
upset_ana_samp$Matrix   <- fix_upset_labs(upset_ana_samp$Matrix)
upset_ana_samp
dev.off()
#!

# Univariable associations between analysis variables and missingness in the analytic sample
df$completecase <- ifelse(apply(df[,allvars],1,anyNA)==F,1,0)
univar_miss <- tbl_uvregression(df[,c('completecase',allvars)], method = glm, 
                                y = completecase, method.args = list(family = binomial), 
                                pvalue_fun = label_style_pvalue(digits = 3),
                                exponentiate = TRUE) %>% as_gt()

# Univariable associations between analysis variables and being in the analytic sample
alldf$insamp <- ifelse(!is.na(alldf[,exposure]),1,0)
univar_anasamp <- tbl_uvregression(alldf[,c('insamp',allvars[!allvars %in% exposure])], method = glm, 
                                   y = insamp, method.args = list(family = binomial), 
                                   pvalue_fun = label_style_pvalue(digits = 3),
                                   exponentiate = TRUE) %>% as_gt()


#### Multivariable association between outcome and missingness in the analytic sample
select_conf <- c('m_pregsmk' ,'parent_age_0', 'parent_hiqual_0', 'parent_sc_0','hoodqual_0')
nrow(df[complete.cases(df[,c(outcome, exposure, mediator, select_conf)]),])
table(df[complete.cases(df[,c(outcome, exposure, mediator, select_conf)]),'completecase'])

multvar_miss_mod <- glm(formula = formula(paste0('completecase','~', paste0(c(outcome, exposure, mediator, select_conf), collapse = '+'))), family=binomial(link = "logit"), data = df)

multvar_miss_tab <- tbl_regression(multvar_miss_mod, exponentiate = T, pvalue_fun = label_style_pvalue(digits = 3)) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  as_gt()


# Delete all objects other than main tables/plots
table_names <- c(
  
  #variable names
  'exposure',
  'intconf',
  'mediator',
  'outcome',
  'baseconf',
  'medoutconf',
  'allvars',
  'confvars',
  
  #data
  'df',
  'alldf',
  'df_cc',
  'imp',
  
  #gform effect description
  'g_med_effs_tab',
  
  #descriptive stats
  'desc_tabs',
  
  #OR tables
  'unadj_intconf_med_tabs',
  'adj_intconf_med_tabs',
  'unadj_outc_tabs',
  'adj_outc_tabs',
  
  #interaction tables
  'unadj_inter_tabs',
  'adj_inter_tabs',
  
  #stratified OR
  'strat_effs_tab',
  
  #gformula mediation
  'pms',
  'gform_tabs'
  
  #missingness tabs
  'univar_miss',
  'univar_anasamp',
  'multvar_miss_tab'
  
  )
rm(list = setdiff(ls(), table_names))
ls()
