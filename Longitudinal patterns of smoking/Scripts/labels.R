##LOAD
library(haven)

##LABELS (N = 524)
#evercana_10 not included
exp_labs <- list(
  #Family smoking
  m_pregsmk = c('m_pre_pregsmk_gest','m_f3m_pregsmk_gest','m_l2w_pregsmk_gest','m_l2m_pregsmk_8w'),
  m_eversmk = c('m_eversmk_gest','m_eversmk_8','m_eversmk_11','m_eversmk_18','m_eversmk_22'),
  m_regsmk = c('m_regsmk_gest','m_regsmk_8w','m_regsmk_8','m_regsmk_11'),
  m_currsmk = c('m_currsmk_18','m_currsmk_22'),
  m_dailsmk = c('m_dailsmk_8w','m_dailsmk_1','m_dailsmk_2','m_dailsmk_3','m_dailsmk_4','m_dailsmk_5','m_dailsmk_6','m_dailsmk_7','m_dailsmk_12','m_dailsmk_18','m_dailsmk_22'),
  m_stopsmk = c('m_stopsmk_8','m_stopsmk_11'),
  m_quitsmk = c('m_quitsmk_8','m_quitsmk_11','m_quitsmk_18','m_quitsmk_22'),
  p_currsmk = c('p_smk_gest','p_smk_8','p_smk_11'),
  p_dailsmk = c('p_dailsmk_l2m_8w','p_dailsmk_8w','p_dailsmk_2','p_dailsmk_3','p_dailsmk_4','p_dailsmk_6','p_dailsmk_8','p_dailsmk_9','p_dailsmk_12'),
  mgm_pregsmk = c('mgm_pregsmk_gest','mgm_pregsmk_8','mgm_pregsmk_25','mgmm_pregsmk_25','mgfm_pregsmk_25'),
  mg_smk = c('mgm_eversmk_gest','mgm_eversmk_8','mgm_eversmk_25','mgf_eversmk_gest','mgf_eversmk_8','mgf_eversmk_25','mgmm_eversmk_25','mgmf_eversmk_25','mgfm_eversmk_25','mgff_eversmk_25'),
  hh_smk = c('hh_smk_gest','hh_smk_2','hh_smk_3','hh_smk_4','hh_smk_7','hh_smk_8','hh_smk_10','hh_smk_11'),
  
  #Family substance use
  m_pregalc = c('m_pre_pregalc_gest','m_f3m_pregalc_gest','m_mov_pregalc_gest','m_l2m_pregalc_8w'),
  m_alc = c('m_alc_8w','m_alc_1','m_alc_2','m_alc_3','m_alc_4','m_alc_5','m_alc_7','m_alc_18','m_alc_22'),
  p_alc = c('p_alc_gest','p_alc_1','p_alc_2','p_alc_3','p_alc_4','p_alc_6','p_alc_9'),
  m_pregcana = c('m_pre_pregcana_gest','m_f3m_pregcana_gest','m_3m_pregcana_gest','m_l2m_pregcana_8w'),
  m_cana = c('m_cana_8w','m_cana_2','m_cana_3','m_cana_4','m_cana_5','m_cana_6','m_cana_9','m_cana_18'),
  m_pregdrug = c('m_pregdrug_gest','m_l2m_pregdrug_8w'),
  m_drug = c('m_drug_8w','m_drug_2','m_drug_3','m_drug_4','m_drug_5','m_drug_6','m_drug_9','m_drug_18'),
  m_drugprob = c('m_drugprob_gest','m_drugprob_8'),
  m_alcprob = c('m_alcprob_gest','m_alcprob_5','m_alcprob_6','m_alcprob_8','m_alcprob_9','m_alcprob_12'),
  p_alcprob = c('p_alcprob_1','p_alcprob_2','p_alcprob_3','p_alcprob_4','p_alcprob_6','p_alcprob_9','p_alcprob_12'),
  mg_alcprob = c('mgm_alcprob_gest','mgm_alcprob_8','mgf_alcprob_gest','mgf_alcprob_8'),
  mg_alcdrugdeath = c('mgm_alcprob_25','mgf_alcprob_25','mgmm_alcprob_25','mgmf_alcprob_25','mgfm_alcprob_25','mgff_alcprob_25'),
  
  #Family mental health
  m_edps = c('m_edps_gest_gr','m_edps_8w_gr','m_edps_1_gr','m_edps_2_gr','m_edps_3_gr','m_edps_28_gr'),
  m_mhprob = c('m_mhp_gest','m_mhp_1','m_mhp_2','m_mhp_3','m_mhp_4','m_mhp_5','m_mhp_6','m_mhp_8','m_mhp_9','m_mhp_11','m_mhp_12'),
  m_feltdepr = c('m_feltdepr_8w','m_pwk_feltdepr_8w','m_feltdepr_2'),
  m_gooddays = c('m_pwk_gooddays_8w','m_pwk_gooddays_1'),
  m_mhmeds = c('m_pregmhmeds_gest','m_mhmeds_8w','m_mhmeds_1','m_mhmeds_2','m_mhmeds_3','m_mhmeds_4','m_mhmeds_5','m_mhmeds_6','m_mhmeds_9','m_mhmeds_10','m_mhmeds_12','m_mhmeds_18'),
  p_mhprob = c('p_mhp_1','p_mhp_3','p_mhp_4','p_mhp_6','p_mhp_9','p_mhp_12'),
  mg_mhprob = c('mgm_mhp_gest','mgm_mhp_8','mgf_mhp_gest','mgf_mhp_8'),
  mg_mhdeath = c('mgm_mhpdeath_25','mgf_mhpdeath_25','mgmm_mhpdeath_25','mgmf_mhpdeath_25','mgfm_mhpdeath_25','mgff_mhpdeath_25'),
  
  #peer substance use
  friends_smk = c('friends_smk_10','friends_smk_14','friends_smk_16','friends_smk_18','friends_smk_20'),
  friends_alc = c('friends_alc_10','friends_alc_13','friends_alc_14','friends_alc_16','friends_alc_18','friends_alc_20','friends_drunk_20','friends_alcprob_20'),
  friends_cana = c('friends_cana_10','friends_cana_13','friends_cana_14','friends_cana_20'),
  friends_drug = c('friends_offdrug_10','friends_offdrug_13','friends_offdrug_14','friends_drug_16','friends_drug_18','friends_drug_20'),
  
  #other substance use
  eversmk = c('eversmk_8','eversmk_10'),
  everdrunk = c('everdrunk_11'),
  everhigh = c('everhigh_11','everhigh_14'),
  everalc = c('everalc_8','everalc_10','everalc_13','everalc_14','alc_16','alc_17','alc_C_18','alc_Q_18','alc_20','alc_22','alc_24','alc_28'),
  evercana = c('evercana_13','evercana_C_14','evercana_Q_14','evercana_16','evercana_17','evercana_C_18','evercana_Q_18','evercana_20','evercana_22','evercana_24','evercana_28'),
  everoffdrug = c('everoffdrug_10','everoffdrug_13','everoffdrug_14'),
  everdrug = c('everdrug_13','everdrug_14','drug_16','drug_17','drug_C_18','drug_Q_18','drug_20','drug_22','drug_24'),
  
  #mental health and wellbeing
  dawba = c('dawba_ADHD_8','dawba_oppconductdis_8','dawba_perdevdis_8','dawba_anxietydis_8','dawba_depressdis_8','dawba_mis_16','dawba_irit_16','dawba_losint_16'),
  mfq = c('cb_mfq_10_gr','mfq_10_gr','mfq_11_gr','mfq_13_gr','mfq_14_gr'),
  cisr = c('cisr_18_gr','cisr_gad_18', 'cisr_gad_24', 'cisr_mildep_18', 'cisr_mildep_24', 'cisr_moddep_18', 'cisr_moddep_24',
           'cisr_sevdep_18','cisr_sevdep_24', 'cisr_panicdis_18', 'cisr_panicdis_24', 'cisr_agoraphob_18', 
           'cisr_socphob_18', 'cisr_socphob_24', 'cisr_specphob_18', 'cisr_specphob_24', 'cisr_chrofat_18', 'cisr_chrofat_24'),
  pliks = c('pliks_18','pliks_24'),
  wemwbs = c('wemwbs_18_gr','wemwbs_23_gr'),
  mhprob = c('mhp_22'),
  mhmeds = c('mhmeds_24','mhmeds_28'),
  
  #BMI
  bmi = c('bmi_7_gr','bmi_9_gr','bmi_10_gr','bmi_11_gr','bmi_13_gr','bmi_14_gr','bmi_16_gr','bmi_18_gr','bmi_24_gr'),
  
  #diet
  kcal = c('totkcal_7_gr','totkcal_14_gr'),
  foodgr = c('foodgr_7','foodgr_14'),
  
  #physical activity
  mvpa = c('mvpa3600_11','mvpa3600_14','mvpa3600_16','mvpa_24'),
  sportclubs = c('sportclubs_11','sportclubs_13','sportclubs_14'),
  exercise = c('exerc_14','exerc_17','exerc_18','exerc_22'),
  
  #sleep
  regsleep = c('regslp_1','regslp_3','regslp_4','regslp_5','regslp_6','regslp_7','regslp_10'),
  timesleep = c('slp_wkdays_11_gr','slp_wkdays_16_gr','slp_25_gr'),
  
  #sex
  sex = c('sex'),
  ethnicity = c('m_ethnic','p_ethnic','ethnic'),
  
  #parental SEP
  m_marst = c('m_marital_gest','m_marital_1','m_marital_2','m_marital_3','m_marital_4','m_marital_7','m_marital_8','m_marital_10','m_marital_18','m_marital_22'),
  m_homown = c('m_homown_gest','m_homown_1','m_homown_3','m_homown_5','m_homown_7','m_homown_10','m_homown_18','m_homown_22'),
  parent_sc = c('m_sc_gest','p_sc_gest'),
  m_econact = c('m_econ_gest','m_econ_8','m_econ_18','m_econ_22'),
  p_econact = c('p_econ_gest','p_econ_8','p_econ_18','p_econ_22'),
  hh_income = c('hhincome_11','hhincome_18'),
  
  #Family education
  m_qual = c('m_hiqual_gest','m_qual_5','m_qual_8'),
  p_qual = c('p_hiqual_gest','p_qual_5','p_qual_8'),
  mg_qual = c('mgm_hiqual_gest','mgm_qual_8','mgf_hiqual_gest','mgf_qual_8'),
  
  #education
  eduasp = c('y11asp_14','asp_16','y11perc_14','uni_18'),
  qual = c('studqual_18','studqual_22','qual_18','qual_20','inedu_22','hiqual_26','eduyrs_26'),
  
  #employment
  econact = c('econact_17','econact_22','econact_23','econact_25'),
  income = c('income_25'),
  eduact = c('eduact_18','eduact_20','eduact_21'),
  workact = c('workact_18','workact_20','workact_21'),
  jobst = c('employst_23'),
  sc = c('nssec_all_23','nssec_5_23'),
  job = c('emply_25','shiftwrk_25','nightwrk_25'),
  
  #neighbourhood deprivation
  hoodqual = c('hoodqual_gest'),
  hoodstress = c('hoodstress_2','hoodstress_3'),
  m_urbrur = c('m_urbrur_12w_gest','m_urbrur_18w_gest','m_urbrur_32w_gest','m_urbrur_8to42w_gest','m_urbrur_8w','m_urbrur_1','m_urbrur_2','m_urbrur_3','m_urbrur_4','m_urbrur_7','m_urbrur_8','m_urbrur_9','m_urbrur_10','m_urbrur_11','m_urbrur_12','m_urbrur_18'),
  m_townsend = c('m_townsend_12w_gest','m_townsend_18w_gest','m_townsend_32w_gest','m_townsend_8to42w_gest','m_townsend_8w','m_townsend_1','m_townsend_2','m_townsend_3','m_townsend_4','m_townsend_5','m_townsend_6','m_townsend_7','m_townsend_8','m_townsend_9','m_townsend_10','m_townsend_11','m_townsend_12','m_townsend_18'),
  m_IMD = c('m_IMD_12w_gest','m_IMD_18w_gest','m_IMD_32w_gest','m_IMD_8to42w_gest','m_IMD_8w','m_IMD_1','m_IMD_2','m_IMD_3','m_IMD_5','m_IMD_6','m_IMD_7','m_IMD_8','m_IMD_9','m_IMD_10','m_IMD_11','m_IMD_18'),
  urbrur = c('urbrur_ccb_8','urbrur_ccc_8','urbrur_ccd_9','urbrur_cce_9','urbrur_ccf_10','urbrur_ccg_10','urbrur_11','urbrur_cck_12','urbrur_ccl_12','urbrur_ccm_13','urbrur_ccn_13','urbrur_ccp_14','urbrur_ccq_14','urbrur_ccr_14','urbrur_17','urbrur_18','urbrur_20'),
  townsend = c('townsend_ccb_8','townsend_ccc_8','townsend_ccd_9','townsend_cce_9','townsend_ccf_10','townsend_ccg_10','townsend_11','townsend_cck_12','townsend_ccl_12','townsend_ccm_13','townsend_ccn_13','townsend_ccp_14','townsend_ccq_14','townsend_ccr_14','townsend_17','townsend_18','townsend_20'),
  IMD = c('IMD_ccb_8','IMD_ccc_8','IMD_ccd_9','IMD_cce_9','IMD_ccf_10','IMD_ccg_10','IMD_11','IMD_cck_12','IMD_ccl_12','IMD_ccm_13','IMD_ccn_13','IMD_ccp_14','IMD_ccq_14','IMD_ccr_14','IMD_17','IMD_18','IMD_20'),
  
  #ACEs
  ACEs = c('aces_catcl0to16','aces_sccl0to16','aces_catex0to16','aces_scex0to16'),
  
  #Trauma
  trauma = c('trauma0to5','trauma5to11','trauma11to17','trauma0to17'),
  
  #cotinine
  m_cotinine = c('m_cotinine_trim1_gest_gr','m_cotinine_trim3_gest_gr'),
  cotinine = c('cotinine_7_gr','cotinine_16_gr','cotinine_18_gr'),
  
  #pregnancy
  preg = c('everpreg_17','everpreg_21','everpreg_22','everpreg_24'),
  npregs = c('npregs_21'),
  baby = c('baby_22','baby_24'),
  currpreg = c('currpreg_20','currpreg_21','currpreg_23'),
  parent = c('parent_20','parent_21','parent_22','parent_23','parent_25','parent_28')
)
labs <- unname(unlist(exp_labs))

sub_labs <- list(
  'Family smoking' = c(
    'm_pre_pregsmk_gest','m_f3m_pregsmk_gest','m_l2w_pregsmk_gest','m_l2m_pregsmk_8w',
    'm_eversmk_gest','m_eversmk_8','m_eversmk_11','m_eversmk_18','m_eversmk_22',
    'm_regsmk_gest','m_regsmk_8w','m_regsmk_8','m_regsmk_11',
    'm_currsmk_18','m_currsmk_22',
    'm_dailsmk_8w','m_dailsmk_1','m_dailsmk_2','m_dailsmk_3','m_dailsmk_4','m_dailsmk_5','m_dailsmk_6','m_dailsmk_7','m_dailsmk_12','m_dailsmk_18','m_dailsmk_22',
    'm_stopsmk_8','m_stopsmk_11',
    'm_quitsmk_8','m_quitsmk_11','m_quitsmk_18','m_quitsmk_22',
    'p_smk_gest','p_smk_8','p_smk_11',
    'p_dailsmk_l2m_8w','p_dailsmk_8w','p_dailsmk_2','p_dailsmk_3','p_dailsmk_4','p_dailsmk_6','p_dailsmk_8','p_dailsmk_9','p_dailsmk_12',
    'mgm_pregsmk_gest','mgm_pregsmk_8','mgm_pregsmk_25','mgmm_pregsmk_25','mgfm_pregsmk_25',
    'mgm_eversmk_gest','mgm_eversmk_8','mgm_eversmk_25','mgf_eversmk_gest','mgf_eversmk_8','mgf_eversmk_25','mgmm_eversmk_25','mgmf_eversmk_25','mgfm_eversmk_25','mgff_eversmk_25',
    'hh_smk_gest','hh_smk_2','hh_smk_3','hh_smk_4','hh_smk_7','hh_smk_8','hh_smk_10','hh_smk_11'
  ),
  'Family substance use' = c(
    'm_pre_pregalc_gest','m_f3m_pregalc_gest','m_mov_pregalc_gest','m_l2m_pregalc_8w',
    'm_alc_8w','m_alc_1','m_alc_2','m_alc_3','m_alc_4','m_alc_5','m_alc_7','m_alc_18','m_alc_22',
    'p_alc_gest','p_alc_1','p_alc_2','p_alc_3','p_alc_4','p_alc_6','p_alc_9',
    'm_pre_pregcana_gest','m_f3m_pregcana_gest','m_3m_pregcana_gest','m_l2m_pregcana_8w',
    'm_cana_8w','m_cana_2','m_cana_3','m_cana_4','m_cana_5','m_cana_6','m_cana_9','m_cana_18',
    'm_pregdrug_gest','m_l2m_pregdrug_8w',
    'm_drug_8w','m_drug_2','m_drug_3','m_drug_4','m_drug_5','m_drug_6','m_drug_9','m_drug_18',
    'm_drugprob_gest','m_drugprob_8',
    'm_alcprob_gest','m_alcprob_5','m_alcprob_6','m_alcprob_8','m_alcprob_9','m_alcprob_12',
    'p_alcprob_1','p_alcprob_2','p_alcprob_3','p_alcprob_4','p_alcprob_6','p_alcprob_9','p_alcprob_12',
    'mgm_alcprob_gest','mgm_alcprob_8','mgf_alcprob_gest','mgf_alcprob_8',
    'mgm_alcprob_25','mgf_alcprob_25','mgmm_alcprob_25','mgmf_alcprob_25','mgfm_alcprob_25','mgff_alcprob_25'
  ),
  'Family mental health' = c(
    'm_edps_gest_gr','m_edps_8w_gr','m_edps_1_gr','m_edps_2_gr','m_edps_3_gr','m_edps_28_gr',
    'm_mhp_gest','m_mhp_1','m_mhp_2','m_mhp_3','m_mhp_4','m_mhp_5','m_mhp_6','m_mhp_8','m_mhp_9','m_mhp_11','m_mhp_12',
    'm_feltdepr_8w','m_pwk_feltdepr_8w','m_feltdepr_2',
    'm_pwk_gooddays_8w','m_pwk_gooddays_1',
    'm_pregmhmeds_gest','m_mhmeds_8w','m_mhmeds_1','m_mhmeds_2','m_mhmeds_3','m_mhmeds_4','m_mhmeds_5','m_mhmeds_6','m_mhmeds_9','m_mhmeds_10','m_mhmeds_12','m_mhmeds_18',
    'p_mhp_1','p_mhp_3','p_mhp_4','p_mhp_6','p_mhp_9','p_mhp_12',
    'mgm_mhp_gest','mgm_mhp_8','mgf_mhp_gest','mgf_mhp_8',
    'mgm_mhpdeath_25','mgf_mhpdeath_25','mgmm_mhpdeath_25','mgmf_mhpdeath_25','mgfm_mhpdeath_25','mgff_mhpdeath_25'
  ),
  'Peer smoking' = c(
    'friends_smk_10','friends_smk_14','friends_smk_16','friends_smk_18','friends_smk_20'
  ),
  'Peer substance use' = c(
    'friends_alc_10','friends_alc_13','friends_alc_14','friends_alc_16','friends_alc_18','friends_alc_20','friends_drunk_20','friends_alcprob_20',
    'friends_cana_10','friends_cana_13','friends_cana_14','friends_cana_20',
    'friends_offdrug_10','friends_offdrug_13','friends_offdrug_14','friends_drug_16','friends_drug_18','friends_drug_20'
  ),
  'Other substance use' = c(
    'eversmk_8','eversmk_10',
    'everdrunk_11','everhigh_11','everhigh_14',
    'everalc_8','everalc_10','everalc_13','everalc_14','alc_16','alc_17','alc_C_18','alc_Q_18','alc_20','alc_22','alc_24','alc_28',
    'evercana_13','evercana_C_14','evercana_Q_14','evercana_16','evercana_17','evercana_C_18','evercana_Q_18','evercana_20','evercana_22','evercana_24','evercana_28',
    'everoffdrug_10','everoffdrug_13','everoffdrug_14',
    'everdrug_13','everdrug_14','drug_16','drug_17','drug_C_18','drug_Q_18','drug_20','drug_22','drug_24'
  ),
  'Mental health and wellbeing' = c(
    'dawba_ADHD_8', 'dawba_oppconductdis_8', 'dawba_perdevdis_8', 'dawba_anxietydis_8', 'dawba_depressdis_8','dawba_mis_16','dawba_irit_16','dawba_losint_16',
    'cb_mfq_10_gr','mfq_10_gr','mfq_11_gr','mfq_13_gr','mfq_14_gr',
    'cisr_18_gr','cisr_gad_18', 'cisr_gad_24', 'cisr_mildep_18', 'cisr_mildep_24', 'cisr_moddep_18', 'cisr_moddep_24',
    'cisr_sevdep_18','cisr_sevdep_24', 'cisr_panicdis_18', 'cisr_panicdis_24', 'cisr_agoraphob_18', 
    'cisr_socphob_18', 'cisr_socphob_24', 'cisr_specphob_18', 'cisr_specphob_24', 'cisr_chrofat_18', 'cisr_chrofat_24',
    'pliks_18','pliks_24','wemwbs_18_gr','wemwbs_23_gr','mhp_22','mhmeds_24','mhmeds_28'
  ),
  'BMI and Diet' = c('bmi_7_gr','bmi_9_gr','bmi_10_gr','bmi_11_gr','bmi_13_gr','bmi_14_gr','bmi_16_gr','bmi_18_gr','bmi_24_gr',
                     'totkcal_7_gr','totkcal_14_gr','foodgr_7','foodgr_14'
  ),
  'Physical activity and Sleep' = c('mvpa3600_11','mvpa3600_14','mvpa3600_16','mvpa_24','sportclubs_11','sportclubs_13','sportclubs_14','exerc_14','exerc_17','exerc_18','exerc_22',
                                    'regslp_1','regslp_3','regslp_4','regslp_5','regslp_6','regslp_7','regslp_10','slp_wkdays_11_gr','slp_wkdays_16_gr','slp_25_gr'
  ),
  'Sex and Ethnicity' = c('sex','m_ethnic','p_ethnic','ethnic'
  ),
  'Parental SEP' = c(
    'm_marital_gest','m_marital_1','m_marital_2','m_marital_3','m_marital_4','m_marital_7','m_marital_8','m_marital_10','m_marital_18','m_marital_22',
    'm_homown_gest','m_homown_1','m_homown_3','m_homown_5','m_homown_7','m_homown_10','m_homown_18','m_homown_22',
    'm_sc_gest','p_sc_gest',
    'm_econ_gest','m_econ_8','m_econ_18','m_econ_22',
    'p_econ_gest','p_econ_8','p_econ_18','p_econ_22',
    'hhincome_11','hhincome_18'
  ),
  'Family education' = c(
    'm_hiqual_gest','m_qual_5','m_qual_8',
    'p_hiqual_gest','p_qual_5','p_qual_8',
    'mgm_hiqual_gest','mgm_qual_8','mgf_hiqual_gest','mgf_qual_8'
  ),
  'Education' = c(
    'y11asp_14','asp_16','y11perc_14','uni_18',
    'studqual_18','studqual_22','qual_18','qual_20',
    'inedu_22','hiqual_26','eduyrs_26'
  ),
  'Employment' = c(
    'econact_17','econact_22','econact_23','econact_25','income_25',
    'eduact_18','eduact_20','eduact_21',
    'workact_18','workact_20','workact_21',
    'employst_23','nssec_all_23','nssec_5_23',
    'emply_25','shiftwrk_25','nightwrk_25'
  ),
  'Maternal neighbourhood deprivation' = c( 
    'hoodqual_gest','hoodstress_2','hoodstress_3',
    'm_urbrur_12w_gest','m_urbrur_18w_gest','m_urbrur_32w_gest','m_urbrur_8to42w_gest','m_urbrur_8w','m_urbrur_1','m_urbrur_2','m_urbrur_3','m_urbrur_4','m_urbrur_7','m_urbrur_8','m_urbrur_9','m_urbrur_10','m_urbrur_11','m_urbrur_12','m_urbrur_18',
    'm_townsend_12w_gest','m_townsend_18w_gest','m_townsend_32w_gest','m_townsend_8to42w_gest','m_townsend_8w','m_townsend_1','m_townsend_2','m_townsend_3','m_townsend_4','m_townsend_5','m_townsend_6','m_townsend_7','m_townsend_8','m_townsend_9','m_townsend_10','m_townsend_11','m_townsend_12','m_townsend_18',
    'm_IMD_12w_gest','m_IMD_18w_gest','m_IMD_32w_gest','m_IMD_8to42w_gest','m_IMD_8w','m_IMD_1','m_IMD_2','m_IMD_3','m_IMD_5','m_IMD_6','m_IMD_7','m_IMD_8','m_IMD_9','m_IMD_10','m_IMD_11','m_IMD_18'
  ),
  'Neighbourhood deprivation' = c(
    'urbrur_ccb_8','urbrur_ccc_8','urbrur_ccd_9','urbrur_cce_9','urbrur_ccf_10','urbrur_ccg_10','urbrur_11','urbrur_cck_12','urbrur_ccl_12','urbrur_ccm_13','urbrur_ccn_13','urbrur_ccp_14','urbrur_ccq_14','urbrur_ccr_14','urbrur_17','urbrur_18','urbrur_20',
    'townsend_ccb_8','townsend_ccc_8','townsend_ccd_9','townsend_cce_9','townsend_ccf_10','townsend_ccg_10','townsend_11','townsend_cck_12','townsend_ccl_12','townsend_ccm_13','townsend_ccn_13','townsend_ccp_14','townsend_ccq_14','townsend_ccr_14','townsend_17','townsend_18','townsend_20',
    'IMD_ccb_8','IMD_ccc_8','IMD_ccd_9','IMD_cce_9','IMD_ccf_10','IMD_ccg_10','IMD_11','IMD_cck_12','IMD_ccl_12','IMD_ccm_13','IMD_ccn_13','IMD_ccp_14','IMD_ccq_14','IMD_ccr_14','IMD_17','IMD_18','IMD_20'
  ),
  'ACEs and Trauma' = c(
    'aces_catcl0to16','aces_sccl0to16','aces_catex0to16','aces_scex0to16',
                        'trauma0to5','trauma5to11','trauma11to17','trauma0to17'
  ),
  'Maternal cotinine in pregnancy' = c(
    'm_cotinine_trim1_gest_gr','m_cotinine_trim3_gest_gr'
  ),
  'Cotinine' = c(
    'cotinine_7_gr','cotinine_16_gr','cotinine_18_gr'
  ),
  'Pregnancy and parenthood' = c(
    'everpreg_17','everpreg_21','everpreg_22','everpreg_24',
    'npregs_21','baby_22','baby_24','currpreg_20','currpreg_21','currpreg_23',
    'parent_20','parent_21','parent_22','parent_23','parent_25','parent_28'
  )
)



##LONG LABELS
long_labs <- list(
  #family smoking
  'Mum smoked before pregnancy' = c('m_pre_pregsmk_gest'),
  'Mum smoked first 3m pregnancy' = c('m_f3m_pregsmk_gest'),
  'Mum smoked last 2w pregnancy' = c('m_l2w_pregsmk_gest'),
  'Mum smoked last 2m pregnancy' = c('m_l2m_pregsmk_8w'),
  'Mum ever smoked' = c('m_eversmk_gest','m_eversmk_8','m_eversmk_11','m_eversmk_18','m_eversmk_22'),
  'Mum regularly smoked' = c('m_regsmk_gest','m_regsmk_8w','m_regsmk_8','m_regsmk_11'),
  'Mum currently smoked' = c('m_currsmk_18','m_currsmk_22'),
  'Mum smoked daily' = c('m_dailsmk_8w','m_dailsmk_1','m_dailsmk_2','m_dailsmk_3','m_dailsmk_4','m_dailsmk_5','m_dailsmk_6','m_dailsmk_7','m_dailsmk_12','m_dailsmk_18','m_dailsmk_22'),
  'Mum stopped smoking' = c('m_stopsmk_8','m_stopsmk_11'),
  'Time since mum quit smoking' = c('m_quitsmk_8','m_quitsmk_11','m_quitsmk_18','m_quitsmk_22'),
  'Mum partner smokes' = c('p_smk_gest','p_smk_8','p_smk_11'),
  'Mum partner smoked last 2m pregnancy' = c('p_dailsmk_l2m_8w'),
  'Mum partner smokes daily' = c('p_dailsmk_8w','p_dailsmk_2','p_dailsmk_3','p_dailsmk_4','p_dailsmk_6','p_dailsmk_8','p_dailsmk_9','p_dailsmk_12'),
  'MGM smoked in pregnancy' = c('mgm_pregsmk_gest','mgm_pregsmk_8','mgm_pregsmk_25'),
  'MGMM smoked in pregnancy' = c('mgmm_pregsmk_25'),
  'MGFM smoked in pregnancy' = c('mgfm_pregsmk_25'),
  'MGM smoked' = c('mgm_eversmk_gest','mgm_eversmk_8','mgm_eversmk_25'),
  'MGF smoked' = c('mgf_eversmk_gest','mgf_eversmk_8','mgf_eversmk_25'),
  'MGMM smoked' = c('mgmm_eversmk_25'),
  'MGMF smoked' = c('mgmf_eversmk_25'),
  'MGFM smoked' = c('mgfm_eversmk_25'),
  'MGFF smoked' = c('mgff_eversmk_25'),
  'Household members smoke' = c('hh_smk_gest','hh_smk_2','hh_smk_3','hh_smk_4','hh_smk_7','hh_smk_8','hh_smk_10','hh_smk_11'),
  
  #family substance use
  'Mum alcohol intake before pregnancy' = c('m_pre_pregalc_gest'),
  'Mum alcohol intake first 3m pregnancy' = c('m_f3m_pregalc_gest'),
  'Mum alcohol intake since baby moved' = c('m_mov_pregalc_gest'),
  'Mum alcohol intake last 2m pregnancy' = c('m_l2m_pregalc_8w'),
  'Mum alcohol intake' = c('m_alc_8w','m_alc_1','m_alc_2','m_alc_3','m_alc_4','m_alc_5','m_alc_7','m_alc_18','m_alc_22'),
  'Mum partner alcohol intake' = c('p_alc_gest','p_alc_1','p_alc_2','p_alc_3','p_alc_4','p_alc_6','p_alc_9'),
  'Mum cannabis use before pregnancy' = c('m_pre_pregcana_gest'),
  'Mum smoked first 3m pregnancy' = c('m_f3m_pregcana_gest'),
  'Mum smoked past 3 months pregnancy' = c('m_3m_pregcana_gest'),
  'Mum smoked last 2m pregnancy' = c('m_l2m_pregcana_8w'),
  'Mum cannabis use' = c('m_cana_8w','m_cana_2','m_cana_3','m_cana_4','m_cana_5','m_cana_6','m_cana_9','m_cana_18'),
  'Mum drug use in pregnancy' = c('m_pregdrug_gest'),
  'Mum drug use last 2m pregnancy' = c('m_l2m_pregdrug_8w'),
  'Mum drug use' = c('m_drug_8w','m_drug_2','m_drug_3','m_drug_4','m_drug_5','m_drug_6','m_drug_9','m_drug_18'),
  'Mum drug problem' = c('m_drugprob_gest','m_drugprob_8'),
  'Mum alcohol problem' = c('m_alcprob_gest','m_alcprob_5','m_alcprob_6','m_alcprob_8','m_alcprob_9','m_alcprob_12'),
  'Mum partner alcohol problem' = c('p_alcprob_1','p_alcprob_2','p_alcprob_3','p_alcprob_4','p_alcprob_6','p_alcprob_9','p_alcprob_12'),
  'MGM alcohol problem' = c('mgm_alcprob_gest','mgm_alcprob_8'),
  'MGF alcohol problem' = c('mgf_alcprob_gest','mgf_alcprob_8'),
  'MGM death alcohol or drugs' = c('mgm_alcprob_25'),
  'MGF death alcohol or drugs' = c('mgf_alcprob_25'),
  'MGMM death alcohol or drugs' = c('mgmm_alcprob_25'),
  'MGMF death alcohol or drugs' = c('mgmf_alcprob_25'),
  'MGFM death alcohol or drugs' = c('mgfm_alcprob_25'),
  'MGFF death alcohol or drugs' = c('mgff_alcprob_25'),
  
  #family mental health
  'Mum EDPS score' = c('m_edps_gest_gr','m_edps_8w_gr','m_edps_1_gr','m_edps_2_gr','m_edps_3_gr','m_edps_28_gr'),
  'Mum mental health problem' = c('m_mhp_gest','m_mhp_1','m_mhp_2','m_mhp_3','m_mhp_4','m_mhp_5','m_mhp_6','m_mhp_8','m_mhp_9','m_mhp_11','m_mhp_12'),
  'Mum felt depressed since birth' = c('m_feltdepr_8w'),
  'Mum felt depressed past week' = c('m_pwk_feltdepr_8w'),
  'Mum felt depressed past month' = c('m_feltdepr_2'),
  'Mum has more good days' = c('m_pwk_gooddays_8w','m_pwk_gooddays_1'),
  'Mum takes medication for mental health' = c('m_pregmhmeds_gest','m_mhmeds_8w','m_mhmeds_1','m_mhmeds_2','m_mhmeds_3','m_mhmeds_4','m_mhmeds_5','m_mhmeds_6','m_mhmeds_9','m_mhmeds_10','m_mhmeds_12','m_mhmeds_18'),
  'Mum partner mental health problem' = c('p_mhp_1','p_mhp_3','p_mhp_4','p_mhp_6','p_mhp_9','p_mhp_12'),
  'MGM mental health problem' = c('mgm_mhp_gest','mgm_mhp_8'),
  'MGF mental health problem' = c('mgf_mhp_gest','mgf_mhp_8'),
  'MGM death mental health problem' = c('mgm_mhpdeath_25'),
  'MGF death mental health problem' = c('mgf_mhpdeath_25'),
  'MGMM death mental health problem' = c('mgmm_mhpdeath_25'),
  'MGMF death mental health problem' = c('mgmf_mhpdeath_25'),
  'MGFM death mental health problem' = c('mgfm_mhpdeath_25'),
  'MGFF death mental health problem' = c('mgff_mhpdeath_25'),
  
  #peer substance use
  'Friends smoke cigarettes' = c('friends_smk_10','friends_smk_14','friends_smk_16','friends_smk_18','friends_smk_20'),
  'Friends drink alcohol' = c('friends_alc_10','friends_alc_13','friends_alc_14','friends_alc_16','friends_alc_18','friends_alc_20','friends_drunk_20'),
  'Friends have alcohol problem' = c('friends_alcprob_20'),
  'Friends use cannabis' = c('friends_cana_10','friends_cana_13','friends_cana_14','friends_cana_20'),
  'Friends been offered drugs' = c('friends_offdrug_10','friends_offdrug_13','friends_offdrug_14'),
  'Friends use drugs' = c('friends_drug_16','friends_drug_18','friends_drug_20'),
  
  #other substance use
  'Ever smoked' = c('eversmk_8','eversmk_10'),
  'Ever got drunk' = c('everdrunk_11'),
  'Ever got high' = c('everhigh_11','everhigh_14'),
  'Alcohol consumption' = c('everalc_8','everalc_10','everalc_13','everalc_14','alc_16','alc_17','alc_C_18','alc_Q_18','alc_20','alc_22','alc_24','alc_28'),
  'Cannabis use' = c('evercana_13','evercana_C_14','evercana_Q_14','evercana_16','evercana_17','evercana_C_18','evercana_Q_18','evercana_20','evercana_22','evercana_24','evercana_28'),
  'Been offered drugs' = c('everoffdrug_10','everoffdrug_13','everoffdrug_14'),
  'Drug use' = c('everdrug_13','everdrug_14','drug_16','drug_17','drug_C_18','drug_Q_18','drug_20','drug_22','drug_24'),
  
  #mental health and wellbeing
  'DAWBA: ADHD disorder' = c('dawba_ADHD_8'),
  'DAWBA: Conduct disorder' = c('dawba_oppconductdis_8'),
  'DAWBA: Development disorder' = c('dawba_perdevdis_8'),
  'DAWBA: Anxiety disorder' = c('dawba_anxietydis_8'),
  'DAWBA: Depressive disorder' = c('dawba_depressdis_8'),
  'DAWBA: Miserable' = c('dawba_mis_16'),
  'DAWBA: Irritable' = c('dawba_irit_16'),
  'DAWBA: Lost interest' = c('dawba_losint_16'),
  'MFQ score, child based' = c('cb_mfq_10_gr'),
  'MFQ score' = c('mfq_10_gr','mfq_11_gr','mfq_13_gr','mfq_14_gr'),
  'CIS-R score' = c('cisr_18_gr'),
  'CIS-R: Generalised anxiety' = c('cisr_gad_18','cisr_gad_24'),
  'CIS-R: Mild depression' = c('cisr_mildep_18','cisr_mildep_24'),
  'CIS-R: Moderate depression' = c('cisr_moddep_18','cisr_moddep_24'),
  'CIS-R: Severe depression' = c('cisr_sevdep_18','cisr_sevdep_24'), 
  'CIS-R: Panic disorder' = c('cisr_panicdis_18','cisr_panicdis_24'),
  'CIS-R: Agoraphobia' = c('cisr_agoraphob_18'), 
  'CIS-R: Social phobia' = c('cisr_socphob_18','cisr_socphob_24'),
  'CIS-R: Specic phobia' = c('cisr_specphob_18','cisr_specphob_24'),
  'CIS-R: Chronic fatigue' = c('cisr_chrofat_18','cisr_chrofat_24'),
  'PLIKS' = c('pliks_18','pliks_24'),
  'WEMWBS score' = c('wemwbs_18_gr','wemwbs_23_gr'),
  'Mental health problem' = c('mhp_22'),
  'Medication for mental health' = c('mhmeds_24','mhmeds_28'),
  
  #BMI
  'BMI' = c('bmi_7_gr','bmi_9_gr','bmi_10_gr','bmi_11_gr','bmi_13_gr','bmi_14_gr','bmi_16_gr','bmi_18_gr','bmi_24_gr'),
  
  #diet
  'Calorie intake' = c('totkcal_7_gr','totkcal_14_gr'),
  'Eat healthy food' = c('foodgr_7','foodgr_14'),
  
  #physical activity
  'MVPA' = c('mvpa3600_11','mvpa3600_14','mvpa3600_16','mvpa_24'),
  'Plays sports' = c('sportclubs_11','sportclubs_13','sportclubs_14'),
  'Exercise' = c('exerc_14','exerc_17','exerc_18','exerc_22'),
  
  #sleep
  'Regular sleep schedule' = c('regslp_1','regslp_3','regslp_4','regslp_5','regslp_6','regslp_7','regslp_10'),
  'Time asleep' = c('slp_wkdays_11_gr','slp_wkdays_16_gr','slp_25_gr'),
  
  #sex
  'Sex' = c('sex'),
  'Mum ethnicity' = c('m_ethnic'),
  'Mum partner ethnicity' = c('p_ethnic'),
  'Ethnicity' = c('ethnic'),
  
  #parental SEP
  'Mum marital status' = c('m_marital_gest','m_marital_1','m_marital_2','m_marital_3','m_marital_4','m_marital_7','m_marital_8','m_marital_10','m_marital_18','m_marital_22'),
  'Mum home ownership' = c('m_homown_gest','m_homown_1','m_homown_3','m_homown_5','m_homown_7','m_homown_10','m_homown_18','m_homown_22'),
  'Mum social class' = c('m_sc_gest'),
  'Mum partner social class' = c('p_sc_gest'),
  'Mum economic activity' = c('m_econ_gest','m_econ_8','m_econ_18','m_econ_22'),
  'Mum partner economic activity' = c('p_econ_gest','p_econ_8','p_econ_18','p_econ_22'),
  'Household income' = c('hhincome_11','hhincome_18'),
  
  #Family education
  'Mum qualifications' = c('m_hiqual_gest','m_qual_5','m_qual_8'),
  'Mum partner qualifications' = c('p_hiqual_gest','p_qual_5','p_qual_8'),
  'MGM education' = c('mgm_hiqual_gest','mgm_qual_8'),
  'MGF education' = c('mgf_hiqual_gest','mgf_qual_8'),
  
  #education
  'Aspiration to stay in education' = c('y11asp_14'),
  'Plans to stay in education after year 11' = c('asp_16'),
  'Perception of actually staying education' = c('y11perc_14'),
  'Plans to go to university' = c('uni_18'),
  'Qualifications studying' = c('studqual_18','studqual_22'),
  'Qualifications achieved' = c('qual_18','qual_20'),
  'In education' = c('inedu_22'),
  'Highest qualification acheived' = c('hiqual_26'),
  'Years in education' = c('eduyrs_26'),
  
  #employment
  'Economic activity' = c('econact_17','econact_22','econact_23','econact_25'),
  'Income' = c('income_25'),
  'Educational activity' = c('eduact_18','eduact_20','eduact_21'),
  'Work activity' = c('workact_18','workact_20','workact_21'),
  'Job status' = c('employst_23'),
  'Social class' = c('nssec_all_23','nssec_5_23'),
  'Currently employed' = c('emply_25'),
  'Shift work' = c('shiftwrk_25'),
  'Night shifts' = c('nightwrk_25'),
  
  #neighbourhood deprivation
  'Neighbourhood quality index' = c('hoodqual_gest'),
  'Neighbourhood stress score' = c('hoodstress_2','hoodstress_3'),
  'GO Urban/Rural indicator (12w gestation)' = c('m_urbrur_12w_gest'),
  'GO Urban/Rural indicator (18w gestation)' = c('m_urbrur_18w_gest'),
  'GO Urban/Rural indicator (32w gestation)' = c('m_urbrur_32w_gest'),
  'GO Urban/Rural indicator (8 to 42w gestation)' = c('m_urbrur_8to42w_gest'),
  'GO Urban/Rural indicator' = c('m_urbrur_8w','m_urbrur_1','m_urbrur_2','m_urbrur_3','m_urbrur_4','m_urbrur_7','m_urbrur_8','m_urbrur_9','m_urbrur_10','m_urbrur_11','m_urbrur_12','m_urbrur_18'),
  'G0 Townsend score (12w gestation)' = c('m_townsend_12w_gest'),
  'G0 Townsend score (18w gestation)' = c('m_townsend_18w_gest'),
  'G0 Townsend score (32w gestation)' = c('m_townsend_32w_gest'),
  'G0 Townsend score (8 to 42w gestation)' = c('m_townsend_8to42w_gest'),
  'G0 Townsend score' = c('m_townsend_8w','m_townsend_1','m_townsend_2','m_townsend_3','m_townsend_4','m_townsend_5','m_townsend_6','m_townsend_7','m_townsend_8','m_townsend_9','m_townsend_10','m_townsend_11','m_townsend_12','m_townsend_18'),
  'G0 IMD (12w gestation)' = c('m_IMD_12w_gest'),
  'G0 IMD (18w gestation)' = c('m_IMD_18w_gest'),
  'G0 IMD (32w gestation)' = c('m_IMD_32w_gest'),
  'G0 IMD (8 to 42w gestation)' = c('m_IMD_8to42w_gest'),
  'G0 IMD' = c('m_IMD_8w','m_IMD_1','m_IMD_2','m_IMD_3','m_IMD_5','m_IMD_6','m_IMD_7','m_IMD_8','m_IMD_9','m_IMD_10','m_IMD_11','m_IMD_18'),
  'Urban/Rural indicator' = c('urbrur_ccb_8','urbrur_ccc_8','urbrur_ccd_9','urbrur_cce_9','urbrur_ccf_10','urbrur_ccg_10','urbrur_11','urbrur_cck_12','urbrur_ccl_12','urbrur_ccm_13','urbrur_ccn_13','urbrur_ccp_14','urbrur_ccq_14','urbrur_ccr_14','urbrur_17','urbrur_18','urbrur_20'),
  'Townsend score' = c('townsend_ccb_8','townsend_ccc_8','townsend_ccd_9','townsend_cce_9','townsend_ccf_10','townsend_ccg_10','townsend_11','townsend_cck_12','townsend_ccl_12','townsend_ccm_13','townsend_ccn_13','townsend_ccp_14','townsend_ccq_14','townsend_ccr_14','townsend_17','townsend_18','townsend_20'),
  'Index of Multiple Deprivation' = c('IMD_ccb_8','IMD_ccc_8','IMD_ccd_9','IMD_cce_9','IMD_ccf_10','IMD_ccg_10','IMD_11','IMD_cck_12','IMD_ccl_12','IMD_ccm_13','IMD_ccn_13','IMD_ccp_14','IMD_ccq_14','IMD_ccr_14','IMD_17','IMD_18','IMD_20'),
  
  #ACEs
  'ACEs category, classic (0-16y)' = c( 'aces_catcl0to16'),
  'ACEs score, classic (0-16y)' = c('aces_sccl0to16'),
  'ACEs category, extended (0-16y)' = c('aces_catex0to16'),
  'ACEs score, extended (0-16y)' = c('aces_scex0to16'),
  
  #Trauma
  'Trauma (0-5y)' = c('trauma0to5'),
  'Trauma (5-11y)' = c('trauma5to11'),
  'Trauma (11-17y)' = c('trauma11to17'),
  'Trauma (0-17y)' = c('trauma0to17'),
  
  #cotinine
  'Mum cotinine first trimester' = c('m_cotinine_trim1_gest_gr'),
  'Mum cotinine third trimester' = c('m_cotinine_trim3_gest_gr'),
  'Cotinine' = c('cotinine_7_gr','cotinine_16_gr','cotinine_18_gr'),
  
  #pregnancy
  'Pregnancy' = c('everpreg_17','everpreg_21','everpreg_22','everpreg_24'),
  'Number of pregnancies' = c('npregs_21'),
  'Had a baby' = c('baby_22','baby_24'),
  'Currently pregnant' = c('currpreg_20','currpreg_21','currpreg_23'),
  'Is a parent' = c('parent_20','parent_21','parent_22','parent_23','parent_25','parent_28')
)



##PLTDF
pltdf <- data.frame(
  grp = rep(names(exp_labs),times=lengths(exp_labs)),
  sub_grp = rep(names(sub_labs),times=lengths(sub_labs)),
  labs = labs
)

#AGES
ages <- strsplit(pltdf$labs, '_')
ages <- sapply(ages, function(x){ifelse(tail(x,1) %in% 'gr', tail(x,2)[1], tail(x,1))})
ages[ages %in% c('ethnic','sex')] <- 'gest'
ages[ages %in% c('catcl0to16','catex0to16','sccl0to16','scex0to16','trauma0to17','trauma0to5','trauma11to17','trauma5to11')] <- 'longit'
ages <- factor(ages, levels=c('gest','8w',1:14,16:18,'20','21','22','23','24','25','26','28','longit'), labels=c('0y','8w',paste0(c(1:14,16:18,'20','21','22','23','24','25','26','28'),'y'),'Longitudinal'))
pltdf$ages <- ages
pltdf$ages_sub <- ifelse(pltdf$ages %in% '0y', 'Gestation', NA)
pltdf$ages_sub <- ifelse(pltdf$ages %in% c('8w','1y','2y','3y','4y','5y','6y','7y'), 'Early Childhood', pltdf$ages_sub)
pltdf$ages_sub <- ifelse(pltdf$ages %in% c('8y','9y','10y','11y','12y'), 'Late Childhood', pltdf$ages_sub)
pltdf$ages_sub <- ifelse(pltdf$ages %in% c('13y','14y','16y','17y','18y'), 'Adolescence', pltdf$ages_sub)
pltdf$ages_sub <- ifelse(pltdf$ages %in% c('20y','21y','22y','23y','24y','25y','26y','28y'), 'Adulthood', pltdf$ages_sub)
pltdf$ages_sub <- ifelse(pltdf$ages %in% 'Longitudinal', 'Longitudinal', pltdf$ages_sub)
pltdf$ages_sub <- factor(pltdf$ages_sub, levels=c('Gestation','Early Childhood','Late Childhood','Adolescence','Adulthood','Longitudinal'))

#REGRP LABELS (!NEED TO CHANGE TO MATCH PAPER!)
pltdf$lab_grp <- pltdf$sub_grp
pltdf$lab_grp[pltdf$lab_grp %in% c('Family smoking','Maternal cotinine in pregnancy','Peer smoking')] <- 'Family and peer smoking'
pltdf$lab_grp[pltdf$lab_grp %in% c('Family substance use','Peer substance use')] <- 'Family and peer substance use'
pltdf$lab_grp[pltdf$lab_grp %in% c('Family education','Parental SEP','Maternal neighbourhood deprivation')] <- 'Family sociodemographic factors'
pltdf$lab_grp[pltdf$lab_grp %in% c('Other substance use','BMI and Diet','Physical activity and Sleep')] <- 'Individual lifestyle factors'
pltdf$lab_grp[pltdf$lab_grp %in% c('Sex and Ethnicity','Education','Employment','Neighbourhood deprivation')] <- 'Individual sociodemographic factors'
pltdf$lab_grp[pltdf$lab_grp %in% c('Family mental health','Mental health and wellbeing','ACEs and Trauma','Pregnancy and parenthood','Cotinine')] <- 'Mental health and other factors'

#LONG LABS
pltdf$long_labs <- rep(names(long_labs),times=lengths(long_labs))[match(labs,unlist(long_labs))]
pltdf$long_labs_noages <- pltdf$long_labs
pltdf$long_labs[!pltdf$ages %in% c('Longitudinal')] <- paste0(pltdf$long_labs[!pltdf$ages %in% c('Longitudinal')], ' (', pltdf$ages[!pltdf$ages %in% c('Longitudinal')], ')')

expdf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/exposure_measures.dta')
smokdf <- read_dta('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/data/smoking_categories.dta')
smokdf <- smokdf[smokdf$miss_longit %in% 0,]
expdf <- merge(expdf, smokdf[,c('aln','qlet')], by=c('aln','qlet'))
info <- data.frame(do.call(rbind,lapply(expdf[,labs], function(x){names(attributes(x)$labels)})),
                   do.call(rbind, lapply(expdf[,labs], table)),
                   sapply(expdf[,labs], function(x){sum(!is.na(x))}))
colnames(info) <- c('labs_unexp','labs_exp', 'sum_unexp','sum_exp', 'sum')
pltdf <- cbind(pltdf,info)
pltdf$prop_exp <- pltdf$sum_exp/pltdf$sum*100
save(pltdf, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/pltdf.rda')
rm(ages,labs,long_labs,exp_labs,sub_labs,info,expdf,smokdf)
