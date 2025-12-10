*Have to run due to error on HPC: command ChkIn is unrecognized r(199)
do "/user/home/qg21962/ado/plus/c/chkin.ado"

*Start log
log using gformula_adj_cc.log, replace

*Load data
use "parent_smk_data_cc.dta", clear

*Define variables
local baselineconflist "i.parent_sc_0 i.parent_hiqual_0 m_homown_10 hhincome_8 m_pregsmk parent_epds_8 parent_mhp_8 hoodqual_0 i.townsend_8 parent_age_0 sex ethnicity parent_cruel_9 peer_subst_13 monitor_13 bullying_13"
local baselineconflist2 "parent_sc_0 parent_hiqual_0 m_homown_10 hhincome_8 m_pregsmk parent_epds_8 parent_mhp_8 hoodqual_0 townsend_8 parent_age_0 sex ethnicity parent_cruel_9 peer_subst_13 monitor_13 bullying_13"
local xvar "parent_dailsmk_12"
local mvar "mfq_14_8cut"
local yvar "currsmk_16"

*Create interaction term
gen inter = `xvar' * `mvar'

*Run gformula
gformula `yvar' `mvar' `xvar' inter `baselineconflist2', ///
mediation ///
obe ///
outcome(`yvar') ///
exposure(`xvar') ///
mediator(`mvar') ///
derived(inter) derrules(inter:`xvar' * `mvar') ///
base_confs(`baselineconflist2') ///
commands(`yvar':logit, `mvar':logit) ///
equations(`yvar': `xvar' `mvar' inter `baselineconflist', ///
`mvar': `xvar' `baselineconflist') ///
seed(14071989) ///
control(`mvar':0) ///
minsim ///
moreMC ///
simulations(100000) ///
samples(100) ///
logOR

*Return results
return list

*End logging
log close
