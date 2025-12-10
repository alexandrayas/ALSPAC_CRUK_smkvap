*Have to run due to error on HPC: command ChkIn is unrecognized r(199)
do "/user/home/qg21962/ado/plus/c/chkin.ado"

*Set args 
args impdata

*Start logging with a unique log file for each iteration
log using "gformula_adj_mi_postc_log_`impdata'.log", replace

*Print impuation interaction number
di "Imp number is:  " `impdata'

*Load and filter the data
use "parent_smk_long_imp.dta", clear
di "`impdata'"
keep if imp == `impdata'

*Define variables
local baselineconflist "i.parent_sc_0 i.parent_hiqual_0 m_homown_10 hhincome_8 m_pregsmk parent_epds_8 parent_mhp_8 hoodqual_0 i.townsend_8 parent_age_0 sex ethnicity parent_cruel_9 peer_subst_13 monitor_13 bullying_13"
local baselineconflist2 "parent_sc_0 parent_hiqual_0 m_homown_10 hhincome_8 m_pregsmk parent_epds_8 parent_mhp_8 hoodqual_0 townsend_8 parent_age_0 sex ethnicity parent_cruel_9 peer_subst_13 monitor_13 bullying_13"
local xvar "parent_dailsmk_12"
local mvar "mfq_14_8cut"
local yvar "currsmk_16"
local icvar "subst_13"

*Create interaction term
gen inter = `xvar' * `mvar'

*Run gformula
gformula `yvar' `mvar' `xvar' inter `baselineconflist2' `icvar', ///
mediation ///
obe ///
outcome(`yvar') ///
exposure(`xvar') ///
mediator(`mvar') ///
derived(inter) derrules(inter:`xvar' * `mvar') ///
base_confs(`baselineconflist2') ///
post_confs(`icvar') ///
commands(`yvar':logit, `mvar':logit, `icvar':logit) ///
equations(`yvar': `xvar' `mvar' inter `baselineconflist' `icvar', ///
`mvar': `xvar' `baselineconflist' `icvar', ///
`icvar': `xvar' `baselineconflist') ///
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
