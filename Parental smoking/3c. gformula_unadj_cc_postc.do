*Have to run due to error on HPC: command ChkIn is unrecognized r(199)
do "/user/home/username/ado/plus/c/chkin.ado"

*Start log
log using gformula_unadj_cc_postc.log, replace

*Load data
use "parent_smk_data_cc.dta", clear

*Define variables
local xvar "parent_dailsmk_12"
local mvar "mfq_14_8cut"
local yvar "currsmk_16"
local icvar "subst_13"

*Create interaction term
gen inter = `xvar' * `mvar'

*Run gformula
gformula `yvar' `mvar' `xvar' inter `icvar', ///
mediation ///
obe ///
outcome(`yvar') ///
exposure(`xvar') ///
mediator(`mvar') ///
derived(inter) derrules(inter:`xvar' * `mvar') ///
post_confs(`icvar') ///
commands(`yvar':logit, `mvar':logit, `icvar':logit) ///
equations(`yvar': `xvar' `mvar' inter `icvar', ///
`mvar': `xvar' `icvar', ///
`icvar': `xvar') ///
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

