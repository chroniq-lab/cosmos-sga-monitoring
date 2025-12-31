
dx_rx = c('htn_dx', 'obesity_dx', 'cerebrovascular_dx', 'hld_dx', 
          'cardiovascular_dx', 'pulmonary_dx', 'pcos_dx')


basic_covariates = paste0(" + raceeth + female + age")
covariates0 = paste0(" + raceeth + female + age + region + diff_exposure_DM + bmi_history + year + 
                      insurance_medicare + insurance_medicaid + insurance_other + insurance_selfpay +
                      SviOverallPctlRankByZip2020_X_imputed + ValidatedStateOrProvince_X")
covariates_nosex = paste0(" + raceeth + age + region + diff_exposure_DM + bmi_history + year + 
                      insurance_medicare + insurance_medicaid + insurance_other + insurance_selfpay +
                      SviOverallPctlRankByZip2020_X_imputed + ValidatedStateOrProvince_X")

dx_rx_covariates = paste0(" + ",paste0(dx_rx,collapse=" + "))
cci_covariate = c(" + cci_category")

# prescriber_covariate = paste0(" + prescriber_index:ap_type:period")
# hisbmi_covariate = c(" + bmi_history_category:ap_type:period")
# comorbidity_covariate = c(" + comorbidity_ava:ap_type:period")
# sumcomorb_covariate = c(" + total_comorb_category:ap_type:period")
cci_effmod= c(" + cci_category:ap_type:period")
sex_effmod= c(" + sex:ap_type:period")
encounter_covariate = c(" + N_enc")

exposure_main = c("ap_type + period + ap_type:period")
exposure_main_wgr = c("ap_type_wgr + period + ap_type_wgr:period")
exposure_main_polypharmacy = c("ap_type_polypharmacy + period + ap_type_polypharmacy:period")

outcome_hba1c_ava = c("hba1c_ava ~ ")
outcome_bmi_ava = c("bmi_ava ~ ")
# outcome_bp_ava = c("bp_ava ~ ")
outcome_ldl_ava = c("ldl_ava ~ ")
outcome_nephropathy_ava = c("nephropathy_ava ~ ")
# outcome_neuropathy_ava = c("neuropathy_ava ~ ")
# outcome_retinopathy_ava = c("retinopathy_ava ~ ")
# outcome_anycpl_ava = c("anycpl_ava ~ ")
# outcome_anyout5_ava = c("anyout5_ava ~ ")




