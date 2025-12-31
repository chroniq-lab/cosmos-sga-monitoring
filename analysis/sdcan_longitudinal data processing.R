rm(list=ls());gc();source(".Rprofile")

long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  dplyr::select(PatientDurableKey,raceeth,female,age,ap_type,ap_type_wgr,ap_type_polypharmacy,region,
                insurance_medicare,insurance_other,insurance_medicaid,insurance_selfpay,
                SviOverallPctlRankByZip2020_X_imputed,ValidatedStateOrProvince_X,
                year,bmi_history,diff_exposure_DM,CCI_score,cci_category,
                ends_with("_Y1"),ends_with("_Y2"),ends_with("_Y3"),
                ends_with("_dx")
  ) %>% 
  pivot_longer(cols = matches("Y[1-3]$"),names_to = c(".value", "period"),names_pattern = "(.*)(_Y[1-3])$") %>% 
  mutate(period = sub("_", "", period)) %>% 
  mutate(bmi_history_category = case_when(bmi_history >= 25 & bmi_history < 30 ~ "Overweight",
                                          bmi_history >= 30 ~ "Obese",
                                          TRUE ~ "Underweight or Normal")) %>% 
  mutate(hba1c_ava = case_when(!is.na(mean_hba1c) ~ 1,
                               TRUE ~ 0),
         bmi_ava = case_when(!is.na(mean_bmi) ~ 1,
                             TRUE ~ 0),
         bp_ava = case_when(!is.na(mean_sbp)&!is.na(mean_dbp) ~ 1,
                            TRUE ~ 0),
         ldl_ava = case_when(!is.na(mean_ldl) ~ 1,
                             TRUE ~ 0),
         nephropathy_ava = case_when(!is.na(nephropathy)|!is.na(mean_uacr)|!is.na(mean_egfr)|!is.na(mean_egfr2) ~ 1,
                                     TRUE ~ 0),
         neuropathy_ava = case_when(!is.na(neuropathy) ~ 1,
                                    TRUE ~ 0),
         retinopathy_ava = case_when(!is.na(retinopathy) ~ 1,
                                     TRUE ~ 0)) %>% 
  mutate(anycpl_ava = case_when(
    nephropathy_ava==1|neuropathy_ava==1|retinopathy_ava==1 ~ 1,
    TRUE ~ 0)) %>% 
  mutate(anyout5_ava = case_when(
    nephropathy_ava==1|hba1c_ava==1|bmi_ava==1|bp_ava==1|ldl_ava==1 ~ 1,
    TRUE ~ 0)) %>% 
  arrange(PatientDurableKey,period)

na_summary <- long_df %>%
  summarise(across(everything(), ~sum(is.na(.))))


saveRDS(long_df,paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS"))


long_df %>%  
  group_by(period, ap_type) %>%  
  summarize(sum_N_enc = sum(N_enc, na.rm = TRUE))



