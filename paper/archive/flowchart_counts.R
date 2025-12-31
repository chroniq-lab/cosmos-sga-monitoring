rm(list=ls());gc();source(".Rprofile")

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_analytic dataset with AP type.RDS"))


df <- analytic_sample %>% 
  
  mutate(hba1c_Y1_ava = case_when(!is.na(mean_hba1c_Y1) ~ 1,
                                  TRUE ~ 0),
         hba1c_Y2_ava = case_when(!is.na(mean_hba1c_Y2) ~ 1,
                                  TRUE ~ 0),
         hba1c_Y3_ava = case_when(!is.na(mean_hba1c_Y3) ~ 1,
                                  TRUE ~ 0),
         bmi_Y1_ava = case_when(!is.na(mean_bmi_Y1) ~ 1,
                                TRUE ~ 0),
         bmi_Y2_ava = case_when(!is.na(mean_bmi_Y2) ~ 1,
                                TRUE ~ 0),
         bmi_Y3_ava = case_when(!is.na(mean_bmi_Y3) ~ 1,
                                TRUE ~ 0),
         bp_Y1_ava = case_when(!is.na(mean_sbp_Y1)&!is.na(mean_dbp_Y1) ~ 1,
                               TRUE ~ 0),
         bp_Y2_ava = case_when(!is.na(mean_sbp_Y2)&!is.na(mean_dbp_Y2) ~ 1,
                               TRUE ~ 0),
         bp_Y3_ava = case_when(!is.na(mean_sbp_Y3)&!is.na(mean_dbp_Y3) ~ 1,
                               TRUE ~ 0),
         ldl_Y1_ava = case_when(!is.na(mean_ldl_Y1) ~ 1,
                                TRUE ~ 0),
         ldl_Y2_ava = case_when(!is.na(mean_ldl_Y2) ~ 1,
                                TRUE ~ 0),
         ldl_Y3_ava = case_when(!is.na(mean_ldl_Y3) ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(
    nephropathy_Y1_ava = case_when(!is.na(nephropathy_Y1)|!is.na(mean_uacr_Y1)|!is.na(mean_egfr_Y1) ~ 1,
                                   TRUE ~ 0),
    nephropathy_Y2_ava = case_when(!is.na(nephropathy_Y2)|!is.na(mean_uacr_Y2)|!is.na(mean_egfr_Y2) ~ 1,
                                   TRUE ~ 0),
    nephropathy_Y3_ava = case_when(!is.na(nephropathy_Y1)|!is.na(mean_uacr_Y3)|!is.na(mean_egfr_Y3) ~ 1,
                                   TRUE ~ 0),
    retinopathy_Y1_ava = case_when(!is.na(retinopathy_Y1) ~ 1,
                                   TRUE ~ 0),
    retinopathy_Y2_ava = case_when(!is.na(retinopathy_Y2) ~ 1,
                                   TRUE ~ 0),
    retinopathy_Y3_ava = case_when(!is.na(retinopathy_Y3) ~ 1,
                                   TRUE ~ 0),
    neuropathy_Y1_ava = case_when(!is.na(neuropathy_Y1) ~ 1,
                                  TRUE ~ 0),
    neuropathy_Y2_ava = case_when(!is.na(neuropathy_Y2) ~ 1,
                                  TRUE ~ 0),
    neuropathy_Y3_ava = case_when(!is.na(neuropathy_Y3) ~ 1,
                                  TRUE ~ 0))


abc_Y1 <- df %>% 
  dplyr::filter(hba1c_Y1_ava==1 & bmi_Y1_ava==1 & bp_Y1_ava==1 & ldl_Y1_ava==1) 

abc_Y2 <- df %>% 
  dplyr::filter(hba1c_Y2_ava==1 & bmi_Y2_ava==1 & bp_Y2_ava==1 & ldl_Y2_ava==1) 

abc_Y3 <- df %>% 
  dplyr::filter(hba1c_Y3_ava==1 & bmi_Y3_ava==1 & bp_Y3_ava==1 & ldl_Y3_ava==1) 

abc_all <- df %>% 
  dplyr::filter(hba1c_Y1_ava==1 & bmi_Y1_ava==1 & bp_Y1_ava==1 & ldl_Y1_ava==1 &
                  hba1c_Y2_ava==1 & bmi_Y2_ava==1 & bp_Y2_ava==1 & ldl_Y2_ava==1 &
                  hba1c_Y3_ava==1 & bmi_Y3_ava==1 & bp_Y3_ava==1 & ldl_Y3_ava==1) 

cpl_Y1 <- df %>% 
  dplyr::filter(nephropathy_Y1_ava==1 & retinopathy_Y1_ava==1 & neuropathy_Y1_ava==1) 

cpl_Y2 <- df %>% 
  dplyr::filter(nephropathy_Y2_ava==1 & retinopathy_Y2_ava==1 & neuropathy_Y2_ava==1) 

cpl_Y3 <- df %>% 
  dplyr::filter(nephropathy_Y3_ava==1 & retinopathy_Y3_ava==1 & neuropathy_Y3_ava==1) 

cpl_all <- df %>% 
  dplyr::filter(nephropathy_Y1_ava==1 & retinopathy_Y1_ava==1 & neuropathy_Y1_ava==1 &
                  nephropathy_Y2_ava==1 & retinopathy_Y2_ava==1 & neuropathy_Y2_ava==1 &
                  nephropathy_Y3_ava==1 & retinopathy_Y3_ava==1 & neuropathy_Y3_ava==1) 

cpl_any <- df %>% 
  dplyr::filter(nephropathy_Y1_ava==1 | retinopathy_Y1_ava==1 | neuropathy_Y1_ava==1 |
                  nephropathy_Y2_ava==1 | retinopathy_Y2_ava==1 | neuropathy_Y2_ava==1 |
                  nephropathy_Y3_ava==1 | retinopathy_Y3_ava==1 | neuropathy_Y3_ava==1) 

cpl_Y2 %>% 
  summarise(UniquePatientKeys = n_distinct(PatientDurableKey))

cpl_Y2 %>% 
  group_by(ap_type) %>% 
  summarise(UniquePatientKeys = n_distinct(PatientDurableKey))






