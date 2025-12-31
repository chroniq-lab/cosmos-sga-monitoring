rm(list=ls());gc();source(".Rprofile")

library(gtsummary)

descriptive_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  mutate(Dmagediag = as.numeric(difftime(diagnosis_date,BirthDate,units="days")/365.25),
         diff_exposure_DM_months = as.numeric(diff_exposure_DM/30)) %>% 
  mutate(bmi_history_category = case_when(bmi_history >= 25 & bmi_history < 30 ~ "Overweight",
                                          bmi_history >= 30 ~ "Obese",
                                          TRUE ~ "Underweight or Normal"),
         insurance = case_when(insurance_medicare == 1 ~ "Medicare",
                               insurance_medicaid == 1 ~ "Medicaid",
                               insurance_other == 1 ~ "Other",
                               insurance_selfpay == 1 ~ "Selfpay",
                               TRUE ~ NA_character_)) %>% 
  mutate(ap_type_wgr = factor(ap_type_wgr,
                              levels=c("None","low_SGA","intermediate_SGA","high_SGA",
                                       "low_FGA","intermediate_FGA","high_FGA"),
                              labels=c("None","low_SGA","intermediate_SGA","high_SGA",
                                       "low_FGA","intermediate_FGA","high_FGA")),
         cci_category = factor(cci_category,levels=c("mild","moderate","severe"),labels=c("mild","moderate","severe"))) %>% 
  mutate(any_Psychiatry = case_when(Psychiatry >= 1 ~ 1,
                                    TRUE ~ 0),
         any_PrimaryCare = case_when(PrimaryCare >= 1 ~ 1,
                                     TRUE ~ 0),
         any_OtherSpecialty = case_when(Psychiatry == 0 & PrimaryCare == 0 & AvailableSpecialty >= 1 ~ 1,
                                        TRUE ~ 0),
         UnspecifiedSpecialty = case_when(Psychiatry == 0 & PrimaryCare == 0 & AvailableSpecialty == 0 & UnspecifiedSpecialty >= 1 ~ 1,
                                          TRUE ~ 0)) %>% 
  mutate(prescriber_index = case_when(any_Psychiatry == 1 ~ "Psychiatry",
                                      any_PrimaryCare == 1 ~ "PrimaryCare",
                                      any_OtherSpecialty == 1 ~ "Other",
                                      UnspecifiedSpecialty == 1 ~ "Unspecified",
                                      TRUE ~ NA_character_)) %>% 
  mutate(nephropathy_ava_Y1 = case_when(!is.na(nephropathy_Y1)|!is.na(mean_uacr_Y1)|!is.na(mean_egfr_Y1)|!is.na(mean_egfr2_Y1) ~ 1,
                                     TRUE ~ 0),
         nephropathy_ava_Y2 = case_when(!is.na(nephropathy_Y2)|!is.na(mean_uacr_Y2)|!is.na(mean_egfr_Y2)|!is.na(mean_egfr2_Y2) ~ 1,
                                     TRUE ~ 0),
         nephropathy_ava_Y3 = case_when(!is.na(nephropathy_Y3)|!is.na(mean_uacr_Y3)|!is.na(mean_egfr_Y3)|!is.na(mean_egfr2_Y3) ~ 1,
                                     TRUE ~ 0),
         neuropathy_ava_Y1 = case_when(!is.na(neuropathy_Y1) ~ 1,
                                    TRUE ~ 0),
         neuropathy_ava_Y2 = case_when(!is.na(neuropathy_Y2) ~ 1,
                                       TRUE ~ 0),
         neuropathy_ava_Y3 = case_when(!is.na(neuropathy_Y3) ~ 1,
                                       TRUE ~ 0),
         retinopathy_ava_Y1 = case_when(!is.na(retinopathy_Y1) ~ 1,
                                     TRUE ~ 0),
         retinopathy_ava_Y2 = case_when(!is.na(retinopathy_Y2) ~ 1,
                                        TRUE ~ 0),
         retinopathy_ava_Y3 = case_when(!is.na(retinopathy_Y3) ~ 1,
                                        TRUE ~ 0))


  (descriptives <- descriptive_df %>% 
      tbl_summary(by=ap_type_wgr,
                  include = c(female,raceeth,Dmagediag,age,region,
                              SviOverallPctlRankByZip2020_X_imputed,
                              
                              N_enc_Y1,N_enc_Y2,N_enc_Y3,
                              
                              bmi_history,bmi_history_category,CCI_score,cci_category,
                              
                              mean_hba1c_Y1,mean_hba1c_Y2,mean_hba1c_Y3,mean_bmi_Y1,mean_bmi_Y2,mean_bmi_Y3,
                              mean_sbp_Y1,mean_sbp_Y2,mean_sbp_Y3,mean_dbp_Y1,mean_dbp_Y2,mean_dbp_Y3,
                              mean_ldl_Y1,mean_ldl_Y2,mean_ldl_Y3,
                              
                              nephropathy_ava_Y1,nephropathy_ava_Y2,nephropathy_ava_Y3,
                              neuropathy_ava_Y1,neuropathy_ava_Y2,neuropathy_ava_Y3,
                              retinopathy_ava_Y1,retinopathy_ava_Y2,retinopathy_ava_Y3,
                              
                              insurance_medicare,insurance_medicaid,insurance_other,insurance_selfpay,insurance,
                              
                              diff_exposure_DM_months, diff_exposure_DM,
                              
                              ends_with("_rx"),ends_with("_dx"),
                              
                              any_Psychiatry,any_PrimaryCare,any_OtherSpecialty,UnspecifiedSpecialty,prescriber_index
                              
                  ),
                  missing = "ifany",
                  missing_text = "Missing",
                  type = list(female ~ "dichotomous",raceeth ~ "categorical",
                              SviOverallPctlRankByZip2020_X_imputed ~ "continuous2",
                              
                              N_enc_Y1 ~ "continuous2",N_enc_Y2 ~ "continuous2",N_enc_Y3 ~ "continuous2",
                              
                              Dmagediag ~ "continuous",age ~ "continuous",bmi_history ~ "continuous",CCI_score ~ "continuous2",
                              
                              mean_hba1c_Y1 ~ "continuous",mean_hba1c_Y2 ~ "continuous",mean_hba1c_Y3 ~ "continuous",
                              mean_bmi_Y1 ~ "continuous",mean_bmi_Y2 ~ "continuous",mean_bmi_Y3 ~ "continuous",
                              mean_sbp_Y1 ~ "continuous",mean_sbp_Y2 ~ "continuous",mean_sbp_Y3 ~ "continuous",
                              mean_dbp_Y1 ~ "continuous",mean_dbp_Y2 ~ "continuous",mean_dbp_Y3 ~ "continuous",
                              mean_ldl_Y1 ~ "continuous",mean_ldl_Y2 ~ "continuous",mean_ldl_Y3 ~ "continuous",
                              
                              nephropathy_ava_Y1 ~ "dichotomous",nephropathy_ava_Y2 ~ "dichotomous",nephropathy_ava_Y3 ~ "dichotomous",
                              neuropathy_ava_Y1 ~ "dichotomous",neuropathy_ava_Y2 ~ "dichotomous",neuropathy_ava_Y3 ~ "dichotomous",
                              retinopathy_ava_Y1 ~ "dichotomous",retinopathy_ava_Y2 ~ "dichotomous",retinopathy_ava_Y3 ~ "dichotomous",
                            
                              region ~ "categorical",bmi_history_category ~ "categorical",cci_category ~ "categorical",
                              
                              insurance_medicare ~ "dichotomous",insurance_medicaid ~ "dichotomous",
                              insurance_other ~ "dichotomous",insurance_selfpay ~ "dichotomous",insurance ~ "categorical",
                              
                              diff_exposure_DM_months ~ "continuous2",diff_exposure_DM ~ "continuous2",
                              
                              ends_with("_rx") ~ "dichotomous",ends_with("_dx") ~ "dichotomous",
                              
                              any_Psychiatry ~ "dichotomous",any_PrimaryCare ~ "dichotomous",
                              any_OtherSpecialty ~ "dichotomous",UnspecifiedSpecialty ~ "dichotomous",
                              prescriber_index ~ "categorical"
    
                    
                              
                  ),
                  digits = list(SviOverallPctlRankByZip2020_X_imputed ~ c(1,1,1,1,1),
                                
                                N_enc_Y1 ~ c(1,1,1,1,1),N_enc_Y2 ~ c(1,1,1,1,1),N_enc_Y3 ~ c(1,1,1,1,1),
                                
                                Dmagediag ~ c(1,1),age ~ c(1,1),bmi_history ~ c(1,1),CCI_score ~ c(1,1),
                                
                                mean_hba1c_Y1 ~ c(1,1,1,1,1),mean_hba1c_Y2 ~ c(1,1,1,1,1),mean_hba1c_Y3 ~ c(1,1,1,1,1),
                                mean_bmi_Y1 ~ c(1,1),mean_bmi_Y2 ~ c(1,1),mean_bmi_Y3 ~ c(1,1),
                                mean_sbp_Y1 ~ c(1,1),mean_sbp_Y2 ~ c(1,1),mean_sbp_Y3 ~ c(1,1),
                                mean_dbp_Y1 ~ c(1,1),mean_dbp_Y2 ~ c(1,1),mean_dbp_Y3 ~ c(1,1),
                                mean_ldl_Y1 ~ c(1,1),mean_ldl_Y2 ~ c(1,1),mean_ldl_Y3 ~ c(1,1),
                                
                                raceeth ~ c(1,1), female ~ c(1,1),
                                
                                nephropathy_ava_Y1 ~ c(1,1),nephropathy_ava_Y2 ~ c(1,1),nephropathy_ava_Y3 ~ c(1,1),
                                neuropathy_ava_Y1 ~ c(1,1),neuropathy_ava_Y2 ~ c(1,1),neuropathy_ava_Y3 ~ c(1,1),
                                retinopathy_ava_Y1 ~ c(1,1),retinopathy_ava_Y2 ~ c(1,1),retinopathy_ava_Y3 ~ c(1,1),
                                
                                region ~ c(1,1),bmi_history_category ~ c(1,1),cci_category ~ c(1,1),
                                
                                insurance_medicare ~ c(1,1),insurance_medicaid ~ c(1,1),
                                insurance_other ~ c(1,1),insurance_selfpay ~ c(1,1),insurance ~ c(1,1),
                                
                                ends_with("_rx") ~ c(1,1),ends_with("_dx") ~ c(1,1),
                                
                                any_Psychiatry ~ c(1,1),any_PrimaryCare ~ c(1,1),
                                any_OtherSpecialty ~ c(1,1),UnspecifiedSpecialty ~ c(1,1),
                                prescriber_index ~ c(1,1),
                                
                                diff_exposure_DM_months ~ c(1,1,1,1,1),diff_exposure_DM ~ c(1,1,1,1,1)
                                
                  ),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
      ) %>% 
      add_n() %>% 
      add_overall()) %>%
    as_gt() %>%
    gt::gtsave(filename = "analysis/sdcan03a_descriptive characteristics by AP weight gain risk type.html")


