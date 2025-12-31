rm(list=ls());gc();source(".Rprofile")

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with AP type.RDS"))

dx_list <- list()

for (iter in 1:15) {
deccoh09a <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09a/Iteration=",iter),partitioning = c("Iteration","Year","Value_Grouper2")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         dx = case_when(
           # 17 in Charlson comorbidity index
           Value_Grouper %in% myocardial_dx_codes ~ "myocardial_infarction",
           ICD10_Value %in% myocardial_dx_codes ~ "myocardial_infarction",
           ICD10_Value %in% heart_dx_codes ~ "heart_failure",
           ICD10_Value %in% pvd_dx_codes ~ "peripheral_vascular",
           Value_Grouper %in% cerebro_dx_codes ~ "cerebrovascular",
           ICD10_Value %in% cerebro_dx_codes ~ "cerebrovascular",
           Value_Grouper %in% dementia_dx_codes ~ "dementia",
           ICD10_Value %in% dementia_dx_codes ~ "dementia",
           Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
           ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
           Value_Grouper %in% rheumatic_dx_codes ~ "rheumatic",
           ICD10_Value %in% rheumatic_dx_codes ~ "rheumatic",
           Value_Grouper %in% pepticulcer_dx_codes  ~ "peptic_ulcer",
           Value_Grouper %in% mildliver_dx_codes ~ "mild_liver",
           ICD10_Value %in% mildliver_dx_codes ~ "mild_liver",
           ICD10_Value %in% dm_dx_codes ~ "diabetes",
           ICD10_Value %in% dmcpl_dx_codes ~ "diabetes_complication", # NA
           str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
           str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
           str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
           ICD10_Value %in% hemiplegia_dx_codes ~ "hemiplegia",
           ICD10_Value %in% renal_dx_codes ~ "renal",
           Value_Grouper %in% malignancy_dx_codes ~ "malignancy",
           ICD10_Value %in% modliver_dx_codes ~ "modsev_liver",
           Value_Grouper %in% tumor_dx_codes ~ "metastatic_tumor",
           Value_Grouper %in% hiv_dx_codes ~ "aids_hiv",
           
           # 7 mental health
           Value_Grouper %in% schizophrenia_dx_codes  ~ "schizophrenia",
           Value_Grouper %in% schizoptypal_dx_codes ~ "schizotypal",
           Value_Grouper %in% delusional_dx_codes ~ "delusional",
           Value_Grouper %in% schizoaffective_dx_codes ~ "schizoaffective",
           Value_Grouper %in% bipolar_manic_dx_codes ~ "bipolar_manic",
           Value_Grouper %in% depressive_mdd_dx_codes ~ "depressive_mdd",
           Value_Grouper %in% psychosis_unspecified_dx_codes ~ "psychosis_unspecified",
           
           # 6 other
           Value_Grouper %in% htn_dx_codes ~ "htn",
           Value_Grouper %in% hld_dx_codes ~ "hld",
           Value_Grouper %in% cardiovascular_dx_codes ~ "cardiovascular",
           ICD10_Value %in% obesity_dx_codes ~ "obesity",
           ICD10_Value %in% mafld_dx_codes ~ "mafld", # NA
           ICD10_Value %in% pcos_dx_codes ~ "pcos",
           
           TRUE ~ NA_character_)) %>% 
  
  dplyr::filter(!is.na(dx)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>%      
  inner_join(analytic_sample %>% 
              dplyr::select(PatientDurableKey,exposure_datekey) %>% 
              mutate(PatientDurableKey = as.numeric(PatientDurableKey)),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  collect()

deccoh09b <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09b/Iteration=",iter),partitioning = c("Iteration","Year","Value_Grouper2")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         dx = case_when(
           # 17 in Charlson comorbidity index
           Value_Grouper %in% myocardial_dx_codes ~ "myocardial_infarction",
           ICD10_Value %in% myocardial_dx_codes ~ "myocardial_infarction",
           ICD10_Value %in% heart_dx_codes ~ "heart_failure",
           ICD10_Value %in% pvd_dx_codes ~ "peripheral_vascular",
           Value_Grouper %in% cerebro_dx_codes ~ "cerebrovascular",
           ICD10_Value %in% cerebro_dx_codes ~ "cerebrovascular",
           Value_Grouper %in% dementia_dx_codes ~ "dementia",
           ICD10_Value %in% dementia_dx_codes ~ "dementia",
           Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
           ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
           Value_Grouper %in% rheumatic_dx_codes ~ "rheumatic",
           ICD10_Value %in% rheumatic_dx_codes ~ "rheumatic",
           Value_Grouper %in% pepticulcer_dx_codes  ~ "peptic_ulcer",
           Value_Grouper %in% mildliver_dx_codes ~ "mild_liver",
           ICD10_Value %in% mildliver_dx_codes ~ "mild_liver",
           ICD10_Value %in% dm_dx_codes ~ "diabetes",
           ICD10_Value %in% dmcpl_dx_codes ~ "diabetes_complication", # NA
           str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
           str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
           str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
           ICD10_Value %in% hemiplegia_dx_codes ~ "hemiplegia",
           ICD10_Value %in% renal_dx_codes ~ "renal",
           Value_Grouper %in% malignancy_dx_codes ~ "malignancy",
           ICD10_Value %in% modliver_dx_codes ~ "modsev_liver",
           Value_Grouper %in% tumor_dx_codes ~ "metastatic_tumor",
           Value_Grouper %in% hiv_dx_codes ~ "aids_hiv",
           
           # 7 mental health
           Value_Grouper %in% schizophrenia_dx_codes  ~ "schizophrenia",
           Value_Grouper %in% schizoptypal_dx_codes ~ "schizotypal",
           Value_Grouper %in% delusional_dx_codes ~ "delusional",
           Value_Grouper %in% schizoaffective_dx_codes ~ "schizoaffective",
           Value_Grouper %in% bipolar_manic_dx_codes ~ "bipolar_manic",
           Value_Grouper %in% depressive_mdd_dx_codes ~ "depressive_mdd",
           Value_Grouper %in% psychosis_unspecified_dx_codes ~ "psychosis_unspecified",
           
           # 6 other
           Value_Grouper %in% htn_dx_codes ~ "htn",
           Value_Grouper %in% hld_dx_codes ~ "hld",
           Value_Grouper %in% cardiovascular_dx_codes ~ "cardiovascular",
           ICD10_Value %in% obesity_dx_codes ~ "obesity",
           ICD10_Value %in% mafld_dx_codes ~ "mafld", # NA
           ICD10_Value %in% pcos_dx_codes ~ "pcos",
           
           TRUE ~ NA_character_)) %>% 
  
  dplyr::filter(!is.na(dx)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>%      
  inner_join(analytic_sample %>% 
               dplyr::select(PatientDurableKey,exposure_datekey) %>% 
               mutate(PatientDurableKey = as.numeric(PatientDurableKey)),
             by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  collect()

billed_dx_before <- bind_rows(deccoh09a,deccoh09b) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  dplyr::filter(exposure_datekey - YearMonthKey <= 2) %>% 
  dplyr::filter(YearMonthKey <= exposure_datekey) %>% 
  distinct(PatientDurableKey,exposure_datekey,dx)


dx_list[[iter]] = billed_dx_before


}


billed_codes_before <- bind_rows(dx_list)


write_parquet(billed_codes_before,paste0(path_sga_dm_control_folder,"/working/sdcan01/billing codes before exposure date.parquet"))


