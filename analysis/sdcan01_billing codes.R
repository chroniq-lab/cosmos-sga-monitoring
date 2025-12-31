rm(list=ls());gc();source(".Rprofile")

library(stringr)

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with AP type.RDS"))

dx_list <- list()

for (iter in 1:15) {
deccoh09a <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09a/Iteration=",iter),partitioning = c("Iteration","Year","Value_Grouper2")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         dx = case_when(
           # 17 in Charlson comorbidity index
           Value_Grouper %in% myocardial_dx_codes ~ "myocardial_infarction",
           str_detect(ICD10_Value, myocardial_dx_pattern) ~ "myocardial_infarction",
           str_detect(ICD10_Value, heart_dx_pattern) ~ "heart_failure",
           str_detect(ICD10_Value, pvd_dx_pattern) ~ "peripheral_vascular",
           Value_Grouper %in% cerebro_dx_codes ~ "cerebrovascular",
           str_detect(ICD10_Value, cerebro_dx_pattern) ~ "cerebrovascular",
           Value_Grouper %in% dementia_dx_codes ~ "dementia",
           str_detect(ICD10_Value, dementia_dx_pattern) ~ "dementia",
           Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
           str_detect(ICD10_Value, pulmonary_dx_pattern) ~ "pulmonary",
           Value_Grouper %in% rheumatic_dx_codes ~ "rheumatic",
           str_detect(ICD10_Value, rheumatic_dx_pattern) ~ "rheumatic",
           Value_Grouper %in% pepticulcer_dx_codes  ~ "peptic_ulcer",
           Value_Grouper %in% mildliver_dx_codes ~ "mild_liver",
           str_detect(ICD10_Value, mildliver_dx_pattern) ~ "mild_liver",
           str_detect(ICD10_Value, dm_dx_pattern) ~ "diabetes",
           str_detect(ICD10_Value, dmcpl_dx_pattern) ~ "diabetes_complication", # NA
           str_detect(ICD10_Value, hemiplegia_dx_pattern) ~ "hemiplegia",
           str_detect(ICD10_Value, renal_dx_pattern) ~ "renal",
           Value_Grouper %in% malignancy_dx_codes ~ "malignancy",
           str_detect(ICD10_Value, modliver_dx_pattern) ~ "modsev_liver",
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
           str_detect(ICD10_Value, obesity_dx_pattern) ~ "obesity",
           str_detect(ICD10_Value, mafld_dx_pattern) ~ "mafld", # NA
           str_detect(ICD10_Value, pcos_dx_pattern) ~ "pcos",
           
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
           str_detect(ICD10_Value, myocardial_dx_pattern) ~ "myocardial_infarction",
           str_detect(ICD10_Value, heart_dx_pattern) ~ "heart_failure",
           str_detect(ICD10_Value, pvd_dx_pattern) ~ "peripheral_vascular",
           Value_Grouper %in% cerebro_dx_codes ~ "cerebrovascular",
           str_detect(ICD10_Value, cerebro_dx_pattern) ~ "cerebrovascular",
           Value_Grouper %in% dementia_dx_codes ~ "dementia",
           str_detect(ICD10_Value, dementia_dx_pattern) ~ "dementia",
           Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
           str_detect(ICD10_Value, pulmonary_dx_pattern) ~ "pulmonary",
           Value_Grouper %in% rheumatic_dx_codes ~ "rheumatic",
           str_detect(ICD10_Value, rheumatic_dx_pattern) ~ "rheumatic",
           Value_Grouper %in% pepticulcer_dx_codes  ~ "peptic_ulcer",
           Value_Grouper %in% mildliver_dx_codes ~ "mild_liver",
           str_detect(ICD10_Value, mildliver_dx_pattern) ~ "mild_liver",
           str_detect(ICD10_Value, dm_dx_pattern) ~ "diabetes",
           str_detect(ICD10_Value, dmcpl_dx_pattern) ~ "diabetes_complication", # NA
           str_detect(ICD10_Value, hemiplegia_dx_pattern) ~ "hemiplegia",
           str_detect(ICD10_Value, renal_dx_pattern) ~ "renal",
           Value_Grouper %in% malignancy_dx_codes ~ "malignancy",
           str_detect(ICD10_Value, modliver_dx_pattern) ~ "modsev_liver",
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
           str_detect(ICD10_Value, obesity_dx_pattern) ~ "obesity",
           str_detect(ICD10_Value, mafld_dx_pattern) ~ "mafld", # NA
           str_detect(ICD10_Value, pcos_dx_pattern) ~ "pcos",
           
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


