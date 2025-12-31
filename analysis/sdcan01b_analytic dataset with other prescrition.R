rm(list=ls());gc();source(".Rprofile")

sdcan01 <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01/sdcan01_analytic dataset.RDS"))

decdat03 <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam")) %>% 
  # restricting to individuals diagnosed with T2D between January 2012 and December 2021 
  dplyr::filter(diagnosis_datekey >= "20120100" & diagnosis_datekey <= "20211200")

decdat03 %>% dim()

# ANY/CLOSEST PRESCRIPTION AFTER T2D -------------

closest_prescriptions_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions after diagnosis date.parquet")) %>% 
  dplyr::select(PatientDurableKey,YearMonthKey,YearMonth,diagnosis_datekey) %>% 
  left_join(decdat03 %>% 
              select("PatientDurableKey","diagnosis_datekey","diagnosis_date"),
            by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = as.numeric(YearMonthKey)) %>% 
  mutate(YearMonthKey_ymd = ymd(YearMonthKey)) %>% 
  mutate(diff_ANYP_DM = abs(as.numeric(difftime(diagnosis_date,YearMonthKey_ymd,units = "days")))) %>% 
  # restrict to 2y after T2D
  dplyr::filter(diff_ANYP_DM <= 730) %>%
  group_by(PatientDurableKey,diagnosis_datekey) %>% 
  dplyr::filter(diff_ANYP_DM == min(diff_ANYP_DM)) %>% 
  slice(1) %>%  
  ungroup() 


analytic_dataset <- decdat03 %>% 
  inner_join(closest_prescriptions_after %>% 
              rename(ANYPYearMonthKey_ymd = YearMonthKey_ymd,
                     ANYPYearMonthKey = YearMonthKey,
                     ANYPYearMonth = YearMonth),
            by = c("PatientDurableKey","diagnosis_datekey","diagnosis_date")) %>% 
  # exclude SGA/FGA
  dplyr::filter(!PatientDurableKey %in% sdcan01$PatientDurableKey) %>%
  mutate(ANYPYearMonthKey_plus1y = ANYPYearMonthKey + 10000,
         ANYPYearMonthKey_plus2y = ANYPYearMonthKey + 20000,
         ANYPYearMonthKey_plus3y = ANYPYearMonthKey + 30000)

saveRDS(analytic_dataset,paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

rm(closest_prescriptions_after);gc()

# Check 
# summary(analytic_dataset$diff_ANYP_DM)


# COVARIATES ----------

# INSURANCE BEFORE EXPOSURE ---------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

deccoh05 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  mutate(insurance_medicare = case_when(CoverageFinancialClass == "Medicare" ~ 1,
                                        TRUE ~ 0),
         insurance_other = case_when(CoverageFinancialClass == "Miscellaneous/Other" ~ 1,
                                     TRUE ~ 0),
         insurance_medicaid = case_when(CoverageFinancialClass == "Medicaid" ~ 1,
                                        TRUE ~ 0),
         insurance_selfpay = case_when(CoverageFinancialClass == "Self-Pay" ~ 1,
                                       TRUE ~ 0)) %>% 
  group_by(PatientDurableKey,YearMonth,YearMonthKey) %>% 
  summarize(insurance_medicare = sum(insurance_medicare),
            insurance_other = sum(insurance_other),
            insurance_medicaid = sum(insurance_medicaid),
            insurance_selfpay = sum(insurance_selfpay)) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  collect() 


deccoh05b <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  mutate(insurance_medicare = case_when(CoverageFinancialClass == "Medicare" ~ 1,
                                        TRUE ~ 0),
         insurance_other = case_when(CoverageFinancialClass == "Miscellaneous/Other" ~ 1,
                                     TRUE ~ 0),
         insurance_medicaid = case_when(CoverageFinancialClass == "Medicaid" ~ 1,
                                        TRUE ~ 0),
         insurance_selfpay = case_when(CoverageFinancialClass == "Self-Pay" ~ 1,
                                       TRUE ~ 0)) %>% 
  group_by(PatientDurableKey,YearMonth,YearMonthKey) %>% 
  summarize(insurance_medicare = sum(insurance_medicare),
            insurance_other = sum(insurance_other),
            insurance_medicaid = sum(insurance_medicaid),
            insurance_selfpay = sum(insurance_selfpay)) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>% 
  collect()
  
  
insurance_before <- bind_rows(deccoh05,deccoh05b) %>% 
  dplyr::filter(YearMonthKey <= ANYPYearMonthKey) %>%
  group_by(PatientDurableKey,ANYPYearMonthKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  ungroup() 

write_parquet(insurance_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/insurance before exposure date with other prescription.parquet"))

rm(insurance_before,deccoh05,deccoh05b);gc()


# HISTORICAL VARIABLES -------------------------------------

# VITALS 2y BEFORE EXPOSURE ---------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

# deccoh01. BMI ---------------

deccoh01a <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey) %>% 
              mutate(ANYPYearMonthKey_minus1y = ANYPYearMonthKey - 10000,
                     ANYPYearMonthKey_minus2y = ANYPYearMonthKey - 20000),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  collect()

deccoh01b <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey) %>% 
              mutate(ANYPYearMonthKey_minus1y = ANYPYearMonthKey - 10000,
                     ANYPYearMonthKey_minus2y = ANYPYearMonthKey - 20000),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  collect()

bmi_before <- bind_rows(deccoh01a,deccoh01b) %>% 
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  # mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
  #                              TRUE ~ Weight*0.453592),
  #        Height_cm = Height*2.54) %>% 
  mutate(period = case_when(YearMonthKey < ANYPYearMonthKey & YearMonthKey >= ANYPYearMonthKey_minus1y ~ "Y1_prior",
                            YearMonthKey < ANYPYearMonthKey_minus1y & YearMonthKey >= ANYPYearMonthKey_minus2y ~ "Y2_prior",
                            TRUE ~ "Y2 or earlier")) %>% 
  dplyr::filter(period %in% c("Y1_prior","Y2_prior"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,BodyMassIndex) %>% 
  rename(bmi_history = BodyMassIndex)

write_parquet(bmi_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/bmi 2y before exposure date with other prescription.parquet"))

rm(bmi_before,deccoh01a,deccoh01b);gc()



# DIAGNOSIS BEFORE EXPOSURE -------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

deccoh04a <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04a"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         dx = case_when(Value_Grouper %in% htn_dx_codes ~ "htn",
                        Value_Grouper %in% hld_dx_codes ~ "hld",
                        ICD10_Value %in% cerebro_dx_codes ~ "cerebro",
                        Value_Grouper %in% cardiovascular_dx_codes ~ "cardiovascular",
                        Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
                        ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
                        ICD10_Value %in% obesity_dx_codes ~ "obesity",
                        ICD10_Value %in% mafld_dx_codes ~ "mafld",
                        ICD10_Value %in% pcos_dx_codes ~ "pcos",
                        
                        Value_Grouper %in% schizophrenia_dx_codes  ~ "schizophrenia",
                        Value_Grouper %in% schizoptypal_dx_codes ~ "schizotypal",
                        Value_Grouper %in% delusional_dx_codes ~ "delusional",
                        Value_Grouper %in% schizoaffective_dx_codes ~ "schizoaffective",
                        Value_Grouper %in% bipolar_manic_dx_codes ~ "bipolar_manic",
                        Value_Grouper %in% depressive_mdd_dx_codes ~ "depressive_mdd",
                        Value_Grouper %in% psychosis_unspecified_dx_codes ~ "psychosis_unspecified",
                        TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(dx)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>%      
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  collect()


deccoh04b <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         dx = case_when(Value_Grouper %in% htn_dx_codes ~ "htn",
                        Value_Grouper %in% hld_dx_codes ~ "hld",
                        ICD10_Value %in% cerebro_dx_codes ~ "cerebro",
                        Value_Grouper %in% cardiovascular_dx_codes ~ "cardiovascular",
                        Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
                        ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
                        ICD10_Value %in% obesity_dx_codes ~ "obesity",
                        ICD10_Value %in% mafld_dx_codes ~ "mafld",
                        ICD10_Value %in% pcos_dx_codes ~ "pcos",
                        
                        Value_Grouper %in% schizophrenia_dx_codes  ~ "schizophrenia",
                        Value_Grouper %in% schizoptypal_dx_codes ~ "schizotypal",
                        Value_Grouper %in% delusional_dx_codes ~ "delusional",
                        Value_Grouper %in% schizoaffective_dx_codes ~ "schizoaffective",
                        Value_Grouper %in% bipolar_manic_dx_codes ~ "bipolar_manic",
                        Value_Grouper %in% depressive_mdd_dx_codes ~ "depressive_mdd",
                        Value_Grouper %in% psychosis_unspecified_dx_codes ~ "psychosis_unspecified",
                        TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(dx)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>%      
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  collect()

diagnosis_before <- bind_rows(deccoh04a,deccoh04b) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  dplyr::filter(ANYPYearMonthKey - YearMonthKey <= 2) %>% 
  dplyr::filter(YearMonthKey <= ANYPYearMonthKey) %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,dx)

write_parquet(diagnosis_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/diagnosis before exposure date with other prescription.parquet"))

rm(diagnosis_before,deccoh04a,deccoh04b);gc()


# COMORBIDITY RX BEFORE EXPOSURE -------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

comorbidity_rx_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass")) %>% 
  mutate(rx = case_when(str_detect(PharmaceuticalClass,"ANTIDEPRES") ~ "antidepressant",
                        
                        PharmaceuticalClass %in% c("SSRI, SEROTONIN RECEPTOR MODULATOR ANTIDEPRESSANTS",
                                                   "TRICYCLIC ANTIDEPRESSANT-PHENOTHIAZINE COMBINATNS",
                                                   "SEROTONIN-NOREPINEPHRINE REUPTAKE-INHIB (SNRIS)",
                                                   "SELECTIVE SEROTONIN INHIB. (SSRIS)/DIET SUPP CMB.",
                                                   "MAOIS - A SELECTIVE AND REVERSIBLE (RIMA)",
                                                   "SSRI-ANTIPSYCH, ATYPICAL,DOPAMINE,SEROTONIN ANTAG",
                                                   "MONOAMINE OXIDASE (MAO) INHIBITOR ANTIDEPRESSANTS",
                                                   "ANTIDEPRESSANTS",
                                                   "TCA-ANTIPSYCHOTIC,DOPAMINE ANTAG.THIOXANTHENE CMBS",
                                                   "SELECTIVE NOREPINEPHRINE REUPTAKE INHIB (NRI)",
                                                   "TRICYCLIC ANTIDEPRESSANTS,REL.NON-SEL.REUPT-INHIB",
                                                   "ANTIDEPRESSANT - POSTPARTUM DEPRESSION (PPD)",
                                                   "ALPHA-2 RECEPTOR ANTAGONIST ANTIDEPRESSANTS",
                                                   "TRICYCLIC ANTIDEPRESSANT-BENZODIAZEPINE COMBINATNS",
                                                   "ANTIDEPRESSANT - NMDA RECEPTOR ANTAGONIST",
                                                   "SEROTONIN-2 ANTAGONIST/REUPTAKE INHIBITORS (SARIS)",
                                                   "MAOIS -NON-SELECTIVE,IRREVERSIBLE ANTIDEPRESSANTS",
                                                   "SEROTONIN-2 ANTAG,REUPTAKE INH/DIETARY SUPP. COMB.",
                                                   "NOREPINEPHRINE AND DOPAMINE REUPTAKE INHIB (NDRIS)",
                                                   "SELECTIVE SEROTONIN REUPTAKE INHIBITOR (SSRIS)",
                                                   "SSRI AND 5HT1A PARTIAL AGONIST ANTIDEPRESSANTS",
                                                   "SELECTIVE SEROTONIN 5-HT2A INVERSE AGONISTS (SSIA)") ~ "antidepressant",
                        
                        # str_detect(PharmaceuticalSubClass,'Antidepres') ~ "antidepressant",
                        str_detect(PharmaceuticalClass,'Antidepres') ~ "antidepressant",
                        str_detect(PharmaceuticalClass,"ANTIPSYCH") &
                          !str_detect(PharmaceuticalClass,"ATYP") ~ "other_antipsychotic",
                        # str_detect(PharmaceuticalClass,"ANTIPSYCH") &
                        #   !str_detect(PharmaceuticalSubClass,"Atypical") ~ "other_antipsychotic",
                        str_detect(PharmaceuticalClass,"ANTIHYPERTENSIVE") ~ "antihypertensive",
                        str_detect(PharmaceuticalClass,"STATIN") ~ "statin",
                        str_detect(PharmaceuticalClass,"ANTIHYPERLIPID") &
                          !str_detect(PharmaceuticalClass,"STATIN") ~ "other_antihyperlipid",
                        str_detect(PharmaceuticalClass,"ANTIEPILE") ~ "antiepileptic",
                        TRUE ~ NA_character_
  )) %>% 
  dplyr::filter(!is.na(rx)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         RXYearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(RXYearMonthKey_ymd = ymd(RXYearMonthKey),
         RXYearMonthKey_ymd_plusmonths = case_when(is.na(Months) ~ ymd(RXYearMonthKey),
                                                   TRUE ~ ymd(RXYearMonthKey) + dmonths(Months) + ddays(15))) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_ymd),
            by = c("PatientDurableKey")) %>% 
  mutate(
    ANYPYearMonthKey_ymd_minus12mo = ANYPYearMonthKey_ymd- dmonths(13) + ddays(15),
    ANYPYearMonthKey_ymd_plus2mo = ANYPYearMonthKey_ymd + dmonths(2) + ddays(15),
  ) %>%
  mutate(include = case_when(
    (ANYPYearMonthKey_ymd_minus12mo >= RXYearMonthKey_ymd &
       ANYPYearMonthKey_ymd_minus12mo <= RXYearMonthKey_ymd_plusmonths & 
       ANYPYearMonthKey_ymd >= RXYearMonthKey_ymd_plusmonths) ~ 1,
    
    (ANYPYearMonthKey_ymd_minus12mo <= RXYearMonthKey_ymd &
       ANYPYearMonthKey_ymd >= RXYearMonthKey_ymd_plusmonths) ~ 2,
    
    (ANYPYearMonthKey_ymd_minus12mo <= RXYearMonthKey_ymd & 
       ANYPYearMonthKey_ymd >= RXYearMonthKey_ymd &
       ANYPYearMonthKey_ymd <= RXYearMonthKey_ymd_plusmonths) ~ 3,
    
    (ANYPYearMonthKey_ymd_minus12mo >= RXYearMonthKey_ymd &
       ANYPYearMonthKey_ymd <= RXYearMonthKey_ymd_plusmonths) ~ 4,
    TRUE ~ NA_real_
    
  )) %>%   
  dplyr::filter(!is.na(include)) %>% 
  mutate(include = as.numeric(include)) %>% 
  collect() 


closest_comorbidity_rx_before <- comorbidity_rx_before %>% 
  mutate(diff_ANYP_RX = abs(as.numeric(difftime(ANYPYearMonthKey,RXYearMonthKey_ymd,units = "days")))) %>% 
  dplyr::select(-contains("ymd")) %>% 
  # distinct(PatientDurableKey,RXYearMonthKey,earliest_datekey,TherapeuticClass,PharmaceuticalClass,SimpleGenericName,type_RX,.keep_all = TRUE) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,rx) %>% 
  mutate(RXgenericnames = paste0("(",paste0(SimpleGenericName,collapse=";"),")"),
         RXprescribedmonths = paste0("(",paste0(RXYearMonthKey,collapse = ";"),")"),
         n_wlrx = n()) %>% 
  # Take the closest rx for each earliest_datekey
  # This may have same RXYearMonthKey mapped to different earliest_datekey
  dplyr::filter(diff_ANYP_RX == min(diff_ANYP_RX)) %>% 
  # If 2 RXYearMonthKey are equidistant from an earliest_datekey, take any one of them
  slice(1) %>% 
  ungroup() %>% 
  arrange(PatientDurableKey,ANYPYearMonthKey) %>% 
  group_by(PatientDurableKey,RXYearMonthKey,rx) %>% 
  mutate(rx_duplicate = n(),
         index = 1:n()) %>% 
  ungroup()

unique_comorbidity_rx_before <- comorbidity_rx_before %>%   
  group_by(PatientDurableKey,RXYearMonthKey,rx) %>% 
  summarize(n = n(),
            include = paste0(include,collapse=";"),
            APprescribedmonths = paste0(ANYPYearMonthKey,collapse=";"),
            RXgenericnames = paste0(SimpleGenericName,collapse=";")) %>% 
  ungroup() %>% 
  rename(n_aapevents = n) 


write_parquet(closest_comorbidity_rx_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/closest_comorbidity prescriptions before exposure date with other prescription.parquet"))
write_parquet(unique_comorbidity_rx_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/unique_comorbidity prescriptions before exposure date with other prescription.parquet"))

rm(comorbidity_rx_before,closest_comorbidity_rx_before,unique_comorbidity_rx_before);gc()



# PRESCRIPTIONS BEFORE EXPOSURE ------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

deccoh03a <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03a"),partitioning = c("Year")) %>% 
  mutate(type = case_when(PharmaceuticalClass %in% c("INSULINS") ~ "insulin",
                          PharmaceuticalClass %in% c("ANTIHYPERGLYCEMIC, BIGUANIDE TYPE") ~ "biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,INSULIN-RELEASE STIM.-BIGUANIDE') ~ "sulfonylurea;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, DPP-4 INHIBITORS') ~ "dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, INSULIN-RELEASE STIMULANT TYPE') ~ "sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,THIAZOLIDINEDIONE(PPARG AGONIST)') ~ "tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,DPP-4 INHIBITOR-BIGUANIDE COMBS.') ~ "dpp4i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INCRETIN MIMETIC(GLP-1 RECEP.AGONIST)') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, AMYLIN ANALOG-TYPE') ~ "amylin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE AND BIGUANIDE') ~ "tzd;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, ALPHA-GLUCOSIDASE INHIBITORS') ~ "agi",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - DOPAMINE RECEPTOR AGONISTS') ~ "dopamine",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE-SULFONYLUREA') ~ "tzd;sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SOD/GLUC COTRANSPORT2(SGLT2) INH') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY. DPP-4 INHIBITORS-HMG COA RI(STATINS)') ~ "dpp4i;statin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,DPP-4 ENZYME INHIB.-THIAZOLIDINEDIONE') ~ "dpp4i;tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SGLT2 INHIBITOR-BIGUANIDE COMBS.') ~ "sglt2i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, SGLT-2 AND DPP-4 INHIBITOR COMB') ~ "sglt2i;dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-GLUCOCORTICOID RECEPTOR BLOCKER') ~ "glucocorticoid",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INSULIN,LONG ACT-GLP-1 RECEPT.AGONIST') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY-SGLT-2 INHIB,DPP-4 INHIB,BIGUANIDE CB') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - INCRETIN MIMETICS COMBINATION') ~ "glpgip",
                          TRUE ~ NA_character_),
  ) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb() %>%
  collect()

deccoh03b <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>% 
  mutate(type = case_when(PharmaceuticalClass %in% c("INSULINS") ~ "insulin",
                          PharmaceuticalClass %in% c("ANTIHYPERGLYCEMIC, BIGUANIDE TYPE") ~ "biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,INSULIN-RELEASE STIM.-BIGUANIDE') ~ "sulfonylurea;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, DPP-4 INHIBITORS') ~ "dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, INSULIN-RELEASE STIMULANT TYPE') ~ "sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,THIAZOLIDINEDIONE(PPARG AGONIST)') ~ "tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,DPP-4 INHIBITOR-BIGUANIDE COMBS.') ~ "dpp4i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INCRETIN MIMETIC(GLP-1 RECEP.AGONIST)') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, AMYLIN ANALOG-TYPE') ~ "amylin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE AND BIGUANIDE') ~ "tzd;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, ALPHA-GLUCOSIDASE INHIBITORS') ~ "agi",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - DOPAMINE RECEPTOR AGONISTS') ~ "dopamine",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE-SULFONYLUREA') ~ "tzd;sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SOD/GLUC COTRANSPORT2(SGLT2) INH') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY. DPP-4 INHIBITORS-HMG COA RI(STATINS)') ~ "dpp4i;statin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,DPP-4 ENZYME INHIB.-THIAZOLIDINEDIONE') ~ "dpp4i;tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SGLT2 INHIBITOR-BIGUANIDE COMBS.') ~ "sglt2i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, SGLT-2 AND DPP-4 INHIBITOR COMB') ~ "sglt2i;dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-GLUCOCORTICOID RECEPTOR BLOCKER') ~ "glucocorticoid",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INSULIN,LONG ACT-GLP-1 RECEPT.AGONIST') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY-SGLT-2 INHIB,DPP-4 INHIB,BIGUANIDE CB') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - INCRETIN MIMETICS COMBINATION') ~ "glpgip",
                          TRUE ~ NA_character_),
  ) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb() %>%
  collect()

prescriptions_before <- bind_rows(deccoh03a,
                                  deccoh03b) %>% 
  dplyr::filter(YearMonthKey <= ANYPYearMonthKey) %>% 
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,type)

write_parquet(prescriptions_before,paste0(path_sga_dm_control_folder,"/working/sdcan01b/prescriptions before exposure date with other prescription.parquet"))

rm(prescriptions_before,deccoh03a,deccoh03b);gc()



# SOURCES ON MONTH OF EXPOSURE ---------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

sources_exposure <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh06b"),partitioning = c("Year","SourceKey"))  %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"00")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonth) %>% 
              mutate(ANYPYearMonthKey = paste0(ANYPYearMonth,"00")),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey == ANYPYearMonthKey) %>% 
  distinct(PatientDurableKey,YearMonthKey,SourceKey) %>% 
  ungroup() %>% 
  collect()

write_parquet(sources_exposure,paste0(path_sga_dm_control_folder,"/working/sdcan01b/sources on month of exposure date with other prescription.parquet"))

rm(sources_exposure);gc()



# PRESCRIBERS OF EXPOSURE --------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

deccoh10 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass")) %>% 
  dplyr::filter(TherapeuticClass == "PSYCHOTHERAPEUTIC DRUGS") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         ProviderYearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(ProviderYearMonthKey = as.numeric(ProviderYearMonthKey)) %>% 
  mutate(type = case_when(
    grepl("ANTIPSYCHOTIC", PharmaceuticalClass, ignore.case = TRUE) & grepl("ATYPICAL", PharmaceuticalClass, ignore.case = TRUE) ~ "SGA",
    grepl("ANTIPSYCHOTIC", PharmaceuticalClass, ignore.case = TRUE) & !grepl("ATYP", PharmaceuticalClass, ignore.case = TRUE) ~ "FGA",
    TRUE ~ "Neither"
  )) %>% 
  mutate(Psychiatry = case_when(str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Behavior") ~ 1,
                                str_detect(SecondSpecialty,"Behavior") ~ 1,
                                str_detect(PrimarySpecialty,"Mental\\s") ~ 1,
                                str_detect(SecondSpecialty,"Mental\\s") ~ 1,
                                TRUE ~ 0),
         PrimaryCare = case_when(PrimarySpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                         "Nurse Practitioner","General Practice","Primary Care",
                                                         "Geriatric Medicine","Diabetes Services","Gerontology",
                                                         "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 SecondSpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                        "Nurse Practitioner","General Practice","Primary Care",
                                                        "Geriatric Medicine","Diabetes Services","Gerontology",
                                                        "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 TRUE ~ 0),
         UnspecifiedSpecialty = case_when(str_detect(PrimarySpecialty,"Unspecified") ~ 1,
                                          TRUE ~ 0),
         AvailableSpecialty = case_when(str_detect(PrimarySpecialty,"Unspecified") ~ 0,
                                        TRUE ~ 1)) %>% 
  dplyr::filter(type == "Neither") %>% 
  collect()                                        


prescribers <- analytic_dataset %>% 
  select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_ymd) %>% 
  inner_join(deccoh10,
             by=c("PatientDurableKey" = "PatientDurableKey","ANYPYearMonthKey"="ProviderYearMonthKey")) %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_ymd,
           type,SimpleGenericName,PrimarySpecialty,SecondSpecialty,
           .keep_all= TRUE) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,type) %>% 
  summarize(Psychiatry = sum(Psychiatry), # Number of times someone with a Psychiatry specialty prescribed type_sga
            PrimaryCare = sum(PrimaryCare), # Number of times someone with a PrimaryCare/IntMed specialty prescribed type_sga
            UnspecifiedSpecialty = sum(UnspecifiedSpecialty),
            AvailableSpecialty = sum(AvailableSpecialty),
            genericnames = paste0("(",paste0(SimpleGenericName,collapse=";"),")")) %>% 
  ungroup() %>% 
  arrange(PatientDurableKey,ANYPYearMonthKey,type) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey) %>% 
  mutate(genericnames_concat = paste0(genericnames,collapse="||"),
         order = 1:n()) %>% 
  ungroup() %>% 
  dplyr::filter(order == 1) %>% 
  mutate(Other = case_when(PrimaryCare == 0 & Psychiatry == 0 ~ 1,
                           TRUE ~ 0)) 


write_parquet(prescribers,paste0(path_sga_dm_control_folder,"/working/sdcan01b/prescribers on exposure date with other prescription.parquet"))

rm(prescribers,deccoh10);gc()



# Outcome -------------------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

## deccoh02ab. HbA1c -----------
hba1c <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(hba1c)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_hba1c <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(hba1c)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_hba1c = mean(hba1c,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_hba1c)


# sdccoh01a. BMI ---------------
bmi <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_bmi <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y3 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_bmi = mean(BodyMassIndex,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_bmi)


## deccoh01b. Blood Pressure ------------
blood_pressure = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_blood_pressure = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_sbp = mean(SystolicBloodPressure),
            mean_dbp = mean(DiastolicBloodPressure)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=c(mean_sbp,mean_dbp))


## deccoh02fb. Ldlc -----------
ldl <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(ldl_value = case_when(NumericValue >= 10 & NumericValue <= 500 ~ NumericValue,
                               TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(ldl_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_ldl <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(ldl_value = case_when(NumericValue >= 10 & NumericValue <= 500 ~ NumericValue,
                               TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(ldl_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_ldl = mean(ldl_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_ldl)


after <- analytic_dataset %>% 
  select(PatientDurableKey,ANYPYearMonthKey) %>% 
  left_join(hba1c %>% 
              rename(n_hba1c_Y1 = Y1,
                     n_hba1c_Y2 = Y2,
                     n_hba1c_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(mean_hba1c %>% 
              rename(mean_hba1c_Y1 = Y1,
                     mean_hba1c_Y2 = Y2,
                     mean_hba1c_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(bmi %>% 
              rename(n_bmi_Y1 = Y1,
                     n_bmi_Y2 = Y2,
                     n_bmi_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(mean_bmi %>% 
              rename(mean_bmi_Y1 = Y1,
                     mean_bmi_Y2 = Y2,
                     mean_bmi_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(blood_pressure %>% 
              rename(n_bp_Y1 = Y1,
                     n_bp_Y2 = Y2,
                     n_bp_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(mean_blood_pressure ,
            by = c("PatientDurableKey")) %>% 
  left_join(ldl %>% 
              rename(n_ldl_Y1 = Y1,
                     n_ldl_Y2 = Y2,
                     n_ldl_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(mean_ldl %>% 
              rename(mean_ldl_Y1 = Y1,
                     mean_ldl_Y2 = Y2,
                     mean_ldl_Y3 = Y3),
            by = c("PatientDurableKey")) 

write_parquet(after,paste0(path_sga_dm_control_folder,"/working/sdcan01b/abc outcomes after exposure date with other prescription.parquet"))
rm(hba1c,mean_hba1c,bmi,mean_bmi,blood_pressure,mean_blood_pressure,ldl,mean_ldl);gc()



# COMPLICATIONS DIAGNOSIS AFTER EXPOSURE ------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

complications_rx_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(type = case_when(str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
                          # str_detect(ICD10_Value,"(N18)\\.(4|5|6)") ~ "ckd",
                          str_detect(ICD10_Value,"(I25)\\.(10)") ~ "chd",
                          str_detect(ICD10_Value,"(I48)\\.(0|1|2|3|4|9)") ~ "afib",
                          str_detect(ICD10_Value,"(G45)\\.(0|1|8|9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I60)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I61)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I63)\\.(30|40)") ~ "stroke",
                          str_detect(ICD10_Value,"(I67)\\.(89)") ~ "stroke",
                          str_detect(ICD10_Value,"(I73)\\.(0|1|8|9)") ~ "pad",
                          str_detect(ICD10_Value,"(I50)\\.(30|31)") ~ "hfpef",
                          str_detect(ICD10_Value,"(I50)\\.(21|22)") ~ "hfref",
                          Value_Grouper %in% htn_dx_codes ~ "htn",
                          Value_Grouper %in% hld_dx_codes ~ "hld",
                          ICD10_Value %in% cerebro_dx_codes ~ "other_cerebro",
                          Value_Grouper %in% cardiovascular_dx_codes ~ "other_cardiovascular",
                          Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% obesity_dx_codes ~ "obesity",
                          ICD10_Value %in% mafld_dx_codes ~ "mafld",
                          ICD10_Value %in% pcos_dx_codes ~ "pcos",
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey >= ANYPYearMonthKey) %>% 
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,type) %>% 
  collect()


write_parquet(complications_rx_after,paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications diagnosis after exposure date with other prescription.parquet"))

rm(complications_rx_after);gc()



# COMPLICATIONS SCREENING AFTER EXPOSURE ------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

complications_test_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07ba"),partitioning = c("Year")) %>% 
  mutate(type = case_when(CptCode %in% nephropathy_codes ~ "nephropathy",
                          CptCode %in% retinopathy_codes ~ "retinopathy",
                          CptCode %in% neuropathy_codes ~ "neuropathy",
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3")) %>% 
  mutate(type_period = paste(type, period, sep = "_")) %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,type_period) %>% 
  collect()


write_parquet(complications_test_after,paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications screening after exposure date with other prescription.parquet"))

rm(complications_test_after);gc()


mean_uacr <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02mb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(uacr_value = NumericValue) %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(uacr_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_uacr = mean(uacr_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_uacr)


mean_egfr <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02nb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(egfr_value = NumericValue) %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(egfr_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_egfr = mean(egfr_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_egfr)


mean_egfr2 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ob"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(egfr2_value = NumericValue) %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(egfr2_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_egfr2 = mean(egfr2_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_egfr2)


complications_lab_after <- mean_uacr %>% 
  rename(mean_uacr_Y1 = Y1,
         mean_uacr_Y2 = Y2,
         mean_uacr_Y3 = Y3) %>% 
  left_join(mean_egfr %>% 
              rename(mean_egfr_Y1 = Y1,
                     mean_egfr_Y2 = Y2,
                     mean_egfr_Y3 = Y3),
            by = c("PatientDurableKey")) %>% 
  left_join(mean_egfr2 %>% 
              rename(mean_egfr2_Y1 = Y1,
                     mean_egfr2_Y2 = Y2,
                     mean_egfr2_Y3 = Y3),
            by = c("PatientDurableKey")) %>%
  left_join(analytic_dataset %>% 
              dplyr::select(PatientDurableKey,ANYPYearMonthKey),
            by = c("PatientDurableKey"))


write_parquet(complications_lab_after,paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications lab screening after exposure date with other prescription.parquet"))

rm(complications_lab_after,mean_uacr,mean_egfr);gc()



# ENCOUNTERS AFTER EXPOSURE -------------

analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS"))

# Followup 1 year - T2D diagnosis
deccoh08c <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh08c"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey),
         YearMonthKey = paste0(Year,"0101")) %>% 
  inner_join(analytic_dataset %>% 
               dplyr::select(PatientDurableKey,diagnosis_datekey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
             by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(N_enc)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,ANYPYearMonthKey,period,N_enc) %>% 
  collect()

# Followup 2 year - T2D diagnosis
deccoh08d <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh08d"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey),
         YearMonthKey = paste0(Year,"0101")) %>% 
  inner_join(analytic_dataset %>% 
               dplyr::select(PatientDurableKey,diagnosis_datekey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
             by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(N_enc)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,ANYPYearMonthKey,period,N_enc) %>% 
  collect()

# Followup 3 year - T2D diagnosis
deccoh08e <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh08e"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey),
         YearMonthKey = paste0(Year,"0101")) %>% 
  inner_join(analytic_dataset %>% 
               dplyr::select(PatientDurableKey,diagnosis_datekey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
             by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(N_enc)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,ANYPYearMonthKey,period,N_enc) %>% 
  collect()

# Followup 4 year - T2D diagnosis
deccoh08f <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh08f"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey),
         YearMonthKey = paste0(Year,"0101")) %>% 
  inner_join(analytic_dataset %>% 
               dplyr::select(PatientDurableKey,diagnosis_datekey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
             by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(N_enc)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,ANYPYearMonthKey,period,N_enc) %>% 
  collect()

# Followup 5 year - T2D diagnosis
deccoh08g <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh08g"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey),
         YearMonthKey = paste0(Year,"0101")) %>% 
  inner_join(analytic_dataset %>% 
               dplyr::select(PatientDurableKey,diagnosis_datekey,ANYPYearMonthKey,ANYPYearMonthKey_plus1y,ANYPYearMonthKey_plus2y,ANYPYearMonthKey_plus3y),
             by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= ANYPYearMonthKey & YearMonthKey < ANYPYearMonthKey_plus1y ~ "Y1",
                            YearMonthKey >= ANYPYearMonthKey_plus1y & YearMonthKey < ANYPYearMonthKey_plus2y ~ "Y2",
                            YearMonthKey >= ANYPYearMonthKey_plus2y & YearMonthKey < ANYPYearMonthKey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(N_enc)) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,ANYPYearMonthKey,period,N_enc) %>% 
  collect()


encounters_after <- bind_rows(deccoh08c,
                              deccoh08d,
                              deccoh08e,
                              deccoh08f,
                              deccoh08g) %>%
  arrange(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  group_by(PatientDurableKey,ANYPYearMonthKey,period) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>% 
  pivot_wider(names_from=period,values_from=N_enc) %>% 
  rename(N_enc_Y1 = Y1,
         N_enc_Y2 = Y2,
         N_enc_Y3 = Y3) %>% 
  distinct(PatientDurableKey,ANYPYearMonthKey,.keep_all = TRUE)


write_parquet(encounters_after,paste0(path_sga_dm_control_folder,"/working/sdcan01b/encounters after exposure date with other prescription.parquet"))

rm(encounters_after,deccoh08c,deccoh08d,deccoh08e,deccoh08f,deccoh08g);gc()



