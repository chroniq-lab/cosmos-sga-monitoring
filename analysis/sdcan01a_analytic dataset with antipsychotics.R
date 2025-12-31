rm(list=ls());gc();source(".Rprofile")

# people with SGA|FGA

# T2D patients meeting SGA or other antipsychotic criteria with SUPREME-DM
sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

# INSURANCE BEFORE EXPOSURE ---------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

insurance_before <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh05"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(insurance_medicare = case_when(CoverageFinancialClass == "Medicare" ~ 1,
                                        TRUE ~ 0),
         insurance_other = case_when(CoverageFinancialClass == "Miscellaneous/Other" ~ 1,
                                     TRUE ~ 0),
         insurance_medicaid = case_when(CoverageFinancialClass == "Medicaid" ~ 1,
                                        TRUE ~ 0),
         insurance_selfpay = case_when(CoverageFinancialClass == "Self-Pay" ~ 1,
                                       TRUE ~ 0),
         INSYearMonthKey = paste0(YearMonth,"01")) %>% 
  group_by(PatientDurableKey,INSYearMonthKey) %>% 
  summarize(insurance_medicare = sum(insurance_medicare),
            insurance_other = sum(insurance_other),
            insurance_medicaid = sum(insurance_medicaid),
            insurance_selfpay = sum(insurance_selfpay)) %>% 
  ungroup() %>% 
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_ymd),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(INSYearMonthKey <= earliest_datekey) %>% 
  group_by(PatientDurableKey,earliest_datekey) %>% 
  dplyr::filter(INSYearMonthKey == max(INSYearMonthKey)) %>% 
  ungroup() %>% 
  collect()

write_parquet(insurance_before,paste0(path_sga_dm_control_folder,"/working/sdcan01a/insurance before exposure date with AP.parquet"))

rm(insurance_before);gc()

# HISTORICAL VARIABLES -----------------------------------------------------------------------------------------------------------

# VITALS 2y BEFORE EXPOSURE ---------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

# sdccoh01a. BMI ---------------
bmi_before <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey) %>% 
              mutate(earliest_datekey_minus1y = earliest_datekey - 10000,
                     earliest_datekey_minus2y = earliest_datekey - 20000),
            by = c("PatientDurableKey")) %>%
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  # mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
  #                              TRUE ~ Weight*0.453592),
  #        Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey < earliest_datekey & YearMonthKey >= earliest_datekey_minus1y ~ "Y1_prior",
                            YearMonthKey < earliest_datekey_minus1y & YearMonthKey >= earliest_datekey_minus2y ~ "Y2_prior",
                            TRUE ~ "Y2 or earlier")) %>% 
  dplyr::filter(period %in% c("Y1_prior","Y2_prior"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,earliest_datekey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,earliest_datekey,BodyMassIndex) %>% 
  collect() %>% 
  rename(bmi_history = BodyMassIndex)

write_parquet(bmi_before,paste0(path_sga_dm_control_folder,"/working/sdcan01a/bmi 2y before exposure date with AP.parquet"))

rm(bmi_before);gc()




# DIAGNOSIS BEFORE EXPOSURE -------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

diagnosis_before <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh04a"),partitioning = c("Year","Value_Grouper2")) %>% 
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
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey) %>% 
              mutate(PatientDurableKey = as.numeric(PatientDurableKey)),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey <= earliest_datekey) %>% 
  distinct(PatientDurableKey,earliest_datekey,dx) %>% 
  collect()

write_parquet(diagnosis_before,paste0(path_sga_dm_control_folder,"/working/sdcan01a/diagnosis before exposure date with AP.parquet"))

rm(diagnosis_before);gc()



# COMORBIDITY RX BEFORE EXPOSURE -------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

comorbidity_rx_before <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass")) %>% 
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
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_ymd),
            by = c("PatientDurableKey")) %>% 
  mutate(
    earliest_ymd_minus12mo = earliest_ymd- dmonths(13) + ddays(15),
    earliest_ymd_plus2mo = earliest_ymd + dmonths(2) + ddays(15),
  ) %>%
  mutate(include = case_when(
    (earliest_ymd_minus12mo >= RXYearMonthKey_ymd &
       earliest_ymd_minus12mo <= RXYearMonthKey_ymd_plusmonths & 
       earliest_ymd >= RXYearMonthKey_ymd_plusmonths) ~ 1,
    
    (earliest_ymd_minus12mo <= RXYearMonthKey_ymd &
       earliest_ymd >= RXYearMonthKey_ymd_plusmonths) ~ 2,
    
    (earliest_ymd_minus12mo <= RXYearMonthKey_ymd & 
       earliest_ymd >= RXYearMonthKey_ymd &
       earliest_ymd <= RXYearMonthKey_ymd_plusmonths) ~ 3,
    
    (earliest_ymd_minus12mo >= RXYearMonthKey_ymd &
       earliest_ymd <= RXYearMonthKey_ymd_plusmonths) ~ 4,
    TRUE ~ NA_real_
    
  )) %>%   
  dplyr::filter(!is.na(include)) %>% 
  mutate(include = as.numeric(include)) %>% 
  collect() 
 

closest_comorbidity_rx_before <- comorbidity_rx_before %>% 
  mutate(diff_AP_RX = abs(as.numeric(difftime(earliest_ymd,RXYearMonthKey_ymd,units = "days")))) %>% 
  dplyr::select(-contains("ymd")) %>% 
  # distinct(PatientDurableKey,RXYearMonthKey,earliest_datekey,TherapeuticClass,PharmaceuticalClass,SimpleGenericName,type_RX,.keep_all = TRUE) %>% 
  group_by(PatientDurableKey,earliest_datekey,rx) %>% 
  mutate(RXgenericnames = paste0("(",paste0(SimpleGenericName,collapse=";"),")"),
         RXprescribedmonths = paste0("(",paste0(RXYearMonthKey,collapse = ";"),")"),
         n_wlrx = n()) %>% 
  # Take the closest rx for each earliest_datekey
  # This may have same RXYearMonthKey mapped to different earliest_datekey
  dplyr::filter(diff_AP_RX == min(diff_AP_RX)) %>% 
  # If 2 RXYearMonthKey are equidistant from an earliest_datekey, take any one of them
  slice(1) %>% 
  ungroup() %>% 
  arrange(PatientDurableKey,earliest_datekey) %>% 
  group_by(PatientDurableKey,RXYearMonthKey,rx) %>% 
  mutate(rx_duplicate = n(),
         index = 1:n()) %>% 
  ungroup()
  
  
unique_comorbidity_rx_before <- comorbidity_rx_before %>%   
  group_by(PatientDurableKey,RXYearMonthKey,rx) %>% 
  summarize(n = n(),
            include = paste0(include,collapse=";"),
            APprescribedmonths = paste0(earliest_datekey,collapse=";"),
            RXgenericnames = paste0(SimpleGenericName,collapse=";")) %>% 
  ungroup() %>% 
  rename(n_aapevents = n) 


# write_parquet(comorbidity_rx_after,paste0(path_aap_t2dm_folder,"/working/atan001/comorbidity prescriptions after exposure date.parquet"))
write_parquet(closest_comorbidity_rx_before,paste0(path_sga_dm_control_folder,"/working/sdcan01a/closest_comorbidity prescriptions before sga.parquet"))
write_parquet(unique_comorbidity_rx_before,paste0(path_sga_dm_control_folder,"/working/sdcan01a/unique_comorbidity prescriptions before sga.parquet"))

rm(comorbidity_rx_before,closest_comorbidity_rx_before,unique_comorbidity_rx_before);gc()



# SOURCES ON MONTH OF EXPOSURE ---------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

sources_exposure <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh06b"),partitioning = c("Year","SourceKey"))  %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"00")) %>% 
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey) %>% 
              mutate(earliest_YearMonthKey = earliest_datekey - 1),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey == earliest_YearMonthKey) %>% 
  distinct(PatientDurableKey,YearMonthKey,SourceKey) %>% 
  ungroup() %>% 
  collect()

write_parquet(sources_exposure,paste0(path_sga_dm_control_folder,"/working/sdcan01a/sources on month of exposure date with AP.parquet"))

rm(sources_exposure);gc()



# PRESCRIBERS OF EXPOSURE --------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

sdccoh10 <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass")) %>% 
  dplyr::filter(TherapeuticClass == "PSYCHOTHERAPEUTIC DRUGS") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         ProviderYearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(ProviderYearMonthKey = as.numeric(ProviderYearMonthKey)) %>% 
  mutate(type = case_when(
    grepl("ANTIPSYCHOTIC", PharmaceuticalClass, ignore.case = TRUE) & grepl("ATYPICAL", PharmaceuticalClass, ignore.case = TRUE) ~ "SGA",
    grepl("ANTIPSYCHOTIC", PharmaceuticalClass, ignore.case = TRUE) & !grepl("ATYP", PharmaceuticalClass, ignore.case = TRUE) ~ "Not_SGA",
    TRUE ~ NA_character_
  )) %>% 
  mutate(Psychiatry = case_when(str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(SecondSpecialty,"Psychiatry") ~ 1,
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
  dplyr::filter(!is.na(type)) %>% 
  collect()


prescribers <- sdcdat201 %>% 
  select(PatientDurableKey,earliest_datekey,earliest_ymd,type) %>% 
  inner_join(sdccoh10,
             by=c("PatientDurableKey" = "PatientDurableKey","earliest_datekey"="ProviderYearMonthKey","type" = "type")) %>% 
  distinct(PatientDurableKey,earliest_datekey,earliest_ymd,
           type,SimpleGenericName,PrimarySpecialty,SecondSpecialty,
           .keep_all= TRUE) %>% 
  group_by(PatientDurableKey,earliest_datekey,type) %>% 
  summarize(Psychiatry = sum(Psychiatry), # Number of times someone with a Psychiatry specialty prescribed type_sga
            PrimaryCare = sum(PrimaryCare), # Number of times someone with a PrimaryCare/IntMed specialty prescribed type_sga
            UnspecifiedSpecialty = sum(UnspecifiedSpecialty),
            AvailableSpecialty = sum(AvailableSpecialty),
            genericnames = paste0("(",paste0(SimpleGenericName,collapse=";"),")")) %>% 
  ungroup() %>% 
  mutate(type = factor(type,levels=c("SGA","Not_SGA"))) %>% 
  arrange(PatientDurableKey,earliest_datekey,type) %>% 
  group_by(PatientDurableKey,earliest_datekey) %>% 
  mutate(genericnames_concat = paste0(genericnames,collapse="||"),
         order = 1:n()) %>% 
  ungroup() %>% 
  dplyr::filter(order == 1) %>% 
  mutate(Other = case_when(PrimaryCare == 0 & Psychiatry == 0 ~ 1,
                           TRUE ~ 0)) 


write_parquet(prescribers,paste0(path_sga_dm_control_folder,"/working/sdcan01a/prescribers on exposure date with AP.parquet"))

rm(prescribers,sdccoh10);gc()



# COMPLICATIONS DIAGNOSIS AFTER EXPOSURE ------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

complications_rx_after <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
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
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey >= earliest_datekey) %>% 
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>%  
  distinct(PatientDurableKey,earliest_datekey,type) %>% 
  collect()

write_parquet(complications_rx_after,paste0(path_sga_dm_control_folder,"/working/sdcan01a/complications diagnosis after exposure date with AP.parquet"))

rm(complications_rx_after);gc()




# COMPLICATIONS SCREENING AFTER EXPOSURE ------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

complications_test_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07ba"),partitioning = c("Year")) %>% 
  mutate(type = case_when(CptCode %in% nephropathy_codes ~ "nephropathy",
                          CptCode %in% retinopathy_codes ~ "retinopathy",
                          CptCode %in% neuropathy_codes ~ "neuropathy",
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(sdcdat201 %>% 
              dplyr::select(PatientDurableKey,earliest_datekey) %>% 
              mutate(
                earliest_datekey_plus1y = earliest_datekey + 10000,
                earliest_datekey_plus2y = earliest_datekey + 20000,
                earliest_datekey_plus3y = earliest_datekey + 30000
              ),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3")) %>% 
  mutate(type_period = paste(type, period, sep = "_")) %>% 
  distinct(PatientDurableKey,earliest_datekey,type_period) %>% 
  collect() 

write_parquet(complications_test_after,paste0(path_sga_dm_control_folder,"/working/sdcan01a/complications screening after exposure date with AP.parquet"))

rm(complications_test_after);gc()

  

# ENCOUNTERS AFTER EXPOSURE -------------

sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

# Followup 1 year
sdccoh08c <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh08c"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_datekey = as.numeric(earliest_datekey)) %>% 
  inner_join(sdcdat201 %>% 
               dplyr::select(PatientDurableKey,earliest_datekey),
             by = c("PatientDurableKey","earliest_datekey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,earliest_datekey) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,earliest_datekey,N_enc) %>% 
  collect()
  
# Followup 2 year
sdccoh08d <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh08d"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_datekey = as.numeric(earliest_datekey)) %>% 
  inner_join(sdcdat201 %>% 
               dplyr::select(PatientDurableKey,earliest_datekey),
             by = c("PatientDurableKey","earliest_datekey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,earliest_datekey) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,earliest_datekey,N_enc) %>% 
  collect()

# Followup 3 year
sdccoh08e <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh08e"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_datekey = as.numeric(earliest_datekey)) %>% 
  inner_join(sdcdat201 %>% 
               dplyr::select(PatientDurableKey,earliest_datekey),
             by = c("PatientDurableKey","earliest_datekey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,earliest_datekey) %>% 
  summarise(N_enc = sum(N_enc,na.rm=TRUE),.groups = 'drop') %>%   
  distinct(PatientDurableKey,earliest_datekey,N_enc) %>% 
  collect()


encounters_after <- sdccoh08c %>% 
  rename(N_enc_Y1 = N_enc) %>% 
  left_join(sdccoh08d %>% 
              rename(N_enc_Y2 = N_enc),
            by = c("PatientDurableKey","earliest_datekey")) %>% 
  left_join(sdccoh08e %>% 
              rename(N_enc_Y3 = N_enc),
            by = c("PatientDurableKey","earliest_datekey")) %>% 
  distinct(PatientDurableKey,earliest_datekey,.keep_all = TRUE)

  
write_parquet(encounters_after,paste0(path_sga_dm_control_folder,"/working/sdcan01a/encounters after exposure date with AP.parquet"))

rm(encounters_after,sdccoh08c,sdccoh08d,sdccoh08e);gc() 
  
  
  


