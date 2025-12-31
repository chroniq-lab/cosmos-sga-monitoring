rm(list=ls());gc();source(".Rprofile")

# check unique medications
# unique_meds <- sdcdat201 %>% 
#   dplyr::filter(type == "SGA") %>% 
#   select(SimpleGenericName) %>%
#   separate_rows(SimpleGenericName, sep = ";") %>%  # split by ";"
#   mutate(SimpleGenericName = str_trim(SimpleGenericName)) %>% 
#   distinct(SimpleGenericName) %>%  
#   arrange(SimpleGenericName)       


sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet")) %>% 
  # restricting to individuals diagnosed with T2D between January 2012 and December 2021 
  dplyr::filter(diagnosis_datekey >= "20120100" & diagnosis_datekey <= "20211200") %>% 
  # classify SGA/FGA by weight gain risk (high,intermediate,low)
  # Ref: Holt 2019 Curr Diab Rep
  mutate(ap_type_wgr = case_when(
    type == "SGA" & SimpleGenericName %in% c("clozapine","olanzapine",
                                             "olanzapine/samidorphan malate","olanzapine pamoate") ~ "high_SGA",
    type == "SGA" & SimpleGenericName %in% c("risperidone","quetiapine fumarate",
                                             "risperidone microspheres","paliperidone",
                                             "paliperidone palmitate") ~ "intermediate_SGA",
    type == "SGA" ~ "low_SGA",
    
    type == "Not_SGA" & SimpleGenericName %in% c("chlorpromazine HCl","thioridazine HCl","promazine HCl") ~ "high_FGA",
    type == "Not_SGA" & SimpleGenericName %in% c("haloperidol","haloperidol lactate","haloperidol decanoate",
                                                 "loxapine","loxapine succinate",
                                                 "molindone HCl","flupentixol di-HCl/melitracen") ~ "intermediate_FGA",
    type == "Not_SGA" ~ "low_FGA",
    TRUE ~ NA_character_
  )) %>% 
  mutate(ap_type_polypharmacy = case_when(type_polypharmacy == "Only_Not_SGA" ~ "Only_FGA",
                                          TRUE ~ type_polypharmacy))

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))

newdm_sga <- sdcdat201 %>%
  dplyr::filter(PatientDurableKey %in% phenotyping_df$PatientDurableKey) %>% 
  mutate(
         earliest_datekey_plus1y = earliest_datekey + 10000,
         earliest_datekey_plus2y = earliest_datekey + 20000,
         earliest_datekey_plus3y = earliest_datekey + 30000
  ) 

# sdccoh02ab. HbA1c ---------------
hba1c <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(hba1c_value = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                                 TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(hba1c_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_hba1c <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(hba1c_value = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                                 TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y3 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(hba1c_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_hba1c = mean(hba1c_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_hba1c)



# sdccoh01b. BMI ---------------
bmi <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_bmi <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(BodyMassIndex = as.numeric(BodyMassIndex)) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y3 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(BodyMassIndex)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_bmi = mean(BodyMassIndex,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_bmi)



# sdccoh01b. Blood Pressure ---------------
blood_pressure <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_blood_pressure <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_sbp = mean(SystolicBloodPressure),
            mean_dbp = mean(DiastolicBloodPressure)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=c(mean_sbp,mean_dbp))




# sdccoh02db. LDL ---------------
ldl <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(ldl_value = case_when(NumericValue >= 10 & NumericValue <= 500 ~ NumericValue,
                               TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(ldl_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  tally() %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=n,values_fill = 0)

mean_ldl <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(newdm_sga %>% 
              dplyr::select(PatientDurableKey,earliest_datekey,earliest_datekey_plus1y, earliest_datekey_plus2y, earliest_datekey_plus3y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  to_duckdb()  %>%
  mutate(ldl_value = case_when(NumericValue >= 10 & NumericValue <= 500 ~ NumericValue,
                               TRUE ~ NA_real_)) %>%
  mutate(period = case_when(YearMonthKey >= earliest_datekey & YearMonthKey < earliest_datekey_plus1y ~ "Y1",
                            YearMonthKey >= earliest_datekey_plus1y & YearMonthKey < earliest_datekey_plus2y ~ "Y2",
                            YearMonthKey >= earliest_datekey_plus2y & YearMonthKey < earliest_datekey_plus3y ~ "Y3",
                            TRUE ~ "Y4 or later")) %>% 
  dplyr::filter(period %in% c("Y1","Y2","Y3"),!is.na(ldl_value)) %>% 
  group_by(PatientDurableKey,period) %>% 
  summarize(mean_ldl = mean(ldl_value,na.rm=TRUE)) %>%   
  ungroup()  %>% 
  collect() %>% 
  pivot_wider(names_from=period,values_from=mean_ldl)


# sdcdat05. SGA Demographics ---------------
sdcdat05 <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat05"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0)) %>% 
  collect()

# sdcdat105. Non SGA Demographics -------------------

sdcdat105 <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat105"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0)) %>% 
  collect()

# analytic_dataset -----------------





analytic_dataset <- bind_rows(sdcdat05,
                              sdcdat105) %>%
  distinct(PatientDurableKey,.keep_all = TRUE) %>% 
  inner_join(newdm_sga,
             by=c("PatientDurableKey")) %>% 
  dplyr::filter(SourceCountry_X == "United States of America") %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam")) %>% 
  mutate(diff_AP_DM = abs(as.numeric(difftime(earliest_ymd,diagnosis_date,units = "days")))) %>% 
  # restrict to 2y after T2D
  dplyr::filter(diff_AP_DM <= 730) %>% 
  dplyr::select(-earliest_datekey_plus1y,-earliest_datekey_plus2y,-earliest_datekey_plus3y,
                -Ethnicity,-FirstRace,-SecondRace,-ThirdRace,
                -Sex,-SexAssignedAtBirth) %>% 
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
            by = c("PatientDurableKey")) %>% 
  collect()


saveRDS(analytic_dataset,paste0(path_sga_dm_control_folder,"/working/sdcan01/sdcan01_analytic dataset.RDS"))

