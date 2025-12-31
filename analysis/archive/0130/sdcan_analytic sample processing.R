rm(list=ls());gc();source(".Rprofile")


sdcdat201 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))

# sdccoh05. INSURANCE BEFORE EXPOSURE ---------
insurance_sdc <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh05"),partitioning = c("Year")) %>% 
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
              dplyr::select(PatientDurableKey,earliest_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(earliest_datekey >= INSYearMonthKey) %>% 
  group_by(PatientDurableKey,earliest_datekey) %>% 
  dplyr::filter(INSYearMonthKey == max(INSYearMonthKey)) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(INS_ymd = ymd(INSYearMonthKey),
         AAP_ymd = ymd(earliest_datekey)) %>% 
  mutate(diff_insurance_months = as.numeric(difftime(AAP_ymd,INS_ymd,units = "days"))/30.5) %>% 
  dplyr::filter(diff_insurance_months <= 2) %>% 
  dplyr::select(-INS_ymd,-AAP_ymd)


analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01_analytic dataset.RDS")) %>% 
  mutate(age = as.numeric(difftime(earliest_ymd,BirthDate,units="days")/365.25)) %>% 
  left_join(insurance_sdc %>% 
              select("PatientDurableKey","insurance_medicare","insurance_medicaid","insurance_other","insurance_selfpay") %>% 
              distinct(PatientDurableKey, .keep_all=TRUE),
            by=c("PatientDurableKey"))



# No T2D ---------------------------------------------------------------------------------------------------------------------------

decdat03 <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) 

insurance_dec <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05"),partitioning = c("Year")) %>% 
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
  collect()


nodm <- decdat03 %>% 
  select("PatientDurableKey","hba1c","fpg","ValidatedStateOrProvince_X","raceeth","female","age",
         "diagnosis_datekey","diagnosis_date","BirthDate","PrimaryRUCA_X","count_minus2y","count_minus1y") %>% 
  dplyr::filter(!PatientDurableKey %in% analytic_dataset$PatientDurableKey) %>% 
  left_join(insurance_dec %>% 
              select("PatientDurableKey","insurance_medicare","insurance_medicaid","insurance_other","insurance_selfpay") %>% 
              distinct(PatientDurableKey, .keep_all=TRUE),
            by=c("PatientDurableKey"))



analytic_sample <- bind_rows(analytic_dataset %>% 
                               mutate(type = case_when(type == "SGA" ~ "SGA",
                                                       TRUE ~ "FGA")),
                             nodm) %>% 
  mutate(exposure_binary = case_when(is.na(type) ~ 0,
                                     TRUE ~ 1),
         exposure_category = case_when(type == "FGA" ~ 3,
                                       type == "SGA" ~ 2,
                                       TRUE ~ 1
         )) %>% 
  mutate(exposure_category = factor(exposure_category,
                                    levels=c(1:3),
                                    labels=c("None","SGA","FGA"))) %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black","Hispanic","NH Other")),
         SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100) %>% 
  mutate(region = case_when(PrimaryRUCA_X %in% as.character(c(1:6)) ~ "Urban",
                            PrimaryRUCA_X %in% as.character(c(7:10)) ~ "Rural",
                            TRUE ~ "Urban")) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam")) %>% 
  mutate(across(starts_with("insurance_"), ~replace_na(., 0))) %>% 
  group_by(ValidatedStateOrProvince_X,raceeth) %>%
  mutate(SviOverallPctlRankByZip2020_X_imputed = if_else(is.na(SviOverallPctlRankByZip2020_X),
                                                         median(SviOverallPctlRankByZip2020_X, na.rm = TRUE),
                                                         SviOverallPctlRankByZip2020_X)) %>%
  ungroup() %>% 
  dplyr::filter(!is.na(SviOverallPctlRankByZip2020_X_imputed))


saveRDS(analytic_sample,paste0(path_sga_dm_control_folder,"/working/analytic sample with covariates.RDS"))


# diagnosis data
sdccoh04a <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh04a"),partitioning = c("Year","Value_Grouper2")) %>% 
  # head(1000) %>% 
  collect()

sdccoh04b <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
  # head(1000) %>% 
  collect()

# Diabetic nephropathy: E11.2, nephropathy: E11.3, peripheral neuropathy: E11.4

diagnosis <- sdccoh04a %>% 
  # dplyr::filter(Value_Grouper2 == "E11")
  dplyr::filter(ICD10_Value %in% c("E11.2","E11.3","E11.4"))













