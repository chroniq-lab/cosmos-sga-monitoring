rm(list=ls());gc();source(".Rprofile")


bmi_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/bmi 2y before exposure date with AP.parquet"))

insurance_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/insurance before exposure date with AP.parquet")) %>% 
  mutate(INS_ymd = ymd(INSYearMonthKey)) %>% 
  mutate(diff_insurance_months = as.numeric(difftime(earliest_ymd,INS_ymd,units = "days"))/30.5) %>% 
  dplyr::filter(diff_insurance_months <= 2) %>% 
  dplyr::select(-INS_ymd,-earliest_ymd)

# diagnosis_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/diagnosis before exposure date with AP.parquet")) %>% 
#   dplyr::select(PatientDurableKey,earliest_datekey,dx) %>% 
#   mutate(value = 1) %>% 
#   pivot_wider(names_from=dx,values_from = value,values_fill = 0)

comorbidity_rx_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/closest_comorbidity prescriptions before sga.parquet")) %>% 
  dplyr::select(PatientDurableKey,earliest_datekey,rx) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=rx,values_from = value,values_fill = 0)

prescribers <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/prescribers on exposure date with AP.parquet")) %>% 
  select(-genericnames,-genericnames_concat,-order)

# complications_rx_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/complications diagnosis after exposure date with AP.parquet")) %>% 
#   dplyr::select(PatientDurableKey,earliest_datekey,type) %>% 
#   mutate(value = 1) %>% 
#   pivot_wider(names_from=type,values_from = value,values_fill = 0)

complications_test_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/complications screening after exposure date with AP.parquet")) %>% 
  dplyr::select(PatientDurableKey,earliest_datekey,type_period) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=type_period,values_from = value,values_fill = NA_real_)

encounters_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01a/encounters after exposure date with AP.parquet"))



# PROCESSING ----------


analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01/sdcan01_analytic dataset.RDS")) %>% 
  left_join(bmi_before,
            by=c("PatientDurableKey","earliest_datekey")) %>% 
  left_join(insurance_before,
            by=c("PatientDurableKey","earliest_datekey")) %>% 
  # left_join(diagnosis_before %>% 
  #             rename_with(~ paste0(., "_dx"), .cols = -c(PatientDurableKey, earliest_datekey)),
  #           by=c("PatientDurableKey","earliest_datekey")) %>% 
  left_join(comorbidity_rx_before %>% 
              rename_with(~ paste0(., "_rx"), .cols = -c(PatientDurableKey, earliest_datekey)),
            by=c("PatientDurableKey","earliest_datekey")) %>%
  # left_join(sources_exposure,
  #           by=c("PatientDurableKey","earliest_datekey")) %>%
  left_join(prescribers,
            by=c("PatientDurableKey","earliest_datekey","type")) %>% 
  # left_join(complications_rx_after %>% 
  #             rename_with(~ paste0(., "_cplrx"), .cols = -c(PatientDurableKey, earliest_datekey)),
  #           by=c("PatientDurableKey","earliest_datekey")) %>%
  left_join(complications_test_after,
            by=c("PatientDurableKey","earliest_datekey")) %>%
  left_join(encounters_after,
            by=c("PatientDurableKey","earliest_datekey")) %>%
  mutate(across(c(ends_with(c("_rx")),
                  one_of(c("insurance_medicare", "insurance_medicaid", "insurance_other", "insurance_selfpay",
                           "Psychiatry", "PrimaryCare", "UnspecifiedSpecialty", "AvailableSpecialty","Other"))),
                ~ case_when(is.na(.) ~ 0, 
                            TRUE ~ .))) %>% 
  mutate(ap_type = case_when(type == "Not_SGA" ~ "FGA",
                             TRUE ~ "SGA")) %>% 
  mutate(age = as.numeric(difftime(earliest_ymd,BirthDate,units="days")/365.25),
         year = as.character(earliest_datekey %/% 10000)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black","Hispanic","NH Other")),
         SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100) %>% 
  mutate(region = case_when(PrimaryRUCA_X %in% as.character(c(1:6)) ~ "Urban",
                            PrimaryRUCA_X %in% as.character(c(7:10)) ~ "Rural",
                            TRUE ~ "Urban")) %>% 
  group_by(ValidatedStateOrProvince_X,raceeth) %>%
  # impute NA as median value
  mutate(SviOverallPctlRankByZip2020_X_imputed = if_else(is.na(SviOverallPctlRankByZip2020_X),
                                                         median(SviOverallPctlRankByZip2020_X, na.rm = TRUE),
                                                         SviOverallPctlRankByZip2020_X)) %>%
  ungroup() %>% 
  dplyr::filter(!is.na(SviOverallPctlRankByZip2020_X_imputed)) %>% 
  rename(diff_exposure_DM = diff_AP_DM)


na_summarya <- analytic_sample %>%
  summarise(across(everything(), ~sum(is.na(.))))

saveRDS(analytic_sample,paste0(path_sga_dm_control_folder,"/working/sdcan01a/sdcan01a_analytic dataset with AP.RDS"))



