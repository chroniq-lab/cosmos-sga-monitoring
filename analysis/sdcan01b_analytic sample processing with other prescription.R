rm(list=ls());gc();source(".Rprofile")

abc_outcomes <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/abc outcomes after exposure date with other prescription.parquet"))

bmi_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/bmi 2y before exposure date with other prescription.parquet"))

insurance_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/insurance before exposure date with other prescription.parquet")) %>% 
  mutate(INS_ymd = ymd(YearMonthKey),
         ANYPYearMonthKey_ymd = ymd(ANYPYearMonthKey)) %>% 
  mutate(diff_insurance_months = as.numeric(difftime(ANYPYearMonthKey_ymd,INS_ymd,units = "days"))/30.5) %>% 
  dplyr::filter(diff_insurance_months <= 2) %>% 
  dplyr::select(-INS_ymd,-ANYPYearMonthKey_ymd)

# diagnosis_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/diagnosis before exposure date with other prescription.parquet")) %>% 
#   dplyr::select(PatientDurableKey,ANYPYearMonthKey,dx) %>% 
#   mutate(value = 1) %>% 
#   pivot_wider(names_from=dx,values_from = value,values_fill = 0)

comorbidity_rx_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/closest_comorbidity prescriptions before exposure date with other prescription.parquet")) %>% 
  dplyr::select(PatientDurableKey,ANYPYearMonthKey,rx) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=rx,values_from = value,values_fill = 0)

prescribers <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/prescribers on exposure date with other prescription.parquet")) %>% 
  select(-genericnames,-genericnames_concat,-order)

# complications_rx_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications diagnosis after exposure date with other prescription.parquet")) %>% 
#   dplyr::select(PatientDurableKey,ANYPYearMonthKey,type) %>% 
#   mutate(value = 1) %>% 
#   pivot_wider(names_from=type,values_from = value,values_fill = 0)

complications_test_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications screening after exposure date with other prescription.parquet")) %>% 
  dplyr::select(PatientDurableKey,ANYPYearMonthKey,type_period) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=type_period,values_from = value,values_fill = NA_real_) 

complications_lab_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/complications lab screening after exposure date with other prescription.parquet"))

encounters_after <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01b/encounters after exposure date with other prescription.parquet"))


# PROCESSING ----------------------------------------
# with no SGA|FGA prescription

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_original analytic sample with other prescription.RDS")) %>% 
  left_join(abc_outcomes,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>% 
  left_join(bmi_before,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>% 
  left_join(insurance_before,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>% 
  # left_join(diagnosis_before %>% 
  #             rename_with(~ paste0(., "_dx"), .cols = -c(PatientDurableKey, ANYPYearMonthKey)),
  #           by=c("PatientDurableKey","ANYPYearMonthKey")) %>% 
  left_join(comorbidity_rx_before %>% 
              rename_with(~ paste0(., "_rx"), .cols = -c(PatientDurableKey, ANYPYearMonthKey)),
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  # left_join(sources_exposure,
  #           by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  left_join(prescribers,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>% 
  # left_join(complications_rx_after %>% 
  #             rename_with(~ paste0(., "_cplrx"), .cols = -c(PatientDurableKey, ANYPYearMonthKey)),
  #           by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  left_join(complications_test_after,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  left_join(complications_lab_after,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  left_join(encounters_after,
            by=c("PatientDurableKey","ANYPYearMonthKey")) %>%
  mutate(across(c(ends_with(c("_rx")),
                  one_of(c("insurance_medicare", "insurance_medicaid", "insurance_other", "insurance_selfpay",
                           "Psychiatry", "PrimaryCare", "UnspecifiedSpecialty", "AvailableSpecialty","Other"))),
                ~ case_when(is.na(.) ~ 0, 
                            TRUE ~ .))) %>% 
  mutate(ap_type = "Neither") %>% 
  mutate(year = as.character(ANYPYearMonthKey %/% 10000)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black","Hispanic","NH Other")),
         SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100
  ) %>% 
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
  rename(diff_exposure_DM = diff_ANYP_DM)


na_summaryb <- analytic_sample %>%
  summarise(across(everything(), ~sum(is.na(.))))

saveRDS(analytic_sample,paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_analytic dataset with other prescription.RDS"))



