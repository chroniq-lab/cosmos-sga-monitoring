rm(list=ls());gc();source(".Rprofile")

# 1. new T2D + US residence, N = 727,076
decdat03 <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam"))


# 2. SGA, FGA, neither after T2D
sga_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01/sdcan01_analytic dataset.RDS")) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam"))


closest_prescriptions_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions after diagnosis date.parquet")) %>% 
  dplyr::select(PatientDurableKey,YearMonthKey,YearMonth,diagnosis_datekey) %>% 
  left_join(decdat03 %>% 
              select("PatientDurableKey","diagnosis_datekey","diagnosis_date"),
            by = c("PatientDurableKey","diagnosis_datekey")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = as.numeric(YearMonthKey)) %>% 
  mutate(YearMonthKey_ymd = ymd(YearMonthKey)) %>% 
  mutate(diff_ANYP_DM = abs(as.numeric(difftime(diagnosis_date,YearMonthKey_ymd,units = "days")))) %>% 
  group_by(PatientDurableKey,diagnosis_datekey) %>% 
  dplyr::filter(diff_ANYP_DM == min(diff_ANYP_DM)) %>% 
  slice(1) %>%  
  ungroup() 


analytic_dataset <- decdat03 %>% 
  inner_join(closest_prescriptions_after %>% 
               rename(ANYPYearMonthKey_ymd = YearMonthKey_ymd,
                      ANYPYearMonthKey = YearMonthKey,
                      ANYPYearMonth = YearMonth),
             by = c("PatientDurableKey","diagnosis_datekey","diagnosis_date"))

neither_df <- analytic_dataset %>% 
  dplyr::filter(!PatientDurableKey %in% sga_df$PatientDurableKey) 

# 3. SGA, FGA, neither within 2y after T2D
sga_df_2y <- sga_df %>% 
  mutate(diff_AP_DM = abs(as.numeric(difftime(earliest_ymd,diagnosis_date,units = "days")))) %>% 
  dplyr::filter(diff_AP_DM <= 730)

neither_df_2y <- neither_df %>% 
  dplyr::filter(diff_ANYP_DM <= 730)



