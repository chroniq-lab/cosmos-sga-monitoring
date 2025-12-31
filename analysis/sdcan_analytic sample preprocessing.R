rm(list=ls());gc();source(".Rprofile")

AP_prescription <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01a/sdcan01a_analytic dataset with AP.RDS"))
other_prescription <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01b/sdcan01b_analytic dataset with other prescription.RDS")) %>% 
  mutate(ap_type_wgr = "None",
         ap_type_polypharmacy = "None")

common_columns <- intersect(names(AP_prescription), names(other_prescription))

analytic_sample <- bind_rows(
  AP_prescription %>%
    select(all_of(common_columns),earliest_datekey),
  other_prescription %>%
    select(all_of(common_columns),ANYPYearMonthKey,
           mean_uacr_Y1,mean_uacr_Y2,mean_uacr_Y3,mean_egfr_Y1,mean_egfr_Y2,mean_egfr_Y3,mean_egfr2_Y1,mean_egfr2_Y2,mean_egfr2_Y3)
) %>% 
  mutate(exposure_datekey = case_when(ap_type == "Neither" ~ ANYPYearMonthKey,
                                      TRUE ~ earliest_datekey))

analytic_sample %>% dim()

na_summary <- analytic_sample %>%
  summarise(across(everything(), ~sum(is.na(.))))

saveRDS(analytic_sample,paste0(path_sga_dm_control_folder,"/working/sdcan_analytic dataset with AP type.RDS"))
write_csv(analytic_sample,paste0(path_sga_dm_control_folder,"/working/sdcan_analytic dataset with AP type.csv"))


# Add imputed bmi_history ---------------------------

imputed_df <- read_csv(paste0(path_sga_dm_control_folder, '/working/sdcan_knn imputation on bmi history.csv'))

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_analytic dataset with AP type.RDS")) %>% 
  select(-bmi_history) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(imputed_df %>% 
              select(PatientDurableKey,ap_type,exposure_datekey,bmi_history),
            by = c("PatientDurableKey","ap_type","exposure_datekey"))

saveRDS(analytic_sample,paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with AP type.RDS"))


# Add Billing code ---------------------------

billed_codes_before <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcan01/billing codes before exposure date.parquet")) %>% 
  dplyr::select(PatientDurableKey,exposure_datekey,dx) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=dx,values_from = value,values_fill = 0)

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with AP type.RDS")) %>% 
  left_join(billed_codes_before %>% 
              rename_with(~ paste0(., "_dx"), .cols = -c(PatientDurableKey, exposure_datekey)),
            by=c("PatientDurableKey","exposure_datekey")) %>% 
  mutate(across(c(ends_with(c("_dx"))),
                ~ case_when(is.na(.) ~ 0, 
                            TRUE ~ .)))

source("functions/calculate_cci.R")

df <- calculate_cci(analytic_sample) %>% 
  mutate(cci_category = case_when(CCI_score <= 3 ~ "mild",
                                  CCI_score > 3 & CCI_score <= 6 ~ "moderate",
                                  TRUE ~ "severe"))

saveRDS(df,paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS"))

na_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.))))


table(df$cci_category,df$ap_type)

