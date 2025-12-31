rm(list=ls());gc();source(".Rprofile")


earliest_sga <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat01"),partitioning = "Year") %>% 
  mutate(YearMonth_ymd = ymd(paste0(YearMonth,"01")),
         YearMonthKey = as.numeric(paste0(YearMonth,"01"))) %>% 
  collect() %>% 
  group_by(PatientDurableKey) %>% 
  summarize(earliest_sga_datekey = min(YearMonthKey),
            earliest_sga_ymd = min(YearMonth_ymd),
            SimpleGenericName = paste0(SimpleGenericName,collapse=";"))

earliest_sga %>% 
  write_parquet(.,paste0(path_sga_dm_control_folder,"/working/sdcdat02/earliest sga supremedm.parquet"))
earliest_sga %>% 
  dplyr::select(PatientDurableKey,earliest_sga_datekey) %>% 
  write_csv(.,paste0(path_sga_dm_control_folder,"/working/sdcdat02/earliest sga supremedm.csv"))

