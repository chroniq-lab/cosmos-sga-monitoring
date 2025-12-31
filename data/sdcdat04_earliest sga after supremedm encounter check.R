rm(list=ls());gc();source(".Rprofile")


sdcdat03a = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat03a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_sga_datekey = as.numeric(earliest_sga_datekey)) %>% 
  distinct(PatientDurableKey,earliest_sga_datekey) %>% 
  mutate(Count = 1) 

sdcdat03b = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat03b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_sga_datekey = as.numeric(earliest_sga_datekey)) %>% 
  distinct(PatientDurableKey,earliest_sga_datekey) %>% 
  mutate(Count = 1)

sdcdat02 = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat02/earliest sga supremedm.parquet"))  %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

sdcdat02 %>% 
  head() %>% collect()

rfd03 = open_dataset(paste0(path_retinopathy_fragmentation_folder,"/working/rfd03/combined cp before encounter check.parquet")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

rfd03 %>% 
  head() %>% collect()

sdcdat04 <- sdcdat02 %>% 
  left_join(rfd03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_date),
            by=c("PatientDurableKey"="PatientDurableKey")) %>% 
  left_join(sdcdat03a %>% 
              rename(count_minus2y = Count),
            by=c("PatientDurableKey","earliest_sga_datekey")) %>%
  left_join(sdcdat03b %>% 
              rename(count_minus1y = Count),
            by=c("PatientDurableKey","earliest_sga_datekey")) %>% 
  to_duckdb() %>% 
  dplyr::filter(earliest_sga_datekey >= 2012000, earliest_sga_datekey <= 20240000,count_minus2y == 1, count_minus1y == 1) %>% 
  collect() %>% 
  distinct(PatientDurableKey,earliest_sga_datekey,.keep_all = TRUE)

short = sdcdat04 %>% 
  dplyr::filter(earliest_sga_ymd <= (diagnosis_date+dmonths(2))) 

table(year(short$earliest_sga_ymd))

sdcdat04 %>% 
  write_parquet(.,paste0(path_sga_dm_control_folder,"/working/sdcdat04/earliest sga supremedm after encounter checks.parquet"))
sdcdat04 %>% 
  write_csv(.,paste0(path_sga_dm_control_folder,"/working/sdcdat04/earliest sga supremedm after encounter checks.csv"))

sdcdat04 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat04/earliest sga supremedm after encounter checks.parquet"))

