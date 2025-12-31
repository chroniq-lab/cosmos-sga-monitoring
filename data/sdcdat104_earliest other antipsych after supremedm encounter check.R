rm(list=ls());gc();source(".Rprofile")


sdcdat103a = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat103a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_otherap_datekey = as.numeric(earliest_otherap_datekey)) %>% 
  distinct(PatientDurableKey,earliest_otherap_datekey) %>% 
  mutate(Count = 1) 

sdcdat103b = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat103b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         earliest_otherap_datekey = as.numeric(earliest_otherap_datekey)) %>% 
  distinct(PatientDurableKey,earliest_otherap_datekey) %>% 
  mutate(Count = 1)

sdcdat102 = open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat102/earliest other antipsych supremedm.parquet"))  %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

sdcdat102 %>% 
  head() %>% collect()

rfd03 = open_dataset(paste0(path_retinopathy_fragmentation_folder,"/working/rfd03/combined cp before encounter check.parquet")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

rfd03 %>% 
  head() %>% collect()

sdcdat104 <- sdcdat102 %>% 
  left_join(rfd03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_date),
            by=c("PatientDurableKey"="PatientDurableKey")) %>% 
  left_join(sdcdat103a %>% 
              rename(count_minus2y = Count),
            by=c("PatientDurableKey","earliest_otherap_datekey")) %>%
  left_join(sdcdat103b %>% 
              rename(count_minus1y = Count),
            by=c("PatientDurableKey","earliest_otherap_datekey")) %>% 
  to_duckdb() %>% 
  dplyr::filter(earliest_otherap_datekey >= 2012000, earliest_otherap_datekey <= 20240000,count_minus2y == 1, count_minus1y == 1) %>% 
  collect() %>% 
  distinct(PatientDurableKey,earliest_otherap_datekey,.keep_all = TRUE)

short = sdcdat104 %>% 
  dplyr::filter(earliest_otherap_datekey <= (diagnosis_date+dmonths(2))) 

table(year(short$earliest_otherap_ymd))

sdcdat104 %>% 
  write_parquet(.,paste0(path_sga_dm_control_folder,"/working/sdcdat104/earliest other antipsych supremedm after encounter checks.parquet"))
sdcdat104 %>% 
  write_csv(.,paste0(path_sga_dm_control_folder,"/working/sdcdat104/earliest other antipsych supremedm after encounter checks.csv"))

sdcdat104 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat104/earliest other antipsych supremedm after encounter checks.parquet"))

