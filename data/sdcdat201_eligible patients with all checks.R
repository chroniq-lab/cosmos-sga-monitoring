rm(list=ls());gc();source(".Rprofile")


# https://github.com/apache/arrow/issues/12371
# Could not use open_dataset() or union_all() because diagnosis_datekey had different variable type

sdcdat04 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat04/earliest sga supremedm after encounter checks.parquet"))
sdcdat104 <- read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat104/earliest other antipsych supremedm after encounter checks.parquet"))

eligible_patients <- bind_rows(sdcdat04 %>% 
                                 mutate(type = "SGA",
                                        diagnosis_datekey = as.numeric(diagnosis_datekey)) %>% 
                                 rename_all(~str_replace(.,pattern = "_sga",replacement = "")),
                               sdcdat104 %>% 
                                 mutate(type = "Not_SGA")%>% 
                                 rename_all(~str_replace(.,pattern="_otherap",replacement=""))) %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(earliest_datekey == min(earliest_datekey)) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(type_polypharmacy = case_when(n>1 ~ "Both",
                          TRUE ~ paste0("Only_", type))) %>% 
  # N = 763793 from 705,204 unique patients
  dplyr::filter(n==1|(n>1&type=="SGA"))
  # N = 705204

eligible_patients %>% 
  dplyr::select(PatientDurableKey,earliest_datekey) %>% 
  write_csv(.,paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.csv"))

eligible_patients %>% 
  write_parquet(.,paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))


