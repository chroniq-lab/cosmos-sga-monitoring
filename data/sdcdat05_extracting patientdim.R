

rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/demographics.R")


demographics(con_Cosmos, 
             project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat02") %>% 
  write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdcdat05"),partitioning = "ValidatedStateOrProvince_X")

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat05"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dim()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat05"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  distinct(PatientDurableKey) %>%
  collect() %>% 
  dim()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat05"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dplyr::filter(PrimaryRUCA_X != "*Unspecified") %>% 
  head() %>%
  collect()

