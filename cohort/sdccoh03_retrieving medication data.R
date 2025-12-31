rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/medications_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  medications_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jguo1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                  LowerInterval = -2,UpperInterval = 3,filter_year = year,end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh03"),partitioning = c("Year","TherapeuticClass"))
  
}
t - Sys.time()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh03"),partitioning = c("Year","TherapeuticClass")) %>%
  dplyr::filter(PatientDurableKey == 185170) %>% 
  collect() %>% View()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh03"),partitioning = c("Year")) %>%
  dim()

for(year in c(2016:2023)){
  print(year)
  print("Lookback")
  medications_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                   LowerInterval = -5,UpperInterval = -2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh03c"),partitioning = c("Year"))
}
