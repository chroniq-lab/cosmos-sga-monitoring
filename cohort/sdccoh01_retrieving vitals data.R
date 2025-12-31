rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/vitals_index.R")

t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  vitals_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                         LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh01a"),partitioning = c("Year"))
  print("Followup")
  vitals_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                         LookBackInterval = 0,FollowUpInterval = 3,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh01b"),partitioning = c("Year"))
  
}
Sys.time() - t


open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh01a"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

