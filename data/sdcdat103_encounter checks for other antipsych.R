rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/encounter_distinct_Range_index.R")




t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  encounter_distinct_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="earliest_otherap_datekey",
                                 project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat102",
                                 LowerInterval = -2,UpperInterval = -1) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdcdat103a"),partitioning = c("Year"))
  
  encounter_distinct_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="earliest_otherap_datekey",
                                 project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat102",
                                 LowerInterval = -1,UpperInterval = 0) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdcdat103b"),partitioning = c("Year"))
}
t - Sys.time()


