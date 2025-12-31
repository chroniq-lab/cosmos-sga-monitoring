rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/diagnosis_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                LowerInterval = -2,UpperInterval = 0,filter_year = year,
                                by_letters = TRUE,alphabets=c("E","F","G","I","J"),
                                detection_string = "SUBSTRING(dtd.Value,1,1)") %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh04a"),partitioning = c("Year","Value_Grouper2"))
  
  print("Followup")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                 LowerInterval = 0,UpperInterval = 3,filter_year = year,
                                 by_letters = TRUE,alphabets=c("E","F","G","I","J"),
                                 detection_string = "SUBSTRING(dtd.Value,1,1)") %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh04b"),partitioning = c("Year","Value_Grouper2"))
}
t - Sys.time()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh04a"),partitioning = c("Year","Value_Grouper2")) %>%
  head(n= 1000) %>%
  collect() %>% View()

