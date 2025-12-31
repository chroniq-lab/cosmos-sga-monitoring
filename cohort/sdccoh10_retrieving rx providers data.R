rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/medications_providers_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback ANTIHYPERGLYCEMICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                   LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "ANTIHYPERGLYCEMICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
 print("Lookback ANTI-OBESITY DRUGS")
  
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "ANTI-OBESITY DRUGS") %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback PSYCHOTHERAPEUTIC DRUGS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "PSYCHOTHERAPEUTIC DRUGS") %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  
  print("Lookback CARDIOVASCULAR")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "CARDIOVASCULAR",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback DIURETICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "DIURETICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback CARDIAC")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "CARDIAC DRUGS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback IMMUNOSUPPRESSANTS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "IMMUNOSUPPRESSANTS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback ANTIARTHRITICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",IndexDateKey_Var = "earliest_datekey",
                                             LowerInterval = -2,UpperInterval = 3,filter_year = year,therapeutic_class = "ANTIARTHRITICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
}
t - Sys.time()

# open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh10a"),partitioning = c("Year")) %>%
#   dplyr::filter(PatientDurableKey == 185170) %>% 
#   collect() %>% View()
# 
open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh10c"),partitioning = c("Year")) %>% 
  group_by(PrimarySpecialty,SecondSpecialty) %>% 
  tally() %>% 
  collect() %>% 
  write_csv(.,"cohort/sdccoh10_distinct primary and second specialty.csv")

