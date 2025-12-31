rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/labs_index.R")

t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "hba1c",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02aa"),partitioning = c("Year"))
  print("Followup hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "hba1c",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ab"),partitioning = c("Year"))

  print("Lookback fpg")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "fpg",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ba"),partitioning = c("Year"))
  print("Followup fpg")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "fpg",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02bb"),partitioning = c("Year"))


  print("Lookback ldl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "ldl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02da"),partitioning = c("Year"))
  print("Followup ldl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "ldl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02db"),partitioning = c("Year"))


  print("Lookback hdl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "hdl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ea"),partitioning = c("Year"))
  print("Followup hdl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "hdl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02eb"),partitioning = c("Year"))


  print("Lookback tgl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "tgl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02fa"),partitioning = c("Year"))
  print("Followup tgl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "tgl",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02fb"),partitioning = c("Year"))

  print("Lookback alt")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "alt",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ga"),partitioning = c("Year"))
  print("Followup alt")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "alt",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02gb"),partitioning = c("Year"))

  print("Lookback ast")
  if(year < 2024){
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "ast",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ha"),partitioning = c("Year"))
  }
  print("Followup ast")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "ast",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02hb"),partitioning = c("Year"))

  print("Lookback creatinine")
  if(year < 2024){
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "creatinine",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ia"),partitioning = c("Year"))
  }
  print("Followup creatinine")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "creatinine",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>%
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ib"),partitioning = c("Year"))
  
  
  # CBC PANEL ---------------
  # leukocytes <- c("6690-2","26464-8","49498-9","804-5"),
  # neutrophils <- c("26499-4","751-8","753-4"),
  # neutrophils_leukocytes <- c("26511-6"),
  # lymphocytes <- c("26474-7","731-0","732-8"), 
  # lymphocytes_leukocytes <- c("26478-8")
  
  print("Lookback leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ja"),partitioning = c("Year"))
  print("Followup leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02hb"),partitioning = c("Year"))
  
  print("Lookback neutrophils")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "neutrophils",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ka"),partitioning = c("Year"))
  print("Followup neutrophils")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "neutrophils",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02kb"),partitioning = c("Year"))
  
  print("Lookback lymphocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "lymphocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02la"),partitioning = c("Year"))
  print("Followup lymphocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "lymphocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02lb"),partitioning = c("Year"))
  
  print("Lookback neutrophils_leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "neutrophils_leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02ma"),partitioning = c("Year"))
  print("Followup neutrophils_leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "neutrophils_leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02mb"),partitioning = c("Year"))
  
  
  print("Lookback lymphocytes_leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "lymphocytes_leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02na"),partitioning = c("Year"))
  print("Followup lymphocytes_leukocytes")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD5A613.[ET4003\\shdw_1208_jvargh1].sdcdat201",type = "lymphocytes_leukocytes",
                       IndexDateKey_Var = "earliest_datekey",
                       LookBackInterval = 0,FollowUpInterval = 4,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdccoh02nb"),partitioning = c("Year"))
  
  
}
Sys.time() - t


# open_dataset(paste0(path_sga_dm_control_folder,"/working/sdccoh02aa"),partitioning = c("Year")) %>%
#   arrange(PatientDurableKey,YearMonth) %>%
#   head(n= 1000) %>%
#   collect() %>% View()

