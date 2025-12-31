rm(list=ls());gc();source(".Rprofile")


eligible_patients <- 
  read_parquet(paste0(path_sga_dm_control_folder,"/working/sdcdat201/eligible patients after distinct check.parquet"))


sdcdat05 <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat05"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0)) %>% 
  # NIH PHS
  mutate(race_nih_phs = case_when(
    FirstRace != "" & SecondRace != "" ~ 6, # More than One Race  
    FirstRace == "" & SecondRace == "" ~ 7,   
    FirstRace == "American Indian or Alaska Native"  ~ 1,
    FirstRace == "Asian"  ~ 2,
    FirstRace == "Native Hawaiian or Other Pacific Islander"  ~ 3,
    FirstRace == "Black or African American"  ~ 4,
    FirstRace == "White"  ~ 5,
    TRUE ~ 7),
    sex_nih_phs = case_when(Sex == "Female" ~ 1,
                            Sex == "Male" ~ 2,
                            TRUE ~ 3)) %>% 
  dplyr::select(PatientDurableKey,raceeth,female,race_nih_phs,sex_nih_phs,Ethnicity,ValidatedStateOrProvince_X,BirthDate) %>% 
  collect() 

sdcdat105 <- open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat105"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0)) %>% 
  # NIH PHS
  mutate(race_nih_phs = case_when(
    FirstRace != "" & SecondRace != "" ~ 6, # More than One Race  
    FirstRace == "" & SecondRace == "" ~ 7,   
    FirstRace == "American Indian or Alaska Native"  ~ 1,
    FirstRace == "Asian"  ~ 2,
    FirstRace == "Native Hawaiian or Other Pacific Islander"  ~ 3,
    FirstRace == "Black or African American"  ~ 4,
    FirstRace == "White"  ~ 5,
    TRUE ~ 7),
    sex_nih_phs = case_when(Sex == "Female" ~ 1,
                            Sex == "Male" ~ 2,
                            TRUE ~ 3)) %>% 
  dplyr::select(PatientDurableKey,raceeth,female,race_nih_phs,sex_nih_phs,Ethnicity,ValidatedStateOrProvince_X,BirthDate) %>% 
  collect()

text_df <- bind_rows(sdcdat05,
                     sdcdat105) %>% 
  distinct(PatientDurableKey,.keep_all=TRUE) %>% 
  left_join(eligible_patients,
             by = "PatientDurableKey") %>% 
  mutate(sex_nih_phs = factor(sex_nih_phs,levels=c(1:3),labels=c("Female","Male","Other")),
         race_nih_phs = factor(race_nih_phs,levels=c(1:7),labels=c("AIAN","Asian","NHOPI","Black","White","MoreThanOne","Unknown")))  %>% 
  mutate(age = as.numeric(difftime(earliest_ymd,BirthDate,units="days")/365.25))  %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam")) %>%
  dplyr::filter(age >= 18, age <100)



text_df %>% 
  group_by(Ethnicity,sex_nih_phs,race_nih_phs) %>% 
  tally() %>% 
  pivot_wider(names_from=c(Ethnicity,sex_nih_phs),values_from=n) %>% 
  dplyr::select(race_nih_phs,contains("Not Hispanic"),contains("Hispanic or"),everything()) %>% 
  write_csv(.,"paper/text_nih phs form.csv")
  
