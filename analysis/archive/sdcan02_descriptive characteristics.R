rm(list=ls());gc();source(".Rprofile")

library(gtsummary)

descriptive_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01_analytic dataset.RDS")) %>% 
  left_join(read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
              dplyr::select(PatientDurableKey,Dmagediag,Bmi,Hba1c,Ldlc,Sbp,Dbp),
            by = c("PatientDurableKey")) %>% 
  mutate(diff_index_origin_time = as.numeric(difftime(earliest_ymd,diagnosis_date,units="days")/365.25)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black","Hispanic","NH Other")),
         SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100)



(descriptives <- descriptive_df %>% 
    tbl_summary(by=type,
                include = c(female,raceeth,
                            SviOverallPctlRankByZip2020_X,
                            
                            Dmagediag,Bmi,Hba1c,Ldlc,Sbp,Dbp,
                            
                            diff_index_origin_time
                            
                            
                            
                ),
                missing = "ifany",
                missing_text = "Missing",
                type = list(Dmagediag ~ "continuous",
                            female ~ "dichotomous",
                            raceeth ~ "categorical",
                            SviOverallPctlRankByZip2020_X ~ "continuous2",
                            
                            
                            Bmi ~ "continuous", Hba1c ~ "continuous2", Sbp ~ "continuous", Dbp ~ "continuous",
                            Ldlc ~ "continuous", 
                            diff_index_origin_time ~ "continuous2"
                            
                            
                ),
                digits = list(Dmagediag ~ c(1,1),
                              SviOverallPctlRankByZip2020_X ~ c(1,1,1,1,1),
                              Bmi ~ c(1,1), Sbp ~ c(1,1), Dbp ~ c(1,1),
                              Hba1c ~ c(1,1,1,1,1), Ldlc ~ c(1,1), 
                              diff_index_origin_time ~ c(1,1,1,1,1)
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "analysis/sdcan02_descriptive characteristics.html")

