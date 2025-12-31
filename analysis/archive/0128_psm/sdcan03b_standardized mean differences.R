rm(list=ls());gc();source(".Rprofile")

analytic_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan03/analytic sample with matchit.RDS"))
summary_matchit <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan03/summary of matchit.RDS")) %>% 
  .$sum.matched %>% 
  data.frame() %>% 
  mutate(term = rownames(.))
rownames(summary_matchit) = NULL

source("H:/code/functions/causality/standardized_difference.R")

p_vars = c("female","insurance_medicare",
           "insurance_other",
           "insurance_medicaid",
           "insurance_selfpay")

c_vars = c("age",
           "SviOverallPctlRankByZip2020_X")

g_vars = c("raceeth","region")


smd = map_dfr(c(c_vars,p_vars,g_vars),
              function(c_v){
                print(c_v);
                standardized_difference(x_variable=analytic_df %>% dplyr::select(one_of(c_v)) %>% pull(),
                                        a_variable=analytic_df$exposure_binary,
                                        w_variable =analytic_df$sipw) %>% 
                  mutate(variable = c_v) %>% 
                  return(.)
              }) %>% 
  left_join(summary_matchit,by=c("variable"="term"))

write_csv(smd,file="analysis/sdcan03b_standardized mean differences.csv")

