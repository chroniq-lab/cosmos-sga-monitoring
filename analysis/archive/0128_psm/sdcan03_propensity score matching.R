rm(list=ls());gc();source(".Rprofile")

source("analysis/sdcan_linear and logistic regression equations.R")

analytic_sample <- readRDS(paste0(path_sga_dm_control_folder,"/working/analytic sample with covariates.RDS"))

ps_equation <- paste0("exposure_binary ~ ",covariates)

# https://stackoverflow.com/questions/63984936/how-to-use-matchit-in-r-when-you-already-have-a-propensity-score
library(MatchIt)
# matchit(exposure_binary ~ prob_exposed, data = analytic_sample, method = "full", distance = analytic_sample$prob_exposed)
m.out = matchit(as.formula(ps_equation), data = analytic_sample, method = "nearest", distance = "glm",replace = FALSE,caliper=0.1,ratio=4)

s = summary(m.out,standardize=TRUE)
saveRDS(s,paste0(path_sga_dm_control_folder,"/working/sdcan03/summary of matchit.RDS"))

readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan03/summary of matchit.RDS")) %>% 
  .$sum.matched %>% 
  data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  write_csv(.,"analysis/sdcan03_summary of matchit matched.csv")


readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan03/summary of matchit.RDS")) %>% 
  .$sum.all %>% 
  data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  write_csv(.,"analysis/sdcan03_summary of matchit unmatched.csv")

matched_data = match.data(m.out)

saveRDS(matched_data,paste0(path_sga_dm_control_folder,"/working/sdcan03/analytic sample with matchit.RDS"))
write_parquet(matched_data,paste0(path_sga_dm_control_folder,"/working/sdcan03/analytic sample with matchit.parquet"))



