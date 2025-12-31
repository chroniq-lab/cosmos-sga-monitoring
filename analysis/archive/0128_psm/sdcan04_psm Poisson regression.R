rm(list=ls());gc();source(".Rprofile")

library(lmtest)
library(sandwich)

source("analysis/sdcan_linear and logistic regression equations.R")


matched_data <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan03/analytic sample with matchit.RDS")) %>% 
  mutate(
    abc_ctl_Y1 = case_when(
      mean_hba1c_Y1<8 & mean_sbp_Y1<140 & mean_dbp_Y1<90 & mean_ldl_Y1<100 ~ 1,
      TRUE ~ 0
      ),
    abc_ctl_Y2 = case_when(
      mean_hba1c_Y2<8 & mean_sbp_Y2<140 & mean_dbp_Y2<90 & mean_ldl_Y2<100 ~ 1,
      TRUE ~ 0
    ),
    abc_ctl_Y3 = case_when(
      mean_hba1c_Y3<8 & mean_sbp_Y3<140 & mean_dbp_Y3<90 & mean_ldl_Y3<100 ~ 1,
      TRUE ~ 0
    ),
    abc_ctl_all = case_when(
      abc_ctl_Y1==1 & abc_ctl_Y2==1 & abc_ctl_Y3==1 ~ 1,
      TRUE ~ 0
    )
  )



fit_poisson_robust <- function(data,f) {
  model <- glm(as.formula(f), data = data, family = poisson)
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
  return(robust_se)
}

f_y1 = paste0("abc_ctl_Y1 ~ exposure_category + raceeth +",covariates)
f_y2 = paste0("abc_ctl_Y2 ~ exposure_category + raceeth +",covariates)
f_y3 = paste0("abc_ctl_Y3 ~ exposure_category + raceeth +",covariates)
f_all = paste0("abc_ctl_all ~ exposure_category + raceeth +",covariates)

abc_y1 <- fit_poisson_robust(data=matched_data,f=f_y1) 
abc_y2 <- fit_poisson_robust(data=matched_data,f=f_y2)
abc_y3 <- fit_poisson_robust(data=matched_data,f=f_y3)
abc_all <- fit_poisson_robust(data=matched_data,f=f_all)

sdcan04_abc <- bind_rows(
  broom::tidy(abc_y1) %>% mutate(outcome = "abc",type="adjusted",year = "Y1"),
  broom::tidy(abc_y2) %>% mutate(outcome = "abc",type="adjusted",year = "Y2"),
  broom::tidy(abc_y3) %>% mutate(outcome = "abc",type="adjusted",year = "Y3"),
  broom::tidy(abc_all) %>% mutate(outcome = "abc",type="adjusted",year = "All 3y")
    
  ) %>% 
  
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error))


sdcan04_abc %>% 
  write_csv(.,"analysis/sdcan04_psm poisson estimates.csv")


