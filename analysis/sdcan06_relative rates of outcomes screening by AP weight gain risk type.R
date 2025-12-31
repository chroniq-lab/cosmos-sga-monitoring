rm(list=ls());gc();source(".Rprofile")

library(splines)
library(emmeans)
library(ggeffects)
library(geepack)
emm_options(pbkrtest.limit = 999999)
library(lme4)

source("analysis/sdcan_linear and logistic regression equations.R")


long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  mutate(
    ap_type_wgr = factor(ap_type_wgr,levels=c("None","low_SGA","intermediate_SGA","high_SGA",
                                              "low_FGA","intermediate_FGA","high_FGA"),
                         labels=c("None","low_SGA","intermediate_SGA","high_SGA",
                                  "low_FGA","intermediate_FGA","high_FGA")),
    period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
    ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X),
    cci_category = factor(cci_category,levels=c("mild","moderate","severe"),labels=c("mild","moderate","severe"))
  ) %>% 
  mutate(N_enc = case_when(is.na(N_enc) ~ 0,
                           N_enc > 365 ~ 365,
                           TRUE ~ N_enc))


# Model: dx_rx + CCI_score category + unique encounters -------------------------

# 4.51min
t = Sys.time()
m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main_wgr,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main_wgr,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m3 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main_wgr,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main_wgr,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out1 = ggpredict(m1,terms = c("ap_type_wgr", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type_wgr", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)
out3 = ggpredict(m3,terms = c("ap_type_wgr", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type_wgr", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)


bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out3 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out4 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan06/sdcan06_model by weight gain risk AP type.csv"))

