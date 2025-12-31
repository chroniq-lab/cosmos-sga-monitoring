rm(list=ls());gc();source(".Rprofile")

library(splines)
library(emmeans)
library(ggeffects)
library(geepack)
emm_options(pbkrtest.limit = 999999)
library(lme4)

source("analysis/sdcan_linear and logistic regression equations.R")


long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  mutate(sex = case_when(female == 1 ~ "female",
                         TRUE ~ "male")) %>% 
  mutate(
    ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA")),
    period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
    ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X),
    cci_category = factor(cci_category,levels=c("mild","moderate","severe"),labels=c("mild","moderate","severe")),
    sex = factor(sex,levels=c("male","female"),labels=c("male","female"))
  ) %>% 
  mutate(N_enc = case_when(is.na(N_enc) ~ 0,
                           N_enc > 365 ~ 365,
                           TRUE ~ N_enc)) 
 


# Model 1: no dx_rx -------------------------------------------------------------
# 3.46min
t = Sys.time()
m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m3 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out1 = ggpredict(m1,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)
out3 = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)


bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out3 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out4 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05a_model1 no dx_rx.csv"))


# Model 2: dx_rx ----------------------------------------------------------------
# 3.82min
t = Sys.time()
m5 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m6 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m7 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m8 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out5 = ggpredict(m5,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m5$geese$vbeta)
out6 = ggpredict(m6,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m6$geese$vbeta)
out7 = ggpredict(m7,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m7$geese$vbeta)
out8 = ggpredict(m8,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m8$geese$vbeta)


bind_rows(out5 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out6 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out7 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out8 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05b_model2 with dx_rx.csv"))


# Model 3: dx_rx + CCI_score category -------------------------------------------
# 4.07min
t = Sys.time()
m9 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m10 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m11 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m12 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out9 = ggpredict(m9,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m9$geese$vbeta)
out10 = ggpredict(m10,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m10$geese$vbeta)
out11 = ggpredict(m11,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m11$geese$vbeta)
out12 = ggpredict(m12,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m12$geese$vbeta)


bind_rows(out9 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out10 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out11 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out12 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05c_model3 with dx_rx and cci category.csv"))


# Model 4: dx_rx, CCI_score category as effect modifier -------------------------
# 6.1min
t = Sys.time()
m13 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates,encounter_covariate,cci_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m14 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates,encounter_covariate,cci_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out13 = ggpredict(m13,terms = c("ap_type", "period[all]", "cci_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m13$geese$vbeta)
out14 = ggpredict(m14,terms = c("ap_type", "period[all]", "cci_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m14$geese$vbeta)


bind_rows(out13 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out14 %>% as.data.frame() %>% mutate(outcome = "BMI")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05da_model4 with dx_rx by cci category.csv"))


# exit, restart
m15 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates,encounter_covariate,cci_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m16 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates,encounter_covariate,cci_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out15 = ggpredict(m15,terms = c("ap_type", "period[all]", "cci_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m15$geese$vbeta)
out16 = ggpredict(m16,terms = c("ap_type", "period[all]", "cci_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m16$geese$vbeta)


bind_rows(out15 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out16 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05db_model4 with dx_rx by cci category.csv"))



# Model 5: dx_rx + CCI_score category + unique encounters (Main model) -------------------------

# 4.07min
t = Sys.time()
m17 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m18 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m19 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m20 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out17 = ggpredict(m17,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m17$geese$vbeta)
out18 = ggpredict(m18,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m18$geese$vbeta)
out19 = ggpredict(m19,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m19$geese$vbeta)
out20 = ggpredict(m20,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m20$geese$vbeta)


bind_rows(out17 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out18 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out19 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out20 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05e_model5 with dx_rx, cci category and encounters.csv"))



# Model 6: dx_rx + CCI_score category + unique encounters; sex as effect modifier-------------------------

# 4.17min
t = Sys.time()
m21 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates_nosex,dx_rx_covariates,cci_covariate,encounter_covariate,sex_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m22 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates_nosex,dx_rx_covariates,cci_covariate,encounter_covariate,sex_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m23 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates_nosex,dx_rx_covariates,cci_covariate,encounter_covariate,sex_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m24 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates_nosex,dx_rx_covariates,cci_covariate,encounter_covariate,sex_effmod)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out21 = ggpredict(m21,terms = c("ap_type", "period[all]", "sex"),type = "fixed",typical = "weighted.mean",vcov_fun = m21$geese$vbeta)
out22 = ggpredict(m22,terms = c("ap_type", "period[all]", "sex"),type = "fixed",typical = "weighted.mean",vcov_fun = m22$geese$vbeta)
out23 = ggpredict(m23,terms = c("ap_type", "period[all]", "sex"),type = "fixed",typical = "weighted.mean",vcov_fun = m23$geese$vbeta)
out24 = ggpredict(m24,terms = c("ap_type", "period[all]", "sex"),type = "fixed",typical = "weighted.mean",vcov_fun = m24$geese$vbeta)


bind_rows(out21 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out22 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out23 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out24 %>% as.data.frame() %>% mutate(outcome = "Nephropathy")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05f_model5 with dx_rx, cci category and encounters by sex.csv"))



