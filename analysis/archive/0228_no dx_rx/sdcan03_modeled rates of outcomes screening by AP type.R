rm(list=ls());gc();source(".Rprofile")

library(splines)
library(emmeans)
library(ggeffects)
library(geepack)
emm_options(pbkrtest.limit = 999999)
library(lme4)

long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  mutate(
    ap_type = factor(ap_type,levels=c("Other","SGA","Not_SGA"),labels=c("None","SGA","FGA")),
    period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
    ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X),
    bmi_history_category = factor(bmi_history_category,levels=c("Underweight or Normal","Overweight","Obese"),
                                  labels=c("Underweight or Normal","Overweight","Obese"))
    )

source("analysis/sdcan_linear and logistic regression equations.R")


# divide into 2 parts
# 14.85 mins
m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m3 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

# 26.48 secs
out1 = ggpredict(m1,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)
out3 = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)


# m3_check <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=poisson())
# out3_check = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed")


bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out3 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out4 %>% as.data.frame() %>% mutate(outcome = "LDL")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03a_modeled probability of abc.csv"))

# exit, restart the program
m5 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m6 <- geeglm(formula = as.formula(paste0(outcome_neuropathy_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m7 <- geeglm(formula = as.formula(paste0(outcome_retinopathy_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m8 <- geeglm(formula = as.formula(paste0(outcome_anycpl_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out5 = ggpredict(m5,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m5$geese$vbeta)
out6 = ggpredict(m6,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m6$geese$vbeta)
out7 = ggpredict(m7,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m7$geese$vbeta)
out8 = ggpredict(m8,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m8$geese$vbeta)


bind_rows(out5 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out6 %>% as.data.frame() %>% mutate(outcome = "Neuropathy"),
          out7 %>% as.data.frame() %>% mutate(outcome = "Retinopathy"),
          out8 %>% as.data.frame() %>% mutate(outcome = "Any complications")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03b_modeled probability of complications.csv"))

m18 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
out18 = ggpredict(m18,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m18$geese$vbeta)

out18 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes") %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03e_modeled probability of any 5 outcomes.csv"))

# EXTRA INTERACTION TERM ----------- 

m9 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m10 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out9 = ggpredict(m9,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m9$geese$vbeta)
out10 = ggpredict(m10,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m10$geese$vbeta)


bind_rows(out9 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out10 %>% as.data.frame() %>% mutate(outcome = "BMI")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03ca_modeled probability of abc with bmi history.csv"))

# exit, restart
m11 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m12 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out11 = ggpredict(m11,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m11$geese$vbeta)
out12 = ggpredict(m12,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m12$geese$vbeta)


bind_rows(out11 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out12 %>% as.data.frame() %>% mutate(outcome = "LDL")
) %>% 
write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03cb_modeled probability of abc with bmi history.csv"))


# exit, restart the program
m13 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m14 <- geeglm(formula = as.formula(paste0(outcome_neuropathy_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out13 = ggpredict(m13,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m13$geese$vbeta)
out14 = ggpredict(m14,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m14$geese$vbeta)


bind_rows(out13 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out14 %>% as.data.frame() %>% mutate(outcome = "Neuropathy")
) %>%  
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03da_modeled probability of complications with bmi history.csv"))

# exit, restart
m15 <- geeglm(formula = as.formula(paste0(outcome_retinopathy_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m16 <- geeglm(formula = as.formula(paste0(outcome_anycpl_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out15 = ggpredict(m15,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m15$geese$vbeta)
out16 = ggpredict(m16,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m16$geese$vbeta)


bind_rows(out15 %>% as.data.frame() %>% mutate(outcome = "Retinopathy"),
          out16 %>% as.data.frame() %>% mutate(outcome = "Any complications")
) %>%  
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03db_modeled probability of complications with bmi history.csv"))


m17 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,covariates0,hisbmi_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
out17 = ggpredict(m17,terms = c("ap_type", "period[all]", "bmi_history_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m17$geese$vbeta)

out17 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes") %>%  
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03dc_modeled probability of any 5 outcomes with bmi history.csv"))



