rm(list=ls());gc();source(".Rprofile")

library(splines)
library(emmeans)
library(ggeffects)
library(geepack)
emm_options(pbkrtest.limit = 999999)
library(lme4)

long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  mutate(
    ap_type = factor(ap_type,levels=c("Neither","SGA","FGA")),
    period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
    ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X)
  ) 

source("analysis/sdcan_linear and logistic regression equations.R")


m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m3 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m5 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m6 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,basic_covariates)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

# 26.48 secs
out1 = ggpredict(m1,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)
out3 = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)
out5 = ggpredict(m5,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m5$geese$vbeta)
out6 = ggpredict(m6,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m6$geese$vbeta)

bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out3 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out4 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out5 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out6 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes")
) %>%  
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan04/sdcan04a_modeled unadjusted probability of 5 outcomes.csv"))


