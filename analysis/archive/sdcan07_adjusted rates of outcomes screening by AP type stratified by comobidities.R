rm(list=ls());gc();source(".Rprofile")

library(splines)
library(emmeans)
library(ggeffects)
library(geepack)
emm_options(pbkrtest.limit = 999999)
library(lme4)

source("analysis/sdcan_linear and logistic regression equations.R")


long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  mutate(comorbidity_ava = case_when(rowSums(across(all_of(dx_rx)) == 1) > 0 ~ "Yes",
                                     TRUE ~ "No")) %>% 
  mutate(
    period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
    bmi_history_category = factor(bmi_history_category,levels=c("Underweight or Normal","Overweight","Obese"),
                                  labels=c("Underweight or Normal","Overweight","Obese")),
    comorbidity_ava = factor(comorbidity_ava,levels=c("No","Yes"),labels=c("No","Yes"))
  ) 

data_yes <- subset(long_df, comorbidity_ava == "Yes") %>% 
  mutate(ap_type = factor(ap_type,levels=c("Other","SGA","Not_SGA"),labels=c("None","SGA","FGA")),
         ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X))

# with comorbidities
t = Sys.time()
m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())
t - Sys.time()
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())

out1 = ggpredict(m1,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)


bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07a_modeled probability of abc with comobidities.csv"))


m3 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())

out3 = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)


bind_rows(out3 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out4 %>% as.data.frame() %>% mutate(outcome = "LDL")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07b_modeled probability of abc with comobidities.csv"))


m5 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())
m6 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,covariates0,dx_rx_covariates)),id=PatientDurableKey,corstr="independence",data=data_yes,family=binomial())

out5 = ggpredict(m5,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m5$geese$vbeta)
out6 = ggpredict(m6,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m6$geese$vbeta)


bind_rows(out5 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out6 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07c_modeled probability of abc with comobidities.csv"))

#--------------------------------------------------------------------
# without comorbidities
data_no <- data_no %>% 
  dplyr::filter(comorbidity_ava == "No" & ap_type != "None") %>% 
  mutate(sga_type = case_when(ap_type == "SGA" ~ "SGA",
                              TRUE ~ "FGA")) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("Alaska","District of Columbia","Montana","South Dakota")) %>% 
  mutate(sga_type = factor(sga_type,levels=c("FGA","SGA"),labels=c("FGA","SGA")),
         ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X))

exposure_main = c("sga_type + period + sga_type:period")

m7 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())
m8 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())

out7 = ggpredict(m7,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m7$geese$vbeta)
out8 = ggpredict(m8,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m8$geese$vbeta)

m9 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())
m10 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())

out9 = ggpredict(m9,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m9$geese$vbeta)
out10 = ggpredict(m10,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m10$geese$vbeta)

m11 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())
m12 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,covariates0)),id=PatientDurableKey,corstr="independence",data=data_no,family=binomial())

out11 = ggpredict(m11,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m11$geese$vbeta)
out12 = ggpredict(m12,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m12$geese$vbeta)


bind_rows(out7 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out8 %>% as.data.frame() %>% mutate(outcome = "BMI"),
          out9 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out10 %>% as.data.frame() %>% mutate(outcome = "LDL"),
          out11 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out12 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07d_modeled probability of abc with comobidities.csv"))



