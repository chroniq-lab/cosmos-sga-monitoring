rm(list=ls());gc();source(".Rprofile")

source("analysis/sdcan_linear and logistic regression equations.R")

descriptive_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  mutate(Dmagediag = as.numeric(difftime(diagnosis_date,BirthDate,units="days")/365.25),
         diff_exposure_DM_months = as.numeric(diff_exposure_DM/30)) %>% 
  mutate(bmi_history_category = case_when(bmi_history >= 25 & bmi_history < 30 ~ "Overweight",
                                          bmi_history >= 30 ~ "Obese",
                                          TRUE ~ "Underweight or Normal"),
         insurance = case_when(insurance_medicare == 1 ~ "Medicare",
                               insurance_medicaid == 1 ~ "Medicaid",
                               insurance_other == 1 ~ "Other",
                               insurance_selfpay == 1 ~ "Selfpay",
                               TRUE ~ NA_character_)) %>% 
  mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA")),
         cci_category = factor(cci_category,levels=c("mild","moderate","severe"),labels=c("mild","moderate","severe"))) %>% 
  mutate(any_Psychiatry = case_when(Psychiatry >= 1 ~ 1,
                                    TRUE ~ 0),
         any_PrimaryCare = case_when(PrimaryCare >= 1 ~ 1,
                                     TRUE ~ 0),
         any_OtherSpecialty = case_when(Psychiatry == 0 & PrimaryCare == 0 & AvailableSpecialty >= 1 ~ 1,
                                        TRUE ~ 0),
         UnspecifiedSpecialty = case_when(Psychiatry == 0 & PrimaryCare == 0 & AvailableSpecialty == 0 & UnspecifiedSpecialty >= 1 ~ 1,
                                          TRUE ~ 0)) %>% 
  mutate(prescriber_index = case_when(any_Psychiatry == 1 ~ "Psychiatry",
                                      any_PrimaryCare == 1 ~ "PrimaryCare",
                                      any_OtherSpecialty == 1 ~ "Other",
                                      UnspecifiedSpecialty == 1 ~ "Unspecified",
                                      TRUE ~ NA_character_))





c_vars = c("hba1c","bp","ldl")

df = descriptive_df %>% 
    dplyr::select(PatientDurableKey, ap_type, raceeth, female, age, region, diff_exposure_DM, bmi_history, year, 
                    insurance_medicare, insurance_medicaid, insurance_other, insurance_selfpay, 
                    SviOverallPctlRankByZip2020_X_imputed, ValidatedStateOrProvince_X,cci_category,
                  N_enc_Y1,N_enc_Y2,N_enc_Y3,
                  contains("hba1c"),contains("bp"),contains("ldl"),
                  one_of(dx_rx)
                  
                  ) %>% 
    pivot_longer(cols=tidyselect::starts_with(c("mean_","n_","N_")),names_pattern = "(.*)_(.*)",names_to=c(".value","period")) %>% 
    mutate(miss_hba1c = case_when(n_hba1c == 0 | is.na(n_hba1c) ~ 1,
                            TRUE ~ 0),
           
           miss_bp = case_when(n_bp == 0 | is.na(n_bp) ~ 1,
                               TRUE ~ 0),
           
           miss_ldl = case_when(n_ldl == 0 | is.na(n_ldl) ~ 1,
                                TRUE ~ 0),
           miss_any = case_when(n_hba1c == 0 | is.na(n_hba1c) |
                              n_bp == 0 | is.na(n_bp) | 
                              n_ldl == 0 | is.na(n_ldl) ~ 1,
                              TRUE ~ 0)
           ) %>% 
    mutate(N_enc = case_when(is.na(N_enc) ~ 0,
                             N_enc > 365 ~ 365,
                             TRUE ~ N_enc)) %>% 
    mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA")),
           period = factor(period,levels=c("Y1","Y2","Y3"),labels=c("Y1","Y2","Y3")),
           ValidatedStateOrProvince_X = as.factor(ValidatedStateOrProvince_X),
           cci_category = factor(cci_category,levels=c("mild","moderate","severe"),labels=c("mild","moderate","severe")),
           sex = factor(female,levels=c(0,1),labels=c("male","female"))) 

# IPW for at least once --------------

ipw_df = df %>% 
  group_by(PatientDurableKey) %>% 
  mutate(hba1c_any = min(miss_hba1c),
         bp_any = min(miss_bp),
         ldl_any = min(miss_ldl),
         abc_any = min(miss_any)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,.keep_all=TRUE)

ipw1 = glm(as.formula(paste0("hba1c_any ~ ","ap_type",covariates0,dx_rx_covariates)),family = binomial(),data=ipw_df)
ipw2 = glm(as.formula(paste0("bp_any ~ ","ap_type",covariates0,dx_rx_covariates)),family = binomial(),data=ipw_df)
ipw3 = glm(as.formula(paste0("ldl_any ~ ","ap_type",covariates0,dx_rx_covariates)),family = binomial(),data=ipw_df)
ipw4 = glm(as.formula(paste0("abc_any ~ ","ap_type",covariates0,dx_rx_covariates)),family = binomial(),data=ipw_df)

probs1 = predict(ipw1,type="response")
probs2 = predict(ipw2,type="response")
probs3 = predict(ipw3,type="response")
probs4 = predict(ipw4,type="response")

ipw_df$hba1c_weights = 1/probs1
ipw_df$bp_weights = 1/probs2
ipw_df$ldl_weights = 1/probs3
ipw_df$abc_weights = 1/probs4


# Numerator model: unconditional probability
num_model <- glm(bp_any ~ 1, family = binomial(), data = ipw_df)
numerator <- predict(num_model, type = "response")

# Stabilized weights
ipw_df$bp_weights_stabilized <- numerator / probs2
summary(ipw_df$bp_weights_stabilized)

# truncation
upper_bound <- quantile(ipw_df$bp_weights_stabilized, 0.99)
ipw_df$bp_weights_trimmed <- pmin(ipw_df$bp_weights_stabilized, upper_bound)
summary(ipw_df$bp_weights_trimmed)



# Regression datasets --------

hba1c_df = df %>% 
  dplyr::filter(miss_hba1c == 0) %>% 
  mutate(hba1c_control = case_when(mean_hba1c <= 8 ~ 1,
                                   TRUE ~ 0)) %>% 
  left_join(ipw_df %>% 
              dplyr::select(PatientDurableKey,hba1c_weights),
            by=c("PatientDurableKey"))

bp_df = df %>% 
  dplyr::filter(miss_bp == 0) %>% 
  mutate(bp_control = case_when(mean_sbp <= 140 & mean_dbp <= 90 ~ 1,
                                   TRUE ~ 0)) %>% 
  left_join(ipw_df %>% 
              dplyr::select(PatientDurableKey,bp_weights_trimmed),
            by=c("PatientDurableKey"))

ldl_df = df %>% 
  dplyr::filter(miss_ldl == 0) %>% 
  mutate(ldl_control = case_when(mean_ldl <= 100 ~ 1,
                                TRUE ~ 0)) %>% 
  left_join(ipw_df %>% 
              dplyr::select(PatientDurableKey,ldl_weights),
            by=c("PatientDurableKey"))

abc_df = df %>% 
  dplyr::filter(miss_any == 0) %>% 
  mutate(abc_control = case_when(
    mean_hba1c <= 8 & mean_sbp <= 140 & mean_dbp <= 90 & mean_ldl <= 100 ~ 1,
    TRUE ~ 0
  )) %>% 
  left_join(ipw_df %>% 
              dplyr::select(PatientDurableKey,abc_weights),
            by=c("PatientDurableKey"))

rm(df,ipw1,ipw2,ipw3,ipw4);gc()

# Model 1 ----------------------------------------------------------------

library(geepack)
library(emmeans)
library(ggeffects)
t = Sys.time()

m1 <- geeglm(formula = as.formula(paste0("hba1c_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=hba1c_df,family=binomial())
t - Sys.time()
m2 <- geeglm(formula = as.formula(paste0("bp_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=bp_df,family=binomial())



m3 <- geeglm(formula = as.formula(paste0("ldl_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=ldl_df,family=binomial())

m4 <- geeglm(formula = as.formula(paste0("abc_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=abc_df,family=binomial())

out1 = ggpredict(m1,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)
out3 = ggpredict(m3,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)

bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c Control"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BP Control"),
          out3 %>% as.data.frame() %>% mutate(outcome = "LDL Control"),
          out4 %>% as.data.frame() %>% mutate(outcome = "ABC Control")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07_model rates of control.csv"))



m1w <- geeglm(formula = as.formula(paste0("hba1c_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=hba1c_df,weights = hba1c_weights,family=binomial())
t - Sys.time()
m2w <- geeglm(formula = as.formula(paste0("bp_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=bp_df,weights=bp_weights_trimmed,family=binomial())



m3w <- geeglm(formula = as.formula(paste0("ldl_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=ldl_df,weights = ldl_weights,family=binomial())

m4w <- geeglm(formula = as.formula(paste0("abc_control ~ ",exposure_main,covariates0,dx_rx_covariates,cci_covariate,encounter_covariate)),
             id=PatientDurableKey,corstr="exchangeable",data=abc_df,weights = abc_weights,family=binomial())


out1w = ggpredict(m1w,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m1w$geese$vbeta)
out2w = ggpredict(m2w,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m2w$geese$vbeta)
out3w = ggpredict(m3w,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m3w$geese$vbeta)
out4w = ggpredict(m4w,terms = c("ap_type", "period[all]"),type = "fixed",typical = "weighted.mean",vcov_fun = m4w$geese$vbeta)

bind_rows(
          out1w %>% as.data.frame() %>% mutate(outcome = "HbA1c Control IPW"),
          out2w %>% as.data.frame() %>% mutate(outcome = "BP Control IPW"),
          out3w %>% as.data.frame() %>% mutate(outcome = "LDL Control IPW"),
          out4w %>% as.data.frame() %>% mutate(outcome = "ABC Control IPW"),
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07_model rates of control with ipw.csv"))

View(bind_rows(
  out1w %>% as.data.frame() %>% mutate(outcome = "HbA1c Control IPW"),
  out2w %>% as.data.frame() %>% mutate(outcome = "BP Control IPW"),
  out3w %>% as.data.frame() %>% mutate(outcome = "LDL Control IPW"),
  out4w %>% as.data.frame() %>% mutate(outcome = "ABC Control IPW"),
))






