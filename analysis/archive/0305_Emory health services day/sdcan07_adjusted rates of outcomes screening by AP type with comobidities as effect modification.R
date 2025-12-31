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
  ) %>% 
  mutate(total_comorbidities = rowSums(select(., htn_dx, obesity_dx, cerebro_dx, hld_dx, 
                                              cardiovascular_dx, pulmonary_dx, pcos_dx), na.rm = TRUE)) %>% 
  mutate(total_comorb_category = case_when(total_comorbidities <= 1 ~ "0-1",
                                           total_comorbidities >= 4 ~ "4-7",
                                           TRUE ~ "2-3"),
         total_comorb_category = factor(total_comorb_category,levels=c("0-1","2-3","4-7"),
                                       labels=c("0-1","2-3","4-7")))

source("analysis/sdcan_linear and logistic regression equations.R")

# check distribution
library(ggplot2)

ggplot(long_df, aes(x = total_comorbidities)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Comorbidities",
       x = "Total Comorbidities",
       y = "Frequency") +
  theme_minimal()


quantile(long_df$total_comorbidities, 0.25, na.rm = TRUE)
quantile(long_df$total_comorbidities, 0.75, na.rm = TRUE)

# --------------------------------------------------------------------------------------------
t = Sys.time()
m1 <- geeglm(formula = as.formula(paste0(outcome_hba1c_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
t - Sys.time()
m2 <- geeglm(formula = as.formula(paste0(outcome_bmi_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out1 = ggpredict(m1,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m1$geese$vbeta)
out2 = ggpredict(m2,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m2$geese$vbeta)


bind_rows(out1 %>% as.data.frame() %>% mutate(outcome = "HbA1c"),
          out2 %>% as.data.frame() %>% mutate(outcome = "BMI")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07a_modeled probability of abc with total comorbidities.csv"))

# exit, restart
m3 <- geeglm(formula = as.formula(paste0(outcome_bp_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m4 <- geeglm(formula = as.formula(paste0(outcome_ldl_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out3 = ggpredict(m3,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m3$geese$vbeta)
out4 = ggpredict(m4,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m4$geese$vbeta)


bind_rows(out3 %>% as.data.frame() %>% mutate(outcome = "BP"),
          out4 %>% as.data.frame() %>% mutate(outcome = "LDL")
) %>% 
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07b_modeled probability of abc with total comorbidities.csv"))

# exit, restart the program
m5 <- geeglm(formula = as.formula(paste0(outcome_nephropathy_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())
m6 <- geeglm(formula = as.formula(paste0(outcome_anyout5_ava,exposure_main,covariates0,dx_rx_covariates,sumcomorb_covariate)),id=PatientDurableKey,corstr="independence",data=long_df,family=binomial())

out5 = ggpredict(m5,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m5$geese$vbeta)
out6 = ggpredict(m6,terms = c("ap_type", "period[all]", "total_comorb_category"),type = "fixed",typical = "weighted.mean",vcov_fun = m6$geese$vbeta)


bind_rows(out5 %>% as.data.frame() %>% mutate(outcome = "Nephropathy"),
          out6 %>% as.data.frame() %>% mutate(outcome = "Any 5 outcomes")
) %>%  
  write_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07c_modeled probability of complications with total comorbidities.csv"))


cumcom_summary <- long_df %>%
  group_by(ap_type, total_comorbidities) %>%
  summarize(unique_patients = n_distinct(PatientDurableKey), .groups = "drop")






