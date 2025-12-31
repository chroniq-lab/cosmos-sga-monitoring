rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
library(ggpubr)

fig_df <- bind_rows(read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05c_modeled probability of abc with bmi history.csv")),
                    read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05d_modeled probability of abc with bmi history.csv")),
                    read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05e_modeled probability of complications with bmi history.csv"))) %>% 
  rename(ap_type = x,period = group,bmi_history_category = facet,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("None","SGA","FGA"),labels=c("None","SGA","FGA"))) 

# Underweight ------------------------
under_df <- fig_df %>% dplyr::filter(bmi_history_category == "Underweight or Normal")

a1c_df <- under_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "HbA1c",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

bmi_df <- under_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "BMI",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- under_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "LDL",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

neph_df <- under_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(,y = "Renal Function",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/underweight modeled rates of outcomes screening adjust for comorbidities by AP type.png"),width=8,height =6)


# Overweight ------------------------
over_df <- fig_df %>% dplyr::filter(bmi_history_category == "Overweight")

a1c_df <- over_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "HbA1c",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

bmi_df <- over_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "BMI",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- over_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "LDL",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

neph_df <- over_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(,y = "Renal Function",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/overweight modeled rates of outcomes screening adjust for comorbidities by AP type.png"),width=8,height =6)


# Obese ------------------------
obese_df <- fig_df %>% dplyr::filter(bmi_history_category == "Obese")

a1c_df <- obese_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "HbA1c",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

bmi_df <- obese_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "BMI",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- obese_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "LDL",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

neph_df <- obese_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(,y = "Renal Function",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/obese modeled rates of outcomes screening adjust for comorbidities by AP type.png"),width=8,height =6)




