rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
library(ggpubr)

fig_df <- bind_rows(read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07a_modeled probability of abc with total comorbidities.csv")),
                    read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07b_modeled probability of abc with total comorbidities.csv")),
                    read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07c_modeled probability of complications with total comorbidities.csv"))) %>% 
  rename(ap_type = x,period = group,total_comorb_category = facet,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("None","SGA","FGA"),labels=c("None","SGA","FGA"))) 


# low ------------------------
low_df <- fig_df %>% dplyr::filter(total_comorb_category == "0-1")

a1c_df <- low_df %>% dplyr::filter(outcome == "HbA1c")
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

bmi_df <- low_df %>% dplyr::filter(outcome == "BMI")
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

bp_df <- low_df %>% dplyr::filter(outcome == "BP")
bp_plot <- ggplot(bp_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Blood Pressure",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- low_df %>% dplyr::filter(outcome == "LDL")
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

neph_df <- low_df %>% dplyr::filter(outcome == "Nephropathy")
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

anout5_df <- low_df %>% dplyr::filter(outcome == "Any 5 outcomes")
anout5_plot <- ggplot(anout5_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Any Outcomes",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          bp_plot,
          ldl_plot,
          neph_plot, 
          anout5_plot,
          nrow=2,
          ncol=3,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/low comorbidities modeled rates of outcomes screening by AP type.png"),width=12,height =5.5)


# medium ------------------------
medium_df <- fig_df %>% dplyr::filter(total_comorb_category == "2-3")

a1c_df <- medium_df %>% dplyr::filter(outcome == "HbA1c")
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

bmi_df <- medium_df %>% dplyr::filter(outcome == "BMI")
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

bp_df <- medium_df %>% dplyr::filter(outcome == "BP")
bp_plot <- ggplot(bp_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Blood Pressure",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- medium_df %>% dplyr::filter(outcome == "LDL")
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

neph_df <- medium_df %>% dplyr::filter(outcome == "Nephropathy")
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

anout5_df <- medium_df %>% dplyr::filter(outcome == "Any 5 outcomes")
anout5_plot <- ggplot(anout5_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Any Outcomes",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          bp_plot,
          ldl_plot,
          neph_plot, 
          anout5_plot,
          nrow=2,
          ncol=3,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/medium comorbidities modeled rates of outcomes screening by AP type.png"),width=12,height =5.5)


# high ------------------------
high_df <- fig_df %>% dplyr::filter(total_comorb_category == "4-7")

a1c_df <- high_df %>% dplyr::filter(outcome == "HbA1c")
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

bmi_df <- high_df %>% dplyr::filter(outcome == "BMI")
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

bp_df <- high_df %>% dplyr::filter(outcome == "BP")
bp_plot <- ggplot(bp_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Blood Pressure",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- high_df %>% dplyr::filter(outcome == "LDL")
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

neph_df <- high_df %>% dplyr::filter(outcome == "Nephropathy")
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

anout5_df <- high_df %>% dplyr::filter(outcome == "Any 5 outcomes")
anout5_plot <- ggplot(anout5_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Any Outcomes",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


ggarrange(a1c_plot,
          bmi_plot,
          bp_plot,
          ldl_plot,
          neph_plot, 
          anout5_plot,
          nrow=2,
          ncol=3,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/high comorbidities modeled rates of outcomes screening by AP type.png"),width=12,height =5.5)

