rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- bind_rows(read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan04/sdcan04a_modeled unadjusted probability of 5 outcomes.csv"))) %>% 
  rename(ap_type = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("None","SGA","FGA"),labels=c("Neither","SGA","FGA"))) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100)

a1c_df <- fig_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "HbA1c monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

bmi_df <- fig_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Weight monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- fig_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "LDL monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

neph_df <- fig_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(,y = "Renal function monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 



library(ggpubr)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/crude rates of outcomes screening by AP type.png"),width=7,height =6)


