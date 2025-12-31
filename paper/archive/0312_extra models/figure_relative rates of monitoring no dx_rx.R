rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05a_model1 no dx_rx.csv")) %>% 
  rename(ap_type = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("None","SGA","FGA"),labels=c("None","SGA","FGA"))) 

a1c_df <- fig_df %>% dplyr::filter(outcome == "HbA1c")
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

bmi_df <- fig_df %>% dplyr::filter(outcome == "BMI")
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

ldl_df <- fig_df %>% dplyr::filter(outcome == "LDL")
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

neph_df <- fig_df %>% dplyr::filter(outcome == "Nephropathy")
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


library(ggpubr)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of monitoring no dx_rx.png"),width=8,height =6)



