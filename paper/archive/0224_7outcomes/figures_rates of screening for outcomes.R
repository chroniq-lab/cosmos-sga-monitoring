rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- bind_rows(read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03a_modeled probability of abc.csv")),
                    read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan03/sdcan03b_modeled probability of complications.csv"))) %>% 
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

bp_df <- fig_df %>% dplyr::filter(outcome == "BP")
bp_plot <- ggplot(bp_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Blood Pressures",color = "Antipsychotic Type") +
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

neur_df <- fig_df %>% dplyr::filter(outcome == "Neuropathy")
neur_plot <- ggplot(neur_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Peripheral Neuropathy",color = "Antipsychotic Type") +
  ylim(0, 0.05) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

reti_df <- fig_df %>% dplyr::filter(outcome == "Retinopathy")
reti_plot <- ggplot(reti_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Retinopathy",color = "Antipsychotic Type") +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 

ancp_df <- fig_df %>% dplyr::filter(outcome == "Any complications")
ancp_plot <- ggplot(ancp_df, aes(x = period, y = predicted, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  labs(y = "Any Complications",color = "Antipsychotic Type") +
  ylim(0, 1.0) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set1") 


library(ggpubr)

ggarrange(a1c_plot,
          bmi_plot,
          bp_plot,
          ldl_plot,
          neph_plot, 
          neur_plot,
          reti_plot,
          ancp_plot,
          nrow=2,
          ncol=4,
          common.legend = TRUE,legend = "right") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/modeled rates of outcomes screening by AP type.png"),width=12,height =5.5)


