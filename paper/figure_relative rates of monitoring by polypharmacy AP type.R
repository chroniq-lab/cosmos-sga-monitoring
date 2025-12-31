rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan08/sdcan08_model by polypharmacy AP type.csv")) %>% 
  rename(ap_type_polypharmacy = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type_polypharmacy = factor(ap_type_polypharmacy,levels=c("None","Only_SGA","Only_FGA","Both"),
                                       labels=c("None","Only_SGA","Only_FGA","Both"))) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100)

ap_colors <- c("None" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A", "Both" = "#800080")

a1c_df <- fig_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type_polypharmacy, color = ap_type_polypharmacy)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "HbA1c monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

bmi_df <- fig_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = probability, group = ap_type_polypharmacy, color = ap_type_polypharmacy)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "Weight monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

ldl_df <- fig_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type_polypharmacy, color = ap_type_polypharmacy)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "LDL cholesterol monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA, size = 0.5)) +
  scale_color_brewer(palette = "Set1") 

neph_df <- fig_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = probability, group = ap_type_polypharmacy, color = ap_type_polypharmacy)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(,y = "Renal function monitoring (%)",color = "Antipsychotic Type") +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
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
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of monitoring by polypharmacy AP type.png"),width=7,height =6)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/relative rates of monitoring by polypharmacy AP type.jpg"),width=7,height =6)



