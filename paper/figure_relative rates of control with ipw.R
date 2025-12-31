rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07_model rates of control with ipw.csv")) %>% 
  rename(ap_type = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA"))) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100)

ap_colors <- c("Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")

a1c_df <- fig_df %>% dplyr::filter(outcome == "HbA1c Control IPW")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "HbA1c <= 8%",color = "Antipsychotic Type") +
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

ldl_df <- fig_df %>% dplyr::filter(outcome == "LDL Control IPW")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "LDL <= 100 mg/dL",color = "Antipsychotic Type") +
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



bp_df <- fig_df %>% dplyr::filter(outcome == "BP Control IPW")
bp_plot <- ggplot(bp_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "Blood Pressure <= 140/90 mmHg",color = "Antipsychotic Type") +
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


abc_df <- fig_df %>% dplyr::filter(outcome == "ABC Control IPW")
abc_plot <- ggplot(abc_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
  geom_line() +  
  geom_point() +  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  scale_color_manual(values = ap_colors) + 
  labs(y = "ABC Control",color = "Antipsychotic Type") +
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
          bp_plot, 
          ldl_plot,
          abc_plot,
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of control with ipw.jpg"),width=7,height =6.6)



