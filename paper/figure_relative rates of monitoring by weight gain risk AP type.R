rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan06/sdcan06_model by weight gain risk AP type.csv")) %>% 
  rename(ap_type_wgr = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100) %>% 
  separate(ap_type_wgr, into = c("wg_risk", "ap_type"), sep = "_", fill = "right") %>%
  mutate(
    ap_type = case_when(
      is.na(ap_type) ~ "Neither",
      TRUE ~ toupper(ap_type)
    ),
    wg_risk = str_to_title(wg_risk)  # Capitalize first letter: "low" -> "Low"
  ) %>% 
  mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA")),
         wg_risk = factor(wg_risk,levels=c("None","Low","Intermediate","High"),labels=c("None","Low","Intermediate","High")))


ap_colors <- c("Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")

# Low risk ------------------------
low_df <- fig_df %>% dplyr::filter(wg_risk == "None" | wg_risk == "Low")

a1c_df <- low_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

bmi_df <- low_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

ldl_df <- low_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

neph_df <- low_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of monitoring of low weight gain risk.png"),width=7,height =6)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/relative rates of monitoring of low weight gain risk.jpg"),width=7,height =6)


# Intermediate risk ------------------------
intermediate_df <- fig_df %>% dplyr::filter(wg_risk == "None" | wg_risk == "Intermediate")

a1c_df <- intermediate_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

bmi_df <- intermediate_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

ldl_df <- intermediate_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

neph_df <- intermediate_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of monitoring of intermediate weight gain risk.png"),width=7,height =6)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/relative rates of monitoring of intermediate weight gain risk.jpg"),width=7,height =6)


# high risk ------------------------
high_df <- fig_df %>% dplyr::filter(wg_risk == "None" | wg_risk == "High")

a1c_df <- high_df %>% dplyr::filter(outcome == "HbA1c")
a1c_plot <- ggplot(a1c_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

bmi_df <- high_df %>% dplyr::filter(outcome == "BMI")
bmi_plot <- ggplot(bmi_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

ldl_df <- high_df %>% dplyr::filter(outcome == "LDL")
ldl_plot <- ggplot(ldl_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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

neph_df <- high_df %>% dplyr::filter(outcome == "Nephropathy")
neph_plot <- ggplot(neph_df, aes(x = period, y = probability, group = ap_type, color = ap_type)) +
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
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/relative rates of monitoring of high weight gain risk.png"),width=7,height =6)

ggarrange(a1c_plot,
          bmi_plot,
          ldl_plot,
          neph_plot, 
          nrow=2,
          ncol=2,
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/relative rates of monitoring of high weight gain risk.jpg"),width=7,height =6)


