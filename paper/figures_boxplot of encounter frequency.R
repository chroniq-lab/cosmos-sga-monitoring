rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

boxplot_data <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>%
  mutate(ap_type = factor(ap_type, levels = c("Neither", "SGA", "FGA"))) %>%
  select(ap_type, CCI_score) %>%
  bind_rows(
    .,
    mutate(., ap_type = "Total")
  ) %>%
  mutate(ap_type = factor(ap_type, levels = c("Total", "Neither", "SGA", "FGA")))

ap_colors <- c("Total" = "#999999", "Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")

boxplot <- ggplot(boxplot_data, aes(x = ap_type, y = CCI_score, fill = ap_type)) +
  geom_boxplot() +
  scale_fill_manual(values = ap_colors) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  labs(x = NULL, y = "Age-adjusted Charlson Comorbidity Index") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5)
  )


ggsave(boxplot,filename=paste0(path_sga_dm_control_folder,"/figures/boxplot of ACCI by AP type.png"),width=8,height =6)
ggsave(boxplot,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/boxplot of ACCI by AP type.jpg"),width=8,height =6)


