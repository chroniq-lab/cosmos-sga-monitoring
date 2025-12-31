library(dplyr)
library(tidyr)
library(ggplot2)

# outcomes and nicer facet labels
outcomes <- c("mean_hba1c", "mean_sbp", "mean_bmi", "mean_ldl")
outcome_labs <- c(
  mean_hba1c = "HbA1c (%, mean)",
  mean_sbp   = "SBP (mmHg, mean)",
  mean_bmi   = "BMI (kg/m?, mean)",
  mean_ldl   = "LDL (mg/dL, mean)"
)

df_long <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS"))  %>%
  mutate(ap_type = factor(ap_type, levels = c("Neither", "SGA", "FGA"))) %>%
  pivot_longer(cols = all_of(outcomes),
               names_to = "outcome",
               values_to = "value")

fig <- ggplot(df_long, aes(x = ap_type, y = value, fill = ap_type)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.3, color = "black") + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2,
               fill = "white", color = "black") + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_y",
             labeller = labeller(outcome = outcome_labs)) +
  scale_fill_manual(values = c("Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/boxplot of mean outcome values by AP type.png"),width=7,height =6)
ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/boxplot of mean outcome values by AP type.jpg"),width=7,height =6)

