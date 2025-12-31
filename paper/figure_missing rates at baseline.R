
library(ggplot2)
library(patchwork)

# indicators: 1 = available at diagnosis, 0/NA = missing
indicators <- c("hba1c_ava", "bmi_ava", "ldl_ava", "nephropathy_ava")

# Calculate missing rates by ap_type
long_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>%
  # T2D diagnosis
  dplyr::filter(diff_exposure_DM == 0) %>% 
  mutate(ap_type = factor(ap_type, levels = c("Neither", "SGA", "FGA"))) %>%
  # make sure NA in indicators count as missing (treat as 0 available)
  mutate(across(all_of(indicators), ~ dplyr::coalesce(., 0))) %>%
  pivot_longer(cols = all_of(indicators),
               names_to = "outcome", values_to = "avail_dx") %>%
  group_by(ap_type, outcome) %>%
  summarise(missing_rate = (1 - mean(avail_dx)) * 100,
            .groups = "drop")

labs_out <- c(
  hba1c_ava       = "HbA1c",
  bmi_ava         = "BMI",
  ldl_ava         = "LDL cholesterol",
  nephropathy_ava = "Renal function"
)

  

fig = ggplot(long_df,
       aes(x = ap_type, y = missing_rate, fill = ap_type)) +
  geom_col(width = 0.65, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", missing_rate)),
            vjust = -0.25, size = 3) +
  facet_wrap(~ outcome, ncol = 2,
             labeller = labeller(outcome = labs_out)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")) +
  labs(x = NULL, y = "Missing rate at T2D diagnosis (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))


ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/missing rates at baseline.jpg"),width=7,height =6.6)
ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/missing rates at baseline.jpg"),width=7,height =6.6)


