rm(list=ls());gc();source(".Rprofile")

# column plot with CI --------------------------------------
summary_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>%
  select(PatientDurableKey,N_enc,period,ap_type) %>% 
  group_by(ap_type, period) %>%
  summarise(
    mean_enc = mean(N_enc, na.rm = TRUE),
    sd_enc = sd(N_enc, na.rm = TRUE),
    n = n(),
    se = sd_enc / sqrt(n),
    lower_ci = mean_enc - 1.96 * se,
    upper_ci = mean_enc + 1.96 * se
  ) %>%
  ungroup() %>% 
  mutate(period = factor(period, levels=c("Y1","Y2","Y3"), labels=c("Year 1","Year 2","Year 3")),
         ap_type = factor(ap_type, levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA")))

ap_colors <- c("Neither" = "#E41A1C", "SGA" = "#377EB8", "FGA" = "#4DAF4A")

fig <- ggplot(summary_df, aes(x = period, y = mean_enc, fill = ap_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = ap_colors, name = "Treatment", labels = c("Neither", "SGA", "FGA")) +
  facet_wrap(~ap_type, scales = "fixed") +
  labs(x = NULL,
       y = "Average number of in-person encounters") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),  
    strip.text.x = element_text(size = 18, face = "bold"),  
    strip.text.y = element_text(size = 16),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 18),
    legend.title = element_blank(), 
    legend.position = "none",
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) 

ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/column plot for total number of encounters.png"),width=8,height =6)
ggsave(fig,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/column plot for total number of encounters.jpg"),width=8,height =6)


