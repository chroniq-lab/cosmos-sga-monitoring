rm(list=ls());gc();source(".Rprofile")

enc_by_type <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  group_by(ap_type) %>%
  summarise(
    total_enc_Y1 = sum(N_enc_Y1, na.rm = TRUE),
    total_enc_Y2 = sum(N_enc_Y2, na.rm = TRUE),
    total_enc_Y3 = sum(N_enc_Y3, na.rm = TRUE)
  ) %>%
  mutate(
    prop_Y1 = total_enc_Y1 / sum(total_enc_Y1),
    prop_Y2 = total_enc_Y2 / sum(total_enc_Y2),
    prop_Y3 = total_enc_Y3 / sum(total_enc_Y3)
  ) %>% 
  mutate(cci_category = "overall") 

enc_by_type1 <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  dplyr::filter(cci_category == "mild") %>% 
  group_by(ap_type) %>%
  summarise(
    total_enc_Y1 = sum(N_enc_Y1, na.rm = TRUE),
    total_enc_Y2 = sum(N_enc_Y2, na.rm = TRUE),
    total_enc_Y3 = sum(N_enc_Y3, na.rm = TRUE)
  ) %>%
  mutate(
    prop_Y1 = total_enc_Y1 / sum(total_enc_Y1),
    prop_Y2 = total_enc_Y2 / sum(total_enc_Y2),
    prop_Y3 = total_enc_Y3 / sum(total_enc_Y3)
  ) %>% 
  mutate(cci_category = "mild") 

enc_by_type2 <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  dplyr::filter(cci_category == "moderate") %>% 
  group_by(ap_type) %>%
  summarise(
    total_enc_Y1 = sum(N_enc_Y1, na.rm = TRUE),
    total_enc_Y2 = sum(N_enc_Y2, na.rm = TRUE),
    total_enc_Y3 = sum(N_enc_Y3, na.rm = TRUE)
  ) %>%
  mutate(
    prop_Y1 = total_enc_Y1 / sum(total_enc_Y1),
    prop_Y2 = total_enc_Y2 / sum(total_enc_Y2),
    prop_Y3 = total_enc_Y3 / sum(total_enc_Y3)
  ) %>% 
  mutate(cci_category = "moderate") 

enc_by_type3 <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_imputed dataset with billing codes by AP type.RDS")) %>% 
  dplyr::filter(cci_category == "severe") %>% 
  group_by(ap_type) %>%
  summarise(
    total_enc_Y1 = sum(N_enc_Y1, na.rm = TRUE),
    total_enc_Y2 = sum(N_enc_Y2, na.rm = TRUE),
    total_enc_Y3 = sum(N_enc_Y3, na.rm = TRUE)
  ) %>%
  mutate(
    prop_Y1 = total_enc_Y1 / sum(total_enc_Y1),
    prop_Y2 = total_enc_Y2 / sum(total_enc_Y2),
    prop_Y3 = total_enc_Y3 / sum(total_enc_Y3)
  ) %>% 
  mutate(cci_category = "severe")

fig_df <- bind_rows(enc_by_type,
                    enc_by_type1,
                    enc_by_type2,
                    enc_by_type3) %>% 
  pivot_longer(
    cols = c(total_enc_Y1, total_enc_Y2, total_enc_Y3, prop_Y1, prop_Y2, prop_Y3),
    names_to = c(".value", "period"),
    names_pattern = "(.*)_(Y[123])"
  ) %>% 
  mutate(prop = prop*100,
         period = factor(period, levels=c("Y1","Y2","Y3"), labels=c("Year 1","Year 2","Year 3")),
         ap_type = factor(ap_type, levels=c("SGA","Not_SGA","Other"), labels=c("SGA","FGA","Neither")),
         cci_category = factor(cci_category, levels=c("overall","mild","moderate","severe"), labels=c("overall","mild","moderate","severe")))


library(ggplot2)
library(dplyr)
library(patchwork)

# proportion of encounters --- not informative
p <- fig_df %>%
  ggplot(aes(x = ap_type, y = prop, fill = period)) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  facet_wrap(~ cci_category, scales = "free_y", labeller = labeller(cci_category = c(overall = "Overall", mild = "Mild", moderate = "Moderate", severe = "Severe"))) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = NULL, y = "Percentage of Encounters (%)", fill = NULL) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), # Adjust title size
        axis.title = element_text(size = 14), # Adjust x and y axis titles size
        axis.text = element_text(size = 12), # Adjust axis text size
        strip.text = element_text(size = 12)) 

final_plot <- p + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(final_plot,filename=paste0(path_sga_dm_control_folder,"/figures/percentage of encounters.png"),width=7,height =6)



# boxplot of total number of encounters --------------------------------------

analytic_df <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan_longitudinal analytic dataset.RDS")) %>% 
  select(PatientDurableKey,N_enc,period,ap_type,cci_category) %>% 
  mutate(period = factor(period, levels=c("Y1","Y2","Y3"), labels=c("Year 1","Year 2","Year 3")),
         ap_type = factor(ap_type, levels=c("SGA","Not_SGA","Other"), labels=c("SGA","FGA","Neither")),
         cci_category = factor(cci_category, levels=c("overall","mild","moderate","severe"), labels=c("Overall","Mild","Moderate","Severe")))


p <- ggplot(analytic_df, aes(x=period, y=N_enc, fill=period)) +
  geom_boxplot() +
  facet_wrap(~ap_type, scales="free") +
  labs(x="",
       y="Total number of unique encounters") +
  theme_minimal() +
  ylim(0, 100) 

# Adjust layout to be 2x2
p + theme(strip.background = element_blank(),
          strip.text.x = element_text(size=10))


