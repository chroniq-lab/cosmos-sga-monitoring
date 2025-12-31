rm(list=ls());gc();source(".Rprofile")

library(stringr)

# rates of monitoring
fig_df <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan05/sdcan05e_model5 with dx_rx, cci category and encounters.csv")) %>% 
  rename(ap_type = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA"))) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100)


output_labels <- c(
  "HbA1c" = "HbA1c",
  "BMI" = "Weight",
  "LDL" = "LDL cholesterol",
  "Nephropathy" = "Renal function"
)

formatted_df <- fig_df %>%
  mutate(
    outcome = recode(outcome, !!!output_labels),
    year = case_when(
      period == "Y1" ~ "Year 1",
      period == "Y2" ~ "Year 2",
      period == "Y3" ~ "Year 3",
      TRUE ~ period
    ),
    mean_ci = sprintf("%.1f (%.1f, %.1f)", probability, ci_low, ci_high)
  ) %>%
  select(outcome, ap_type, year, mean_ci)

final_df <- formatted_df %>%
  pivot_wider(
    names_from = year,
    values_from = mean_ci
  ) %>%
  arrange(factor(outcome, levels = c("HbA1c", "Weight", "LDL cholesterol", "Renal function")),
          factor(ap_type, levels = c("Neither", "SGA", "FGA"))) %>%
  rename(
    `Outcome` = outcome,
    `Antipsychotic type` = ap_type
  )

write.csv(final_df, "paper/table_relative rates of monitoring.csv", row.names = FALSE)


# rates of control ------------------------------------------------------------------------
fig_df_ctrl <- read_csv(paste0(path_sga_dm_control_folder,"/working/sdcan07/sdcan07_model rates of control with ipw.csv")) %>% 
  rename(ap_type = x,period = group,ci_low = conf.low, ci_high = conf.high) %>% 
  mutate(ap_type = factor(ap_type,levels=c("Neither","SGA","FGA"),labels=c("Neither","SGA","FGA"))) %>% 
  mutate(probability = predicted *100,
         ci_low = ci_low*100,
         ci_high = ci_high*100)


formatted_df <- fig_df_ctrl %>%
  mutate(
    year = case_when(
      period == "Y1" ~ "Year 1",
      period == "Y2" ~ "Year 2",
      period == "Y3" ~ "Year 3",
      TRUE ~ period
    ),
    mean_ci = sprintf("%.1f (%.1f, %.1f)", probability, ci_low, ci_high)
  ) %>%
  select(outcome, ap_type, year, mean_ci)

final_df_ctrl <- formatted_df %>%
  pivot_wider(
    names_from = year,
    values_from = mean_ci
  ) %>%
  arrange(factor(outcome, levels = c("HbA1c Control IPW", "BP Control IPW", "LDL Control IPW", "ABC Control IPW")),
          factor(ap_type, levels = c("Neither", "SGA", "FGA"))) %>%
  rename(
    `Outcome` = outcome,
    `Antipsychotic type` = ap_type
  )

write.csv(final_df_ctrl, "paper/table_relative rates of control with ipw.csv", row.names = FALSE)

