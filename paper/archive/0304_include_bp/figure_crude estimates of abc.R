rm(list=ls());gc();source(".Rprofile")


analytic_dataset <- readRDS(paste0(path_sga_dm_control_folder,"/working/sdcan01_analytic dataset.RDS"))


measured = analytic_dataset %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black",
                                                         "Hispanic","NH Other"))) %>% 
  mutate(across(matches("n_(hba1c|sbp|ldl)"),~case_when(is.na(.) ~ 0,
                                                        TRUE ~ .))) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(raceeth = "Total")) %>% 
  group_by(raceeth,type) %>% 
  
  
  summarize(across(matches("^n_(hba1c|sbp|ldl)"),.fns=list(any = ~mean(.>0)),
                .names="{fn}_{col}")) %>% 
  pivot_longer(cols=-one_of("raceeth","type"),names_to="variable",values_to="prop_category")



control = analytic_dataset %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NH White","NH Black",
                                                         "Hispanic","NH Other"))) %>% 
  mutate(across(matches("mean_hba1c"),~case_when(is.na(.) ~ "Unknown",
                                                 . >= 8 ~ "Uncontrolled",
                                                        TRUE ~ "Controlled"))) %>% 
  mutate(mean_bp_Y1 = case_when(is.na(mean_sbp_Y1) | is.na(mean_dbp_Y1) ~ "Unknown",
                                mean_sbp_Y1 > 140 | mean_dbp_Y1 > 90 ~ "Uncontrolled",
                                TRUE ~ "Controlled"),
         mean_bp_Y2 = case_when(is.na(mean_sbp_Y2) | is.na(mean_dbp_Y2) ~ "Unknown",
                                mean_sbp_Y2 > 140 | mean_dbp_Y2 > 90 ~ "Uncontrolled",
                                TRUE ~ "Controlled"),
         mean_bp_Y3 = case_when(is.na(mean_sbp_Y3) | is.na(mean_dbp_Y3) ~ "Unknown",
                                mean_sbp_Y3 > 140 | mean_dbp_Y3 > 90 ~ "Uncontrolled",
                                TRUE ~ "Controlled"),
         ) %>% 
  mutate(across(matches("mean_ldl"),~case_when(is.na(.) ~ "Unknown",
                                                 . >= 100 ~ "Uncontrolled",
                                                        TRUE ~ "Controlled"))) %>%
  dplyr::select(raceeth,type,matches("mean_(hba1c|bp|ldl)")) %>% 
  pivot_longer(cols=-one_of("raceeth","type"),names_to="variable",values_to="category") %>% 
  bind_rows(.,
            {.} %>% 
              mutate(raceeth = "Total")) %>% 
  group_by(raceeth,type,variable,category) %>%
  tally() %>% 
  ungroup() %>% 
  group_by(raceeth,type,variable) %>% 
  mutate(prop_category = n/sum(n)) %>% 
  ungroup()
  
  
# Figure. Proportion measured -------------------

fig_measured = measured %>% 
  mutate(raceeth = factor(raceeth,levels=c("Total","NH White","NH Black","Hispanic","NH Other")),
         type = factor(type,levels=c("SGA","Not_SGA"),labels=c("SGA","FGA")),
         year = case_when(str_detect(variable,"Y1") ~ "Year 1",
                          str_detect(variable,"Y2") ~ "Year 2",
                          str_detect(variable,"Y3") ~ "Year 3",
                          TRUE ~ NA_character_
                          ),
         biomarker = case_when(str_detect(variable,"hba1c") ~ 1,
                               str_detect(variable,"sbp") ~ 2,
                               str_detect(variable,"ldl") ~ 3,
                               TRUE ~ NA_real_)) %>% 
  mutate(biomarker = factor(biomarker,levels=c(1:3),labels=c("HbA1c","Blood Pressure","LDL"))) %>% 
  
  ggplot(data=.,aes(x=biomarker,y=round(prop_category*100,1),label=round(prop_category*100,1),fill=raceeth)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(y = round(prop_category*100,1) + 2),position = position_dodge(width = 0.9),size=2.5) +
  facet_grid(type~year) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "") +
  xlab("") +
  ylab("Percentage (%)")

fig_measured %>% 
  ggsave(.,filename = paste0(path_sga_dm_control_folder,"/figures/proportion measured.jpg"),width = 12,height=6)
  
  
# Figure. Proportion controlled -------------------

fig_control = control %>% 
  mutate(raceeth = factor(raceeth,levels=c("Total","NH White","NH Black","Hispanic","NH Other")),
         type = factor(type,levels=c("SGA","Not_SGA"),labels=c("SGA","FGA")),
         year = case_when(str_detect(variable,"Y1") ~ "Year 1",
                          str_detect(variable,"Y2") ~ "Year 2",
                          str_detect(variable,"Y3") ~ "Year 3",
                          TRUE ~ NA_character_
         ),
         biomarker = case_when(str_detect(variable,"hba1c") ~ 1,
                               str_detect(variable,"bp") ~ 2,
                               str_detect(variable,"ldl") ~ 3,
                               TRUE ~ NA_real_)) %>% 
  mutate(biomarker = factor(biomarker,levels=c(1:3),labels=c("HbA1c","Blood Pressure","LDL"))) %>% 
  dplyr::filter(raceeth == "Total") %>% 
  arrange(year,biomarker,type,desc(category)) %>% 
  group_by(year,biomarker,type) %>% 
  mutate(prop_category_labelpos = cumsum(prop_category)) %>% 
  ungroup() %>% 
  
  ggplot(data=.,aes(x=biomarker,y=round(prop_category*100,1),
                    label = round(prop_category*100,1),fill=category)) +
  geom_col() +
  geom_text(aes(y=round(prop_category_labelpos*100,1)-1)) +
  facet_grid(type~year) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "") +
  xlab("") +
  ylab("Percentage (%)")

fig_control %>% 
  ggsave(.,filename = paste0(path_sga_dm_control_folder,"/figures/proportion control.jpg"),width = 12,height=6)



