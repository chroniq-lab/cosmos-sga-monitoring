rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
library(reshape2)

comorbidities <- data.frame(
  Comorbidity = c(
    "AIDS/HIV", "Any malignancy", "Cerebrovascular disease", "Chronic pulmonary disease", 
    "Congestive heart failure", "Dementia", "Diabetes with chronic complication", 
    "Diabetes without chronic complication", "Hemiplegia or paraplegia", "Metastatic solid tumor", 
    "Mild liver disease", "Moderate or severe liver disease", "Myocardial infarction", 
    "Peptic ulcer disease", "Peripheral vascular disease", "Renal disease", "Rheumatic disease", 
    "Cardiovascular", "Hyperlipidemia", "Hypertension", "Polycystic Ovarian Syndrome", "Obesity", 
    "Bipolar disorder", "Delusional disorders", "Major depressive disorder, recurrent", 
    "Psychosis Unspecified", "Schizoaffective disorders", "Schizophrenia", "Schizotypal disorder"
  ),
  Total =   c(0.2, 6.1,  5.2,  13.5, 10.9, 2.2,  17.5, 60.0, 1.1, 1.7, 3.9, 0.7, 6.0, 
              0.6, 6.4,  12.4, 1.8,  23.3, 48.0, 41.7, 0.3,  18.6, 1.4,  0.1, 9.4,  0.3, 0.4, 0.5, 0),
  Neither = c(0.2, 5.3,  4.5,  11.8, 9.6,  1.2,  16.2, 59.5, 0.8, 1.3, 3.4, 0.5, 5.3, 
              0.5, 5.8,  11.1, 1.6,  21.4, 46.8, 41.0, 0.4,  17.7, 0.5,  0,   7.6,  0.1, 0.1, 0.1, 0),
  SGA =     c(0.4, 9.3,  10.7, 26.7, 19.6, 11.3, 25.7, 62.5, 2.6, 3.2, 6.7, 1.6, 10.1, 
              1.4, 9.9,  20.1, 2.7,  34.9, 55.0, 44.8, 0.6,  23.9, 12.9, 1.3, 27.4, 2.3, 4.2, 4.7, 0),
  FGA =     c(0.2, 17.2, 11.2, 27.0, 24.2, 8.0,  31.2, 66.5, 2.7, 7.4, 9.2, 2.7, 13.4, 
              1.8, 13.3, 26.1, 3.1,  43.5, 63.3, 51.8, 0.6,  28.7, 2.4,  0.5, 18.8, 0.8, 1.0, 1.6, 0)
) %>% 
  mutate(Comorbidity = factor(Comorbidity, levels = rev(unique(Comorbidity))))

comorbidities$Comorbidity <- factor(comorbidities$Comorbidity, levels = rev(unique(comorbidities$Comorbidity)))
comorbidities_melted <- melt(comorbidities, id.vars = "Comorbidity")

p <- ggplot(comorbidities_melted, aes(x = variable, y = Comorbidity, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "peachpuff", high = "red4", name = "Proportion (%)") +
  geom_text(aes(label = sprintf("%.1f%%", value)), size = 5, color = "black", vjust = 0.5) +
  labs( x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 14), 
        axis.text.y = element_text(size = 13), 
        legend.position = "bottom", 
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 12),  
        text = element_text(size = 12),
        legend.margin = margin(t = -14, b = 0, unit = "pt"))

ggsave(p,filename=paste0(path_sga_dm_control_folder,"/figures/heatmap for proportion of comorbidities.png"),width=8,height =6)
ggsave(p,filename=paste0(path_sga_dm_control_folder,"/figures/jpg/heatmap for proportion of comorbidities.jpg"),width=8,height =6)


