# Function to calculate Charlson Comorbidity Index (CCI)
calculate_cci <- function(df) {
  # Assign weights to each comorbidity
  weights <- c(
    myocardial_infarction_dx = 1,
    heart_failure_dx = 1,
    peripheral_vascular_dx = 1, 
    cerebrovascular_dx = 1,
    dementia_dx = 1,
    pulmonary_dx = 1, 
    rheumatic_dx = 1,
    peptic_ulcer_dx = 1,
    mild_liver_dx = 1,
    diabetes_dx = 1,
    diabetes_complication_dx = 2,
    hemiplegia_dx = 2,
    renal_dx = 2, 
    malignancy_dx = 2,
    modsev_liver_dx = 3,
    metastatic_tumor_dx = 6, 
    aids_hiv_dx = 6
  )
  
  # Add age points: 1 point for each decade over 40
  df$age_points <- pmax(0, (df$age - 40) %/% 10)
  
  # Calculate total CCI score: Sum of comorbidity weights + age points
  # Only select columns that are present in both the dataframe and the weights list
  comorbidity_columns <- intersect(names(weights), names(df))
  df$CCI_score <- rowSums(df[, comorbidity_columns] * weights[comorbidity_columns], na.rm = TRUE) + df$age_points
  
  return(df)
}

