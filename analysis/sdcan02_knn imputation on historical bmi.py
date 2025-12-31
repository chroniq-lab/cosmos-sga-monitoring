import numpy as np
import pandas as pd
from sklearn.impute import KNNImputer
import os

path_sga_dm_control_folder = "Z:/Project D5A613/COSMOS SGA T2DM Control"

data_mi = pd.read_csv(path_sga_dm_control_folder + '/working/sdcan_analytic dataset with AP type.csv')

selected_variables = ["PatientDurableKey","ap_type","exposure_datekey","age","year","bmi_history","diff_exposure_DM"]

#drop missing values in the selected variables
data_mi = data_mi[selected_variables]
data_mi.shape

columns_to_impute = ["bmi_history"]

def impute_study_data(data, n_neighbors=5):
    imputer = KNNImputer(n_neighbors=n_neighbors)
    data[columns_to_impute] = imputer.fit_transform(data[columns_to_impute])
    return data
# Impute data for each AP type
ap_types = data_mi['ap_type'].unique()
imputed_datasets = []

for site in ap_types:
    site_data = data_mi[data_mi['ap_type'] == site].copy()
    imputed_data = impute_study_data(site_data)
    imputed_datasets.append(imputed_data)
    
# Merge all imputed datasets back to one
imputed_data_merged = pd.concat(imputed_datasets)


# Check if there are any missing values
print(imputed_data_merged.isnull().sum())
print(data_mi.isnull().sum())
# compare the imputed data with the original data
data_mi.describe()
imputed_data_merged.describe()

# save the imputed data
imputed_data_merged.to_csv(path_sga_dm_control_folder + '/working/sdcan_knn imputation on bmi history.csv', index=False)


