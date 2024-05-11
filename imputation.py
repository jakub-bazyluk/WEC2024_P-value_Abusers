import pandas as pd
import numpy as np
from sklearn.impute import KNNImputer

data = pd.read_csv("processed_df_modelling.csv")

print(data.isna().sum().sort_values().tail(20))

data_health = data[['Longitude', 'Latitude', 'healthcare_advices_ratio_total']].values
data_forests_ratio = data[['Longitude', 'Latitude', 'forests_area_ratio_area_km2']].values
data_forests_area = data[['Longitude', 'Latitude', 'forests_area']].values
data_kinder = data[['Longitude', 'Latitude', 'children_3_5_in_kindergartens']].values
data_tourists = data[['Longitude', 'Latitude', 'tourits_per_1000_persons']].values
imputer = KNNImputer(n_neighbors=3)
data_health = imputer.fit_transform(data_health)
data_forests_ratio = imputer.fit_transform(data_forests_ratio)
data_forests_area = imputer.fit_transform(data_forests_area)
data_kinder = imputer.fit_transform(data_kinder)
data_tourists = imputer.fit_transform(data_tourists)

data[['Longitude', 'Latitude', 'healthcare_advices_ratio_total']] = data_health
data[['Longitude', 'Latitude', 'forests_area_ratio_area_km2']] = data_forests_ratio
data[['Longitude', 'Latitude', 'forests_area']] = data_forests_area
data[['Longitude', 'Latitude', 'children_3_5_in_kindergartens']] = data_kinder
data[['Longitude', 'Latitude', 'tourits_per_1000_persons']] = data_tourists

print(data.isna().sum().sort_values().tail(20))

data.to_csv("processed_df_modellingV2.csv")