import pandas as pd
from geopy.geocoders import Nominatim

df = pd.read_excel("Vaccination_locations_2021_March.xlsx")[['Miasto']]
df['Miasto'] = df['Miasto'].str.lower()
df = df.value_counts().reset_index()

GEOLOCATOR = Nominatim(user_agent="my_geocoder")
def get_latlong(row):
    name = row['Miasto'] + ' Polska'
    location = GEOLOCATOR.geocode(name, timeout=10)
    if location:
        row['Latitude'] = location.latitude
        row['Longitude'] = location.longitude
        print(name)
    else:
        print(f"Location {name} not found!")
    return row

df = df.apply(get_latlong, axis = 1)
df.to_csv("No_vacc_locations_latlong.csv", index = False)