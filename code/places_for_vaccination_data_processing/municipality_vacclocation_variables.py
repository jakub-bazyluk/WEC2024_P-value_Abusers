import pandas as pd
from math import radians, sin, cos, sqrt, atan2

locations = pd.read_csv("No_vacc_locations_latlong.csv")
muns = pd.read_csv("processed_df_modellingV2.csv")


def calculate_distance(lat1, lon1, lat2, lon2):
    # Convert latitude and longitude from degrees to radians
    lat1 = radians(lat1)
    lon1 = radians(lon1)
    lat2 = radians(lat2)
    lon2 = radians(lon2)

    # Radius of the Earth in kilometers
    R = 6371.0

    # Haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    distance = R * c

    return distance
def min_dist_to_location(row):
    long = row['Longitude']
    lat = row['Latitude']
    dist = 10000
    for index, location in locations.iterrows():
        loc_long = location['Longitude']
        loc_lat = location['Latitude']
        dist_new = calculate_distance(lat, long, loc_lat, loc_long)
        dist = min(dist, dist_new)
    return round(dist,2)

def no_locations_in10km(row):
    long = row['Longitude']
    lat = row['Latitude']
    no_loc = 0
    for index, location in locations.iterrows():
        loc_long = location['Longitude']
        loc_lat = location['Latitude']
        dist_new = calculate_distance(lat, long, loc_lat, loc_long)
        if(dist_new <= 10):
            no_loc = no_loc + location['count']
    return no_loc

muns['min_dist'] = muns.apply(min_dist_to_location, axis = 1)
muns['no_loc_in10km'] = muns.apply(no_locations_in10km, axis = 1)

muns.to_csv("processed_df_modellingV3.csv")

