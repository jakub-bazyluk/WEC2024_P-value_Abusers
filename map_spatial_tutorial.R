library(dplyr)
library(sf)
library(sfdep)
library(sp)
library(spdep)
library(readr)
library(ggplot2)
library(ggthemes)

# import data for municipalities

data_municipalities <- read_csv("C:/Users/Jakub/Documents/WEC2024_P-value_Abusers/startup/_data/data_municipalities.csv")

# import a shapefile for municipalities

map_municipalities <- st_read("C:/Users/Jakub/Documents/WEC2024_P-value_Abusers/startup/_data/shapefile/map_municipalities.shp")

data_municipalities$geometry <- map_municipalities$geometry
# plot the borders of municipalities

plot(st_geometry(map_municipalities))

# check if municipality codes in the datafile and the map are the same

which(!map_municipalities$mncplty_c %in% data_municipalities$municipality_code)
which(!data_municipalities$municipality_code %in% map_municipalities$mncplty_c )

# get longitude and latitude
mean(map_municipalities$geometry)

centroids <- st_centroid(map_municipalities$geometry)
centroid_coords <- st_coordinates(centroids)

# Create a dataframe with centroid coordinates
data_municipalities$Longitude <- centroid_coords[, "X"]
data_municipalities$Latitude <- centroid_coords[, "Y"]
# OK - all are the same
data_municipalities$Longitude
# lets add the data to the map object

map_municipalities_with_data <- 
  map_municipalities %>% 
  left_join(data_municipalities, 
            by = join_by(mncplty_c == municipality_code))

# and make a visualization for a selected variable -- e.g. percent_vaccinated

ggplot(map_municipalities_with_data, 
       aes(fill = percent_vaccinated)) +
  geom_sf() +
  ggthemes::theme_map() +
  scale_fill_continuous()

#-------------------------------------------
# import a shapefile for historical partitions of Poland

map_partitions <- st_read("C:/Users/Jakub/Documents/WEC2024_P-value_Abusers/startup/_data/shapefile/map_partitions.shp")

data_municipalities <- mutate(data_municipalities, within_russian = st_contains(st_buffer(map_partitions$geometry[3], dist = 0.1),data_municipalities$geometry, sparse = FALSE)[,1])
data_municipalities <- mutate(data_municipalities, within_prussian = st_contains(st_buffer(map_partitions$geometry[2], dist = 0.1),data_municipalities$geometry, sparse = FALSE)[,1])
data_municipalities <- mutate(data_municipalities, within_austrian = st_contains(st_buffer(map_partitions$geometry[1], dist = 0.1),data_municipalities$geometry, sparse = FALSE)[,1])

# check if correct onehot encoding
sum(data_municipalities$within_austrian + data_municipalities$within_prussian + data_municipalities$within_russian)

map_municipalities_with_data <- 
  map_municipalities %>% 
  left_join(data_municipalities, 
            by = join_by(mncplty_c == municipality_code))

# lets plot historical borders between partitions

ggplot() +
  geom_sf(data = map_partitions, 
          aes(fill = partition)) +
  ggthemes::theme_map()
  
# lets try to overlay the two maps

ggplot() +
  geom_sf(data = map_municipalities_with_data, 
          aes(fill = percent_vaccinated)) +
  ggthemes::theme_map() +
  scale_fill_continuous() +
  geom_sf(data = map_partitions, 
          size = 2, 
          color = "red",
          fill = NA)

# Test if works
ggplot() +
  geom_sf(data = map_municipalities_with_data, 
          aes(fill = factor(within_austrian))) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Outside Russia", "Within Russia")) +
  geom_sf(data = map_partitions, 
          size = 2, 
          color = "red",
          fill = NA)

map_municipalities_with_data$within_russian

#---------------------------------------------------------------
# creating spatial weight matrix - contiguity (i.e. common border)
municipalities_neighbours <- sfdep::st_contiguity(map_municipalities)

# lets check its structure
glimpse(municipalities_neighbours)

# converting a list of neighbours into a matrix
spatial_weights <- nb2mat(municipalities_neighbours)

# you can check if it is row standardized by default
summary(rowSums(spatial_weights))

# OK

# based on that we can create a variable which is a spatial lag of 
# percent vaccinated (i.e. average value based on the neighbours of 
# each municipality)

map_municipalities_with_data$splag_percent_vaccinated <- 
  spatial_weights %*% as.matrix(map_municipalities_with_data$percent_vaccinated)


# Write spatial file

df <- as.data.frame(map_municipalities_with_data)
df <- df[,c("mncplty_c", "Longitude", "Latitude", "within_russian", "within_prussian", "within_austrian")]
write.csv(df, "spatial_municipality_corrected2.csv", row.names = TRUE)
