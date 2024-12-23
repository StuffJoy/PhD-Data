devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/aggregate-function")
install.packages("tmap")

library(mosquitoR)
library(dplyr)
library(sf)

library(tmap)
library(ggplot2)

#names(malert_geopackage_data)
#table(malert_geopackage_data$movelab_annotation.classification)

#Pulls mosquito alert data and performs spatial join
malert_geopackage_data = get_malert_geopackage_data("2014-2024", "ESP", "4")

#Filters based on criteria
malert_geopackage_data <- malert_geopackage_data %>%
  filter(malert_geopackage_data$movelab_annotation_euro.class_label == "aedes-albopictus" & malert_geopackage_data$movelab_annotation_euro.class_value >= 1)

#Urban atlas location on linux
urban_atlas <- st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg")

#used to check referencing system
crs_check = st_crs(malert_geopackage_data) 

#Transforms urban atlas to wg84
urban_atlas <- st_transform(urban_atlas, crs = 4326)

#Explanation: The st_transform() function changes the CRS of malert_geopackage_data to match that of urban_atlas. This is essential for accurate spatial operations.
#malert_geopackage_data <- st_transform(malert_geopackage_data, st_crs(urban_atlas))

#Joins urban atlas data with malert_geopackage_data
malert_geopackage_data <- st_join(malert_geopackage_data, urban_atlas, join = st_intersects)

# Filter out records with no match (outside the urban atlas area)
malert_geopackage_data <- malert_geopackage_data[!is.na(malert_geopackage_data$fua_name), ]

#Gives list of cities with count of reports
aggregated_data <- malert_geopackage_data %>%
  group_by(malert_geopackage_data$NAME_4) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





# Plot the urban atlas with mosquito reports overlaid
ggplot() +
  geom_sf(data = urban_atlas, fill = "lightgrey", color = "black", alpha = 0.5) +  # Urban atlas layer
  geom_sf(data = malert_geopackage_data, aes(color = "Aedes Albopictus"), size = 1, alpha = 0.7) +  # Mosquito reports layer
  labs(title = "Mosquito Reports in Urban Areas of Barcelona",
       color = "Mosquito Type") +
  theme_minimal() +
  theme(legend.position = "bottom")


names(urban_atlas)
# Set to interactive mode
tmap_mode("view")

# Create the map
tm_shape(urban_atlas) +
  tm_polygons(col = "class_2018", alpha = 0.3, border.col = "black") +  # Customize with appropriate column
  tm_shape(malert_geopackage_data) +
  tm_dots(col = "red", size = 0.1, alpha = 0.7) +
  tm_layout(title = "Interactive Map: Mosquito Reports in Barcelona")


 

# Transform the data to WGS84 (EPSG:4326) for compatibility with OSM
urban_atlas_sf <- st_transform(urban_atlas, crs = 4326)
malert_geopackage_data_sf <- st_transform(malert_geopackage_data, crs = 4326)

# Create a leaflet map with OpenStreetMap
leaflet() %>%
  addTiles() %>%  # Add the OSM basemap
  addPolygons(data = urban_atlas_sf, color = "blue", weight = 1, opacity = 0.5, fillOpacity = 0.3) %>%
  addCircleMarkers(data = malert_geopackage_data_sf, color = "red", radius = 3, opacity = 0.7) %>%
  addLegend(position = "bottomright", colors = "red", labels = "Mosquito Reports") %>%
  setView(lng = 2.1734, lat = 41.3851, zoom = 12)  # Adjust to Barcelona coordinates