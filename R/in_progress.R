# Title ####
# Making some iniital map objects

rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
library(rgdal)
library(tmap)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
library(dplyr)

source('scripts/functions.r')
source('scripts/parameters.r')


cell_mask = 0.025

# Barcelona Map ####

# Step 1: Read the GeoPackage
urban_atlas <- st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg")

# Step 2: Transform the coordinate system to EPSG:3035 (European equal-area)
urban_atlas_transformed <- st_transform(urban_atlas, crs = 3035)

# Step 3: Summarise to calculate the total area and merge polygons

# Set the geometry column to "geom"
st_geometry(urban_atlas_transformed) <- "geom"

# Summarise to calculate the total area
urban_atlas_perimeter <- urban_atlas_transformed %>%
  summarise(total_area = sum(st_area(.)))

# Step 4: Plot the resulting perimeter
plot(urban_atlas_perimeter, col = "blue", main = "Urban Atlas Perimeter")


# Load the required library
library(sf)

# Step 1: Read the Urban Atlas GeoPackage
urban_atlas <- st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg")

# Step 2: Transform the coordinate reference system (optional)
# If GeoJSON needs a specific CRS (e.g., WGS 84 - EPSG:4326), transform it:
urban_atlas_transformed <- st_transform(urban_atlas, crs = 4326)

# Step 3: Write the GeoJSON file
st_write(urban_atlas_transformed, "urban_atlas_barcelona.geojson", driver = "GeoJSON")




# Load required libraries
library(sf)
library(jsonlite)

# Step 1: Read the Urban Atlas GeoPackage
urban_atlas <- st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg")

# Step 2: Transform the coordinate reference system (optional, e.g., WGS 84)
urban_atlas_transformed <- st_transform(urban_atlas, crs = 4326)

# Step 3: Convert the sf object to a data frame
urban_atlas_df <- as.data.frame(urban_atlas_transformed)

# Step 4: Add the geometry as WKT (Well-Known Text) for simpler JSON
urban_atlas_df$geometry <- st_as_text(st_geometry(urban_atlas_transformed))

# Step 5: Export to JSON
write_json(urban_atlas_df, "urban_atlas_barcelona.json", pretty = TRUE)



