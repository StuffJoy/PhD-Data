# Title ####
# Making some iniital map objects

rm(list=ls())

install.packages("rgdal")
install.packages("rgdal", type="source")

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
source('parameters.R')


cell_mask = 0.025

# Barcelona Map ####

# Step 1: Read the GeoPackage
urban_atlas <- st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg")

# Step 2: Transform the coordinate system to EPSG:3035 (European equal-area)
urban_atlas_transformed <- st_transform(urban_atlas, crs = 3035)

# Step 3: Summarise to calculate the total area and merge polygons
urban_perimeter <- urban_atlas_transformed %>%
  summarise(area = sum(st_area(geom)))


# Step 4: Plot the resulting perimeter
plot(urban_perimeter, col = "blue", main = "Urban Atlas Perimeter")





write_rds(bcn_perimeter, "data/proc/a001_eic_bcn_perimeter.Rds")

bcn_map_sp = as(urban_atlas_transformed, "Spatial")

bcn_map_sf <- st_as_sf(bcn_map_sp)

bcn_map_sp_LonLat <- st_transform(bcn_map_sf, crs = LONLAT)

bcn_map_sp_LonLat <- as(bcn_map_sp_LonLat, "Spatial")

spain_gpdf = readRDS('spain_grid_small.Rds')
spain_gpdf_sf <- st_as_sf(spain_gpdf)

bcn_gpdf = spain_gpdf[bcn_map_sp_LonLat,]
bcn_gpdf <- spain_gpdf_sf[st_intersects(spain_gpdf_sf, bcn_map_sp_LonLat, sparse = FALSE), ]

st_crs(bcn_map_sp_LonLat)
st_crs(spain_gpdf)


write_rds(bcn_gpdf, "data/cartography/bcn_gpdf.Rds")


bcn_cells = bcn_gpdf$TigacellID

write_rds(bcn_cells, "data/proc/a000_bcn_cells.Rds")


write_rds(bcn_map, "data/proc/a001_eic_bcn_map.Rds")






# Socio-economic variables ####



bcn_demog2 <- read_delim("demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".") %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% 
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  filter(period == 2017, indicadores_demograficos != "Population") %>% 
  select(-period, -municipalities) %>%  # Remove 'municipalities' if no longer needed
  pivot_wider(names_from=indicadores_demograficos, values_from=total) %>% 
  clean_names()

bcn_demog4 <- read_delim("demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".") %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "sección")) %>%  # Keep 'sección' filter for correct rows
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  filter(period == 2017, indicadores_demograficos != "Population") %>% 
  select(-period, -municipalities) %>%  # Remove 'municipalities' if no longer needed
  pivot_wider(names_from=indicadores_demograficos, values_from=total) %>% 
  clean_names()

bcn_demog4 <- bcn_raw %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "sección")) %>%  # Keep 'sección' filter for correct rows
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  filter(period == 2017, indicadores_demograficos != "Population") %>% 
  select(-period, -municipalities) %>%  # Remove 'municipalities' if no longer needed
  pivot_wider(names_from=indicadores_demograficos, values_from=total) %>% 
  clean_names()

bcn_raw <- read_delim("demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".")

bcn_demog4_filtered <- bcn_demog4 %>%
  filter(str_detect(Municipalities, paste(city_names, collapse = "|")))

bcn_demog4_filtered3 <- bcn_raw %>%
  filter(str_detect(Municipalities, paste(city_names_normalized, collapse = "|")))

bcn_demog4_excluded <- bcn_demog4_filtered %>%
  filter(!str_detect(Municipalities, paste(city_names, collapse = "|")))

bcn_demog4_excluded2 <- bcn_demog4_filtered2 %>%
  filter(!str_detect(Municipalities, paste(city_names, collapse = "|")))

bcn_demog4_filtered2 <- bcn_raw %>%
  mutate(Municipalities_normalized = str_replace_all(Municipalities, "'", ""),  # Remove apostrophes
         Municipalities_normalized = str_trim(Municipalities_normalized),  # Remove leading/trailing spaces
         Municipalities_normalized = tolower(Municipalities_normalized)) %>%  # Convert to lowercase
  filter(str_detect(Municipalities_normalized, 
                    paste(tolower(str_replace_all(city_names, "'", "")), collapse = "|")))  # Normalize city names

city_names_normalized <- city_names %>%
  str_replace_all("L'Ametlla del Vallès", "Ametlla del Vallès, L'") %>%
  str_replace_all("L'Hospitalet de Llobregat","Hospitalet de Llobregat, L'")

# You can now inspect bcn_demog4_excluded to see the excluded municipalities


bcn_demog3 <- read_delim("demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".") %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% 
  mutate(admin_code = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  separate(municipalities, into = c(NA, NA, NA, "census_section"), sep = " ") %>%  # Extract 02003
  filter(period == 2017, indicadores_demograficos != "Population") %>% 
  select(-period) %>% 
  pivot_wider(id_cols = c(census_section, admin_code),  # Include both columns in the ID
              names_from = indicadores_demograficos, 
              values_from = total) %>% 
  clean_names()

print(city_names_normalized)


bcn_gini <- read_delim("data/demography/37686.csv",  ";", escape_double = FALSE, trim_ws = TRUE, na = ".") %>% clean_names() %>% filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% filter(period==2017) %>% select(-period) %>% pivot_wider(census_section, names_from=indicadores_de_renta_media, values_from=total) %>% clean_names()

bcn_income <- read_delim("data/demography/30896.csv",  ";", escape_double = FALSE, trim_ws = TRUE, na = ".") %>% clean_names() %>% filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% filter(period==2017) %>% select(-period) %>% pivot_wider(census_section, names_from=indicadores_de_renta_media, values_from=total) %>% clean_names()

write_rds(bcn_demog, "data/proc/a000_eic_bcn_demog.Rds")

write_rds(bcn_gini, "data/proc/a000_eic_bcn_gini.Rds")

write_rds(bcn_income, "data/proc/a000_eic_bcn_income.Rds")