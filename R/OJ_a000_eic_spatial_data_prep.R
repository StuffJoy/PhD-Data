# Title ####
# Making some iniital map objects

rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
#library(rgdal)
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

write_rds(urban_perimeter, "data/proc/a001_eic_bcn_perimeter.Rds")

bcn_map_sp = as(urban_atlas_transformed, "Spatial")

bcn_map_sf <- st_as_sf(bcn_map_sp)

bcn_map_sp_LonLat <- st_transform(bcn_map_sf, crs = LONLAT)

#bcn_map_sp_LonLat <- as(bcn_map_sp_LonLat, "Spatial")

spain_gpdf = readRDS('spain_grid_small.Rds')

spain_gpdf_sf <- st_as_sf(spain_gpdf)

bcn_gpdf <- spain_gpdf_sf[st_intersects(spain_gpdf_sf, bcn_map_sp_LonLat, sparse = FALSE), ]

write_rds(bcn_gpdf, "data/cartography/bcn_gpdf.Rds")


bcn_cells = bcn_gpdf$TigacellID

write_rds(bcn_cells, "data/proc/a000_bcn_cells.Rds")


write_rds(urban_atlas_transformed, "data/proc/a001_eic_bcn_map.Rds")


# Land Cover ####

ua = st_read("~/Downloads/ES002L2_BARCELONA_UA2018_v013/Data/ES002L2_BARCELONA_UA2018_v013.gpkg", layer = "ES002L2_BARCELONA_UA2018")

write_rds(ua, "data/proc/a001_eic_us.Rds")

# Population #### - MISSING

padron = read_csv("data/demography/2020_10_TAULA_MAP_SCENSAL.csv") %>%  mutate(pop = HOMES + DONES) %>% select(SECCIO_CENSAL, pop)

bcn_map_pop = bcn_map %>% filter(TIPUS_UA == "SEC_CENS") %>% mutate(SECCIO_CENSAL = paste0(DISTRICTE, SEC_CENS)) %>% left_join(padron) %>% mutate(pop_density = pop/AREA)

write_rds(bcn_map_pop, "data/proc/a001_eic_bcn_map_pop.Rds")

# Socio-economic variables ####

#Pulls mosquito alert data and performs spatial join
malert_geopackage_data = get_malert_geopackage_data("2014-2024", "ESP", "4")

#Filters based on criteria
malert_geopackage_data <- malert_geopackage_data %>%
  filter(malert_geopackage_data$movelab_annotation_euro.class_label == "aedes-albopictus" & malert_geopackage_data$movelab_annotation_euro.class_value >= 1)

#Transforms urban atlas to wg84
urban_atlas <- st_transform(urban_atlas, crs = 4326)

#Joins urban atlas data with malert_geopackage_data
malert_geopackage_data <- st_join(malert_geopackage_data, urban_atlas, join = st_intersects)

# Filter out records with no match (outside the urban atlas area)
malert_geopackage_data <- malert_geopackage_data[!is.na(malert_geopackage_data$fua_name), ]

#Gives list of cities with count of reports
aggregated_data <- malert_geopackage_data %>%
  group_by(malert_geopackage_data$NAME_4) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

city_names <- malert_geopackage_data %>%
  pull(NAME_4) %>%
  unique()

city_names <- city_names %>%
  str_replace_all("L'Ametlla del Vallès", "Ametlla del Vallès, L'") %>%
  str_replace_all("L'Hospitalet de Llobregat","Hospitalet de Llobregat, L'")

ses_30904 <- read_delim("demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".")

ses_30904 <- ses_30904 %>%
  filter(str_detect(Municipalities, paste(city_names, collapse = "|")))

demog <- ses_30904 %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "sección")) %>%  # Keep 'sección' filter for correct rows
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  filter(period == 2017, indicadores_demograficos != "Population") %>% 
  select(-period, -municipalities) %>%  # Remove 'municipalities' if no longer needed
  pivot_wider(names_from=indicadores_demograficos, values_from=total) %>% 
  clean_names()

ses_37686 <- read_delim("demography/37686.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".")

ses_37686 <- ses_37686 %>%
  filter(str_detect(Municipalities, paste(city_names, collapse = "|")))

gini <- ses_37686 %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "sección")) %>% 
  #separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% 
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  filter(period==2017) %>% select(-period, -municipalities) %>% 
  #pivot_wider(census_section, names_from=indicadores_de_renta_media, values_from=total) %>% 
  pivot_wider(names_from=indicadores_de_renta_media, values_from=total) %>% 
  clean_names()

ses_30896 <- read_delim("demography/30896.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".")

ses_30896 <- ses_30896 %>%
  filter(str_detect(Municipalities, paste(city_names, collapse = "|")))

income <- ses_30896 %>% 
  clean_names() %>% 
  filter(str_detect(municipalities, "sección")) %>% 
  #separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% 
  mutate(census_section = str_extract(municipalities, "^\\d+")) %>%  # Extract 0801902003
  #filter(period==2017) %>% select(-period, -municipalities) %>% 
  filter(period==2017) %>% select(-period) %>% 
  pivot_wider(names_from=indicadores_de_renta_media, values_from=total) %>% 
  clean_names()

write_rds(bcn_demog, "data/proc/a000_eic_bcn_demog.Rds")

write_rds(bcn_gini, "data/proc/a000_eic_bcn_gini.Rds")

write_rds(bcn_income, "data/proc/a000_eic_bcn_income.Rds")
