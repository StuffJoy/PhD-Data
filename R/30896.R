devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/aggregate-function")
install.packages("tmap")

library(mosquitoR)
library(dplyr)
library(sf)
library(tidyverse)
library(tmap)
library(ggplot2)
library(stringr)
library(janitor)

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

city_names <- malert_geopackage_data %>%
  pull(NAME_4) %>%
  unique()

city_names <- city_names %>%
  str_replace_all("L'Ametlla del Vallès", "Ametlla del Vallès, L'") %>%
  str_replace_all("L'Hospitalet de Llobregat","Hospitalet de Llobregat, L'")


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
