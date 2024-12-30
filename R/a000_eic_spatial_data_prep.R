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

source('scripts/functions.r')
source('scripts/parameters.r')


cell_mask = 0.025

# Barcelona Map ####

bcn_map = st_read("0301100100_UNITATS_ADM_POLIGONS.json") %>% st_transform(3035)
spain_map = st_read("gadm41_ESP_4.json") %>% st_transform(3035)

colnames(spain_map)

bcn_perimeter = bcn_map %>% summarise(area = sum(AREA))

spain_perimeter <- spain_map %>%
  summarise(area = sum(st_area(geometry)))



bcn_area_utm31N = st_read("data/cartography/BCN_UNITATS_ADM/0301100100_UNITATS_ADM_POLIGONS.json") %>% summarise(area = sum(AREA)) %>% st_drop_geometry() %>% pull(area)


plot(bcn_perimeter)
plot(spain_perimeter)

write_rds(bcn_perimeter, "data/proc/a001_eic_bcn_perimeter.Rds")

bcn_map_sp = as(bcn_map, "Spatial")

bcn_map_sp_LonLat = spTransform(bcn_map_sp, LONLAT)

spain_gpdf = readRDS('data/cartography/spain_grid_small.Rds')

bcn_gpdf = spain_gpdf[bcn_map_sp_LonLat,]

write_rds(bcn_gpdf, "data/cartography/bcn_gpdf.Rds")


bcn_cells = bcn_gpdf$TigacellID

write_rds(bcn_cells, "data/proc/a000_bcn_cells.Rds")


write_rds(bcn_map, "data/proc/a001_eic_bcn_map.Rds")



# Land Cover ####

ua = st_read("data/cartography/urban_atlas_bcn/ES002L2_BARCELONA_UA2018_v012/Data/ES002L2_BARCELONA_UA2018_v012.gpkg", layer = "ES002L2_BARCELONA_UA2018")

write_rds(ua, "data/proc/a001_eic_us.Rds")

# Population ####

padron = read_csv("demography/2020_10_TAULA_MAP_SCENSAL.csv") %>%  mutate(pop = HOMES + DONES) %>% select(SECCIO_CENSAL, pop)


bcn_map_pop = bcn_map %>% filter(TIPUS_UA == "SEC_CENS") %>% mutate(SECCIO_CENSAL = paste0(DISTRICTE, SEC_CENS)) %>% left_join(padron) %>% mutate(pop_density = pop/AREA)

write_rds(bcn_map_pop, "data/proc/a001_eic_bcn_map_pop.Rds")


# Socio-economic variables ####

bcn_demog <- read_delim("data/demography/30904.csv", ";", escape_double = FALSE, trim_ws = TRUE, na=".") %>% clean_names() %>% filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% filter(period==2017, indicadores_demograficos != "Population") %>% select(-period) %>% pivot_wider(census_section, names_from=indicadores_demograficos, values_from=total) %>% clean_names()

bcn_gini <- read_delim("demography/37686.csv",  ";", escape_double = FALSE, trim_ws = TRUE, na = ".") %>% clean_names() %>% filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% filter(period==2017) %>% select(-period) %>% pivot_wider(census_section, names_from=indicadores_de_renta_media, values_from=total) %>% clean_names()

bcn_income <- read_delim("demography/30896.csv",  ";", escape_double = FALSE, trim_ws = TRUE, na = ".") %>% clean_names() %>% filter(str_detect(municipalities, "Barcelona"), str_detect(municipalities, "sección")) %>% separate(municipalities, into=c(NA, NA, NA, "census_section"), sep=" ") %>% filter(period==2017) %>% select(-period) %>% pivot_wider(census_section, names_from=indicadores_de_renta_media, values_from=total) %>% clean_names()

write_rds(bcn_demog, "data/proc/a000_eic_bcn_demog.Rds")

write_rds(bcn_gini, "data/proc/a000_eic_bcn_gini.Rds")

write_rds(bcn_income, "data/proc/a000_eic_bcn_income.Rds")