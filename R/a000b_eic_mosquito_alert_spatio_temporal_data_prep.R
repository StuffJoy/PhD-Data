####Title####
# Main script for EIC Barcelona modeling
## Written using R 4.0.2

# R CMD BATCH --no-save --no-restore scripts/eic/a001_eic_mosquito_alert_spatio_temporal_model.R scripts/eic/a001_eic_mosquito_alert_spatio_temporal_model.out 


rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
# library(rgdal)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
library(RSocrata)
library(data.table)
library(jsonlite)
library(mosquitoR)

source('scripts/parameters.r')

ncores = 16

cell_mask = 0.025

# Barcelona Map ####

bcn_perimeter = read_rds("data/proc/a001_eic_bcn_perimeter.Rds")  %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)
#comes from a000_eic_spatial_data_prep.R

bcn_gpdf = read_rds("data/cartography/bcn_gpdf.Rds") %>% st_as_sf() %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)
#comes from a000_eic_spatial_data_prep.R

bcn_cells = read_rds("data/proc/a000_bcn_cells.Rds")
#comes from a000_eic_spatial_data_prep.R

bcn_map = read_rds("data/proc/a001_eic_bcn_map.Rds") %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)
#comes from a000_eic_spatial_data_prep.R

# Land Cover ####

ua = read_rds("data/proc/a001_eic_us.Rds") %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)
#comes from a000_eic_spatial_data_prep.R

ua3d = ua %>% mutate(code_2018 = str_sub(code_2018, 1, 3)) %>% filter(code_2018 %in% c("111", "121"))

# Weather #### - #comes from a000_eic_historical_weather_data_prep

bcn_weather_daily =read_rds("data/proc/a001_eic_bcn_weather_daily.Rds")

bcn_weather_lags_7d = read_rds("data/proc/a001_eic_bcn_weather_lags_7d.Rds")

bcn_weather_lags_14d = read_rds("data/proc/a001_eic_bcn_weather_lags_14d.Rds")

bcn_weather_lags_30d = read_rds("data/proc/a001_eic_bcn_weather_lags_30d.Rds")

bcn_weather_lags_30d_lag7 = read_rds("data/proc/a001_eic_bcn_weather_lags_21d_lag7.Rds")

bcn_weather_ppt_lags = read_rds("data/proc/a001_eic_bcn_weather_ppt_lags.Rds")


# Population ####

bcn_map_pop = read_rds("data/proc/a001_eic_bcn_map_pop.Rds") %>% st_set_crs(3035) # resetting CRS to deal with issue of data created on different machines (ignore warning)
#comes from a000_eic_spatial_data_prep.R

sampling_effort = read_csv("https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz") 

range(sampling_effort$SE)

vrs = readRDS('~/research/mosquito_model_data_prep/data/proc/mosquito_alert_cleaned_reports.Rds') %>% filter(report_type == "adult") 

D = vrs %>% mutate(year = year(date), sea_days = yday(date), TigacellID_small = make_samplingcell_ids(lon, lat, .025)) %>% filter(date > as_date(android_start_date)) %>% left_join(vrs %>% group_by(user) %>% summarise(mean_score = mean(movelab_certainty_category, na.rm=TRUE))) %>% mutate(reliable_report = (movelab_certainty_category>0 | ( (is.na(movelab_certainty_category) | movelab_certainty_category >=0) & mean_score>0))) %>% filter(reliable_report) %>% filter(TigacellID_small %in% bcn_cells) %>% left_join(bcn_weather_daily %>%dplyr::select(date, mwi, maxTM, meanPPT24H, mwi_zeros_past_14d, FH_zeros_past_14d)) %>% left_join(bcn_weather_ppt_lags %>% dplyr::select(date, PPT_7d_8daysago)) %>% left_join(bcn_weather_lags_30d %>% dplyr::select(date, FW30, FH30, FT30, mwi30)) %>% left_join(bcn_weather_lags_14d %>% dplyr::select(date, FW14, FH14, FT14, mwi14)) %>% left_join(bcn_weather_lags_30d_lag7 %>% dplyr::select(date, FW21, FH21, FT21, mwi21))

MA_season_bounds = D %>% filter(movelab_certainty_category==2) %>% mutate(year = factor(year)) %>% summarise(season_start = min(sea_days), season_end = max(sea_days))

write_rds(MA_season_bounds, "data/proc/a000b_eic_MA_season_bounds.Rds")

MA_season_bounds_yearly = D %>% filter(movelab_certainty_category==2) %>% mutate(year = factor(year)) %>% group_by(year) %>% summarise(season_start = min(sea_days), season_end = max(sea_days))

write_rds(MA_season_bounds_yearly, "data/proc/a000b_eic_MA_season_bounds_yearly.Rds")


# Mosquito Alert Spatio-Temporal ####
trs_daily = sampling_effort %>%
  filter(TigacellID %in% bcn_cells, date >= as_date("2018-01-01")) 

min_SE = trs_daily %>% filter(SE > 0) %>% pull(SE) %>% min()
min_SE_logit = log(min_SE/(1-min_SE))

write_rds(min_SE_logit, "data/proc/a001_eic_min_SE_logit.Rds")

trs_start = min(trs_daily$date)
trs_end = max(trs_daily$date)

write_rds(trs_daily, "data/proc/a001_eic_trs_daily.Rds")

D_SE = D %>% mutate(TigacellID = TigacellID_small) %>% filter(year>=2018) %>% left_join(trs_daily) 

D_sf = st_as_sf(D_SE, coords = c("lon", "lat"), crs=4326) %>% st_transform(3035) %>% st_join(ua %>% dplyr::select(code_2018), join = st_intersects, left=TRUE) %>% st_join(bcn_map_pop %>% dplyr::select(pop_density, DISTRICTE, BARRI, SEC_CENS, GRANBARRI), join = st_intersects, left=FALSE) %>% mutate(presence = TRUE) 

bcn_perimeter = bcn_map %>% summarise(area = sum(AREA))

sampling_factor = 10

# making trs yearly for pseudoabsence calcs

trs_yearly = trs_daily %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(SE_expected_year=sum(SE_expected)) 

# Pseudoabsences ####
# sampling pseudoabsences proportional to ASDM sampling effort
# adding grid approach used in citsci-spatial-biases article
asdm_prediction_points = read_rds("data/proc/asdm_prediction_points_from_citsci_spatial_bias.Rds") %>% filter(year == 2020) %>% dplyr::select(year, preds) # taking just one year because there is no variation by year in the ASDM model, so they are all the same, and we will repliace the object next

# left_join(trs_yearly) %>% mutate(absence_weights = preds*SE_expected_year)

n_absences = sampling_factor*nrow(D_sf)

n_absences_per_year = sample_n(trs_yearly, n_absences, replace = TRUE, weight = trs_yearly$SE_expected_year) %>% group_by(year) %>% summarise(n = n())

D_samp_sf = bind_rows(lapply(n_absences_per_year$year, function(this_year){
  this_n_absences = n_absences_per_year %>% filter(year == this_year) %>% pull(n)
  these_trs = trs_daily %>% mutate(year = year(date)) %>% filter(year == this_year)
  this_sample = sample_n(asdm_prediction_points, size = this_n_absences, weight = asdm_prediction_points$preds, replace = FALSE) %>% mutate(year = this_year)
  these_dates = sample_n(these_trs, size = this_n_absences, weight = these_trs$SE_expected, replace = TRUE)
  this_sample$date = these_dates$date
  return(this_sample)
})) %>% st_transform(st_crs(ua)) %>% st_join(ua %>% dplyr::select(code_2018), join = st_intersects, left=TRUE) %>% st_join(bcn_map_pop %>% dplyr::select(pop_density, DISTRICTE, BARRI, SEC_CENS, GRANBARRI), join = st_intersects, left=FALSE) %>% mutate(presence = FALSE) %>% left_join(bcn_weather_daily %>% dplyr::select(date, mwi, maxTM, meanPPT24H, mwi_zeros_past_14d, FH_zeros_past_14d)) %>% left_join(bcn_weather_ppt_lags %>% dplyr::select(date, PPT_7d_8daysago)) %>% left_join(bcn_weather_lags_30d %>% dplyr::select(date, FW30, FH30, FT30, mwi30)) %>% left_join(bcn_weather_lags_14d %>% dplyr::select(date, FW14, FH14, FT14, mwi14)) %>% left_join(bcn_weather_lags_30d_lag7 %>% dplyr::select(date, FW21, FH21, FT21, mwi21)) %>% rename(asdm_se = preds)

D_samp_sf_lonlat = D_samp_sf %>% st_transform(4326) 

coords = round_down(st_coordinates(D_samp_sf_lonlat), cell_mask)

D_samp_sf = D_samp_sf %>% mutate(TigacellID = paste0(coords[,1], "_", coords[,2])) %>% left_join(trs_daily) %>% replace_na(list(SE=0, SE_logit=min_SE_logit, SE_window_7day=0, SE_window_30day=0, SE_window_365day=0, SE_logit_sum_7day=min_SE_logit, SE_logit_sum_30day=min_SE_logit, SE_logit_sum_365day=min_SE_logit))

# temporarily adding NAs for asdm_se. Will then predict it at end
D_sf$asdm_se = NA

D_mod = rbind(D_samp_sf, D_sf %>% dplyr::select(names(D_samp_sf))) %>% mutate(SE_density = pop_density*SE, year = factor(year(date)), sea_days = yday(date)) %>% mutate(id = row_number())

# private green ####
# adding private green 
private_green = st_read("~/research/citsci-spatial-bias/data/private/NDVI_privat.shp")

D_mod = D_mod %>% st_join(private_green %>% dplyr::select(id_ndvi) %>% st_transform(st_crs(D_mod))) %>% mutate(private_green = !is.na(id_ndvi)) %>% dplyr::select(-id_ndvi)

nearest_private_green_indexes = D_mod %>% st_nearest_feature(private_green %>% st_transform(st_crs(D_mod)))

dist_nearest_private_green = D_mod %>% st_distance(private_green[nearest_private_green_indexes,]%>% st_transform(st_crs(D_mod)), by_element = TRUE)

ddf = function(d, a=.01, b=1) exp(-a*(d^b))

D_mod = D_mod %>% mutate(dist_nearest_private_green = as.numeric(dist_nearest_private_green), dist_nearest_private_green_neg_exp = -exp(dist_nearest_private_green), ddf_proximity = ddf(dist_nearest_private_green))

# Drains ####

# drain data ####

drain_data = read_xlsx("~/research/citsci-spatial-bias/data/private/dades_items_revisats_1-1-2019_31-12-2023.xlsx", sheet = "2_items_revisats_per_any", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))

drains_table_yearly = drain_data %>% dplyr::select(year =yyear, id_item = codi_svipla, geom = geometria, tipus_entitat, tipologia, n_aedes_activity_detections = num_rev_act_aedes, n_water_detections = num_rev_aigua) %>% mutate(aedes_activity = n_aedes_activity_detections>0, water = n_water_detections>0) %>% filter(!is.na(geom))

write_rds(drains_table_yearly, "data/proc/drains_table_yearly.Rds")

drain_locations_wkb_character = drains_table_yearly %>% dplyr::select(id_item, geom) %>% distinct()

drain_locations = bind_rows(lapply(1:nrow(drain_locations_wkb_character), function(i) st_sf(id_item = drain_locations_wkb_character$id_item[i], geometry = st_as_sfc(structure(list(drain_locations_wkb_character$geom[i]), class = "WKB"), EWKB = TRUE))))

write_rds(drain_locations, "data/proc/drain_locations.Rds")

drains_yearly_all = drain_locations %>% right_join(drains_table_yearly, multiple = "all") 

write_rds(drains_yearly_all, "data/proc/drains_yearly_all.Rds")

drains_yearly_water = drain_locations %>% right_join(drains_table_yearly, multiple = "all") %>% filter(aedes_activity) 

drains_yearly_aedes = drain_locations %>% right_join(drains_table_yearly, multiple = "all") %>% filter(water) 

this_year = unique(D_mod$year)[1]

D_mod_water_drain_dists = bind_rows(lapply(unique(D_mod$year), function(this_year){
  this_D_mod = D_mod %>% filter(year == this_year)
  these_drains = drains_yearly_water %>% filter(year == this_year)
  these_nearest_drain_indexes = this_D_mod %>% st_nearest_feature(these_drains %>% st_transform(st_crs(this_D_mod)))
  these_dists = this_D_mod %>% st_distance(these_drains[these_nearest_drain_indexes,]%>% st_transform(st_crs(this_D_mod)), by_element = TRUE) %>% as.numeric()
  this_D_mod$dist_nearest_water_drain = these_dists
  return(this_D_mod)
}))

D_mod_aedes_drain_dists = bind_rows(lapply(unique(D_mod_water_drain_dists$year), function(this_year){
  this_D_mod = D_mod_water_drain_dists %>% filter(year == this_year)
  these_drains = drains_yearly_aedes %>% filter(year == this_year)
  these_nearest_drain_indexes = this_D_mod %>% st_nearest_feature(these_drains %>% st_transform(st_crs(this_D_mod)))
  these_dists = this_D_mod %>% st_distance(these_drains[these_nearest_drain_indexes,]%>% st_transform(st_crs(this_D_mod)), by_element = TRUE) %>% as.numeric()
  this_D_mod$dist_nearest_aedes_drain = these_dists
  return(this_D_mod)
}))


drains_yearly_water_buff200 = drains_yearly_water %>% dplyr::select(year, aedes_activity_drain_200m = aedes_activity) %>% st_transform(st_crs(D_mod)) %>% st_buffer(dist = 200) 

drains_yearly_water_buff500 = drains_yearly_water %>% dplyr::select(year, aedes_activity_drain_500m = aedes_activity) %>% st_transform(st_crs(D_mod)) %>% st_buffer(dist = 500) 


D_mod_drain_buffers = bind_rows(lapply(unique(D_mod_aedes_drain_dists$year), function(this_year){
  this_D_mod = D_mod_aedes_drain_dists %>% filter(year == this_year) 
  D_mod_activity_ids = this_D_mod %>% st_filter(drains_yearly_water_buff200 %>% filter(year==this_year & aedes_activity_drain_200m)) %>% pull(id) %>% unique()
  D_mod_no_activity_ids = this_D_mod %>% st_filter(drains_yearly_water_buff200 %>% filter(year==this_year & !aedes_activity_drain_200m)) %>% pull(id) %>% unique()
  D_mod_water200_ids = this_D_mod %>% st_filter(drains_yearly_water_buff200 %>% filter(year==this_year)) %>% pull(id) %>% unique()
  D_mod_water500_ids = this_D_mod %>% st_filter(drains_yearly_water_buff500 %>% filter(year==this_year)) %>% pull(id) %>% unique()
  this_D_mod %>% mutate(aedes_drain_zone = case_when( id %in% D_mod_activity_ids~"yes", (!id %in% D_mod_activity_ids & id %in% D_mod_no_activity_ids)~"no", .default = "unknown"), water_drain_zone200 = id %in% D_mod_water200_ids, water_drain_zone500 = id %in% D_mod_water500_ids)
}))


D_mod = D_mod_drain_buffers
# START Buffers ####

# D_mod_buffers_100 = D_mod %>% dplyr::select(id) %>% st_buffer(dist = 100, nQuadSegs = 10) 
# D_mod_buffers_area_100 = as.integer(st_area(D_mod_buffers_100)[1]) # calculating exact area of one buffer; not just using pi*r^2 since they are not perfect circles but rather polygons


# ua_D_mod_buff_int_100 = bind_rows(mclapply(1:nrow(D_mod_buffers_100), function(i){
#  this_buffer = D_mod_buffers_100[i,]
#  st_intersection(ua3d, this_buffer)
# }, mc.cores = ncores) )


# ua_D_mod_buff_int_areas_100 = st_area(ua_D_mod_buff_int_100)


# D_mod_lc_100 = ua_D_mod_buff_int_100 %>% st_drop_geometry() %>% mutate(area = as.numeric(ua_D_mod_buff_int_areas_100)) %>% group_by(id, code_2018) %>% summarise(proportion = sum(area)/D_mod_buffers_area_100) %>%  pivot_wider(names_from=code_2018, names_prefix = "buff100_code_2018_", values_from = proportion, values_fill = 0) 


# D_mod = D_mod %>% left_join(D_mod_lc_100, by="id") 

# END Buffers ####

# grid ####

this_grid = st_make_grid(bcn_perimeter, cellsize = c(400, 400), what = "polygons", square = FALSE) %>% st_sf %>% rowid_to_column("grid_ID")

write_rds(this_grid, "data/proc/car_grid.Rds")

D_mod = D_mod %>% st_join(this_grid)


D_mod_df = as_tibble(D_mod) %>% dplyr::select(-geometry) %>% as.data.frame

D_mod_df$census_section = paste0(D_mod_df$DISTRICTE, D_mod_df$SEC_CENS)

# Socio-economic variables ####

bcn_demog = read_rds("data/proc/a000_eic_bcn_demog.Rds")

bcn_gini = read_rds("data/proc/a000_eic_bcn_gini.Rds")

bcn_income = read_rds("data/proc/a000_eic_bcn_income.Rds")



D_mod_df = D_mod_df %>% left_join(bcn_income) %>% left_join(bcn_demog) %>% left_join(bcn_gini)

# important: here I am excluding neighborhoods with no sampling effort and no reports so that when we do predictions these do not get treated as necessarily low probability solely due to the pseudoabsences

barri_dates_0SE = D_mod_df %>% group_by(BARRI, date) %>% summarise(SE = sum(SE, na.rm=TRUE), reports = sum(presence, na.rm=TRUE)) %>% ungroup() %>% filter(SE==0 & reports == 0) %>% mutate(barri_date = paste0(BARRI, date)) %>% dplyr::select(barri_date) %>% pull()

D_mod_df = D_mod_df %>% mutate(barri_date = paste0(BARRI, date)) %>% filter(!barri_date %in% barri_dates_0SE) %>% rename(p_singlehh = porcentaje_de_hogares_unipersonales, mean_age = average_age_of_the_population, mean_rent_consumption_unit = mediana_de_la_renta_por_unidad_de_consumo, popd = pop_density) %>% mutate(id_item = "X")

# adding asdm preds to presence points
asdm_main = read_rds("data/proc/asdm_main_model_from_citsci_spatial_bias.Rds")

asdm_pred = apply(posterior_epred(asdm_main, newdata = D_mod_df, allow_new_levels = TRUE), 2, function(x) mean(x))

D_mod_df$asdm_sampling_effort = asdm_pred

D_mod_df %>% mutate(test = asdm_se - asdm_sampling_effort) %>% pull(test) %>% hist()

D_mod_df %>% pull(asdm_se) %>% hist()

###

write_rds(D_mod_df, "data/proc/a000_mosquito_alert_spatio_temporal_data_D_mod_df.Rds")

write_rds(D_mod, "data/proc/a000_mosquito_alert_spatio_temporal_data_D_mod.Rds")

# checking to make sure no 0 SE where there is presence (since presence itself means a report which shows SE -- see above)
D_mod_df %>% filter(SE==0 & presence == 1)

table(D_mod_df$presence)