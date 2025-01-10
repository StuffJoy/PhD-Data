# Title ####
# For downloading and preparing historical weather data. This should only need to be run once, after which the current and future weather data script can be used

####Dependencies####
library(tidyverse)
library(lubridate)
library(janitor)
library(RcppRoll)
library(parallel)
library(RSocrata)

source("parameters.R")

message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
}

n_cores = 16

# 2020 ####
bcn_weather_2020 = read_csv("https://opendata-ajuntament.barcelona.cat/resources/bcn/EU_Estacions_Meteorologiques/2020_MeteoCat_Detall_Estacions.csv") %>% clean_names() %>% rename(date = data_lectura, weather_type = acronim) %>% filter(!weather_type %in% c("PX", "PN", "DVVX10", "DVM10")) %>% mutate(weather_type = case_when(!weather_type %in% c("PPT", "RS24h", "PM")~weather_type, weather_type=="PPT"~"PPT24H", weather_type=="RS24h"~"RS24H", weather_type=="PM"~"HPA"))

# 2021 ####

bcn_weather_2021 = read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/cf1de5ca-9d1c-424c-9543-8ab23e7f478e/resource/0b96a191-463e-43ce-bb3a-67e61a96f466/download/2021_MeteoCat_Detall_Estacions.csv") %>% clean_names() %>% rename(date = data_lectura, weather_type = acronim) %>% filter(!weather_type %in% c("PX", "PN", "DVVX10", "DVM10")) %>% mutate(weather_type = case_when(!weather_type %in% c("PPT", "RS24h", "PM")~weather_type, weather_type=="PPT"~"PPT24H", weather_type=="RS24h"~"RS24H", weather_type=="PM"~"HPA"))

# 2022 ####
bcn_weather_2022 = read_csv("https://opendata-ajuntament.barcelona.cat/resources/bcn/EU_Estacions_Meteorologiques/2022_MeteoCat_Detall_Estacions.csv") %>% clean_names() %>% rename(date = data_lectura, weather_type = acronim) %>% filter(!weather_type %in% c("PX", "PN", "DVVX10", "DVM10")) %>% mutate(weather_type = case_when(!weather_type %in% c("PPT", "RS24h", "PM")~weather_type, weather_type=="PPT"~"PPT24H", weather_type=="RS24h"~"RS24H", weather_type=="PM"~"HPA"))

# 2023 ####

bcn_weather_2023 = read_csv("https://opendata-ajuntament.barcelona.cat/resources/bcn/EU_Estacions_Meteorologiques/2023_MeteoCat_Detall_Estacions.csv") %>% clean_names() %>% rename(date = data_lectura, weather_type = acronim) %>% filter(!weather_type %in% c("PX", "PN", "DVVX10", "DVM10")) %>% mutate(weather_type = case_when(!weather_type %in% c("PPT", "RS24h", "PM")~weather_type, weather_type=="PPT"~"PPT24H", weather_type=="RS24h"~"RS24H", weather_type=="PM"~"HPA"))



# most recent ####
source("auth/transparencia_cat.R")
# Barcelona weather station codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-estacions-meteorol-giques-autom-tiques/yqwd-vj5e/data : 
bcn_station_codes = "'AN', 'X4', 'D5', 'X8', 'X2'"
# Variable codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-variables-meteorol-giques/4fb2-n3yi/data
weather_variables = read_csv("https://analisi.transparenciacatalunya.cat/api/views/4fb2-n3yi/rows.csv?accessType=DOWNLOAD&sorting=true") %>% clean_names()

weather_acronims = c("VV10", "VVx10", "HR", "HRx", "HRn", "Tx", "Tn", "T", "PPT", "P") # not including RS here because the values for last 4 days are wrong (negative)
wv_codes = weather_variables %>% filter(acronim %in% weather_acronims) %>% pull(codi_variable) 

wv_code_string = paste0("'", paste(wv_codes, collapse = "', '"), "'")


last_day_in_set = max(bcn_weather_2023$date)

yesterday = today() - 1


these_dates = seq.Date(from = (last_day_in_set+1), to=yesterday, by = "day")

this_date = these_dates[1]

meteo_cat_weather_last4d <- bind_rows(mclapply(these_dates, function(this_date) {
  message_parallel(this_date)
  result = read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura between '", this_date, "T00:00:00' and '", this_date,"T23:59:59' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in(", wv_code_string, ")"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() 
  return(result)
}, mc.cores = n_cores))

as_date(meteo_cat_weather_last4d$data_lectura[1], tz="CET")

meteo_cat_weather_last4d_a = meteo_cat_weather_last4d%>% filter(!is.na(data_lectura)) %>% mutate(date = as_date(data_lectura, tz="CET"), codi_variable = as.numeric(codi_variable), valor_lectura = as.numeric(valor_lectura)) %>% dplyr::select(date, data_lectura, codi_variable, valor_lectura, codi_estacio)  %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% weather_acronims) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type =="Tx"~"TX", weather_type =="Tn"~"TN", weather_type =="T"~"TM", weather_type=="VV10"~"VVM10", weather_type=="VVx10"~"VVX10", weather_type=="HR"~"HRM", weather_type=="HRx"~"HRX", weather_type=="HRn"~"HRN", weather_type=="P"~"HPA")) %>% rename(valor = valor_lectura) %>% select(data_lectura, date, codi_estacio, weather_type, valor) %>% pivot_wider(id_cols = c(data_lectura, date, codi_estacio), names_from = weather_type, values_from = valor) %>% group_by(date, codi_estacio) %>% summarise(HRX = max(HRX, na.rm=TRUE), VVM10 = mean(VVM10, na.rm=TRUE), TM = mean(TM, na.rm=TRUE), HRM = mean(HRM, na.rm=TRUE), HPA = mean(HPA, na.rm=TRUE), PPT24H = mean(PPT24H, na.rm=TRUE), TX = max(TX, na.rm=TRUE), TN = min(TN, na.rm=TRUE), HRN = min(HRN, na.rm=TRUE), VVX10 = mean(VVX10, na.rm=TRUE)) %>% ungroup() %>% pivot_longer(cols = -c(date, codi_estacio), names_to = "weather_type", values_to = "valor")

meteo_cat_weather_last4d_a = meteo_cat_weather_last4d %>% 
  filter(!is.na(data_lectura)) %>%
  mutate(
    date = as_date(data_lectura, tz="CET"),
    codi_variable = as.numeric(codi_variable),
    valor_lectura = as.numeric(valor_lectura)
  ) %>%
  dplyr::select(date, data_lectura, codi_variable, valor_lectura, codi_estacio) %>%
  left_join(weather_variables) %>%
  rename(weather_type = acronim) %>%
  filter(weather_type %in% weather_acronims) %>%
  mutate(weather_type = case_when(
    weather_type=="PPT"~"PPT24H",
    weather_type =="Tx"~"TX",
    weather_type =="Tn"~"TN",
    weather_type =="T"~"TM",
    weather_type=="VV10"~"VVM10",
    weather_type=="VVx10"~"VVX10",
    weather_type=="HR"~"HRM",
    weather_type=="HRx"~"HRX",
    weather_type=="HRn"~"HRN",
    weather_type=="P"~"HPA"
  )) %>%
  rename(valor = valor_lectura) %>%
  select(date, codi_estacio, weather_type, valor) %>%
  # Using reframe instead of summarise
  group_by(date, codi_estacio, weather_type) %>%
  reframe(
    valor = case_when(
      weather_type == "HRX" ~ max(valor, na.rm = TRUE),
      weather_type == "HRN" ~ min(valor, na.rm = TRUE),
      weather_type == "TX" ~ max(valor, na.rm = TRUE),
      weather_type == "TN" ~ min(valor, na.rm = TRUE),
      TRUE ~ mean(valor, na.rm = TRUE)
    )
  ) %>%
  pivot_wider(
    id_cols = c(date, codi_estacio),
    names_from = weather_type,
    values_from = valor,
    values_fn = list(valor = mean)
  ) %>%
  pivot_longer(
    cols = -c(date, codi_estacio),
    names_to = "weather_type",
    values_to = "valor"
  )




#%>% filter(!is.na(date), !is.na(weather_type), !is.na(codi_estacio))

# 1996-2019 ####
bcn_weather_2019_fab = read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/cf1de5ca-9d1c-424c-9543-8ab23e7f478e/resource/52e6204c-bf67-47d8-af27-e89cf5f18fda/download/2019_d5_observatori_fabra.csv", col_types = cols(CODI_ESTACIO = col_character(),
                                                                                                                                                                                                                                     DATA_LECTURA = col_character(),
                                                                                                                                                                                                                                     TM = col_double(),
                                                                                                                                                                                                                                     TX = col_double(),
                                                                                                                                                                                                                                     TN = col_double(),
                                                                                                                                                                                                                                     HRM = col_double(),
                                                                                                                                                                                                                                     PPT24H = col_double(),
                                                                                                                                                                                                                                     HPA = col_double(),
                                                                                                                                                                                                                                     RS24H = col_double(),
                                                                                                                                                                                                                                     VVM10 = col_double(),
                                                                                                                                                                                                                                     DVM10 = "skip",
                                                                                                                                                                                                                                     VVX10 = col_double(),
                                                                                                                                                                                                                                     DVX10 = "skip")) %>% mutate(date = as_date(DATA_LECTURA, format = "%d/%m/%Y")) %>% select(-DATA_LECTURA) %>% pivot_longer(cols = -c(date, CODI_ESTACIO), names_to = "weather_type", values_to = "valor") %>% clean_names() 

bcn_weather_2019_raval = read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/cf1de5ca-9d1c-424c-9543-8ab23e7f478e/resource/84f88831-58c0-47db-b486-9eac252564c6/download/2019_x4_barcelona_el_raval.csv") %>% mutate(date = as_date(DATA_LECTURA, format = "%d/%m/%Y")) %>% select(-DATA_LECTURA) %>% pivot_longer(cols = -c(date, CODI_ESTACIO), names_to = "weather_type", values_to = "valor") %>% clean_names() %>% filter(!weather_type %in% c("DVM10", "DVX10"))


bcn_weather_2019_zoo = read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/cf1de5ca-9d1c-424c-9543-8ab23e7f478e/resource/57a4b4b5-a45a-4743-8663-247083d2bed2/download/2019_x2_barcelona_zoo.csv") %>% mutate(date = as_date(DATA_LECTURA, format = "%d/%m/%Y")) %>% select(-DATA_LECTURA) %>% pivot_longer(cols = -c(date, CODI_ESTACIO), names_to = "weather_type", values_to = "valor") %>% clean_names() %>% filter(!weather_type %in% c("DVM10", "DVX10")) 

bcn_weather_2019_uni = read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/cf1de5ca-9d1c-424c-9543-8ab23e7f478e/resource/d7b5b67c-c3b6-415e-a1d6-c2845f2bed77/download/2019_x8_barcelona_zona_universitaria.csv") %>% mutate(date = as_date(DATA_LECTURA, format = "%d/%m/%Y")) %>% select(-DATA_LECTURA) %>% pivot_longer(cols = -c(date, CODI_ESTACIO), names_to = "weather_type", values_to = "valor") %>% clean_names() %>% filter(!weather_type %in% c("DVM10", "DVX10")) 


# Combining Everything ####
bcn_weather = bind_rows(list(bcn_weather_2023 %>% dplyr::select(-data_extrem), bcn_weather_2022 %>% dplyr::select(-data_extrem), bcn_weather_2021 %>% dplyr::select(-data_extrem), bcn_weather_2020 %>% dplyr::select(-data_extrem), meteo_cat_weather_last4d_a, bcn_weather_2019_fab, bcn_weather_2019_raval, bcn_weather_2019_zoo, bcn_weather_2019_uni)) %>% filter(!is.na(weather_type) & date >= as_date(android_start_date)) %>% group_by(date, weather_type) %>% summarise(bcn_mean = mean(valor, na.rm=TRUE), bcn_max = max(valor, na.rm=TRUE), bcn_min = min(valor, na.rm=TRUE)) %>% ungroup() #%>% filter(!weather_type %in% c("HRX", "HRN"))

bcn_weather_daily = bcn_weather %>% pivot_wider(id_cols = date, names_from = weather_type, values_from = bcn_mean, names_prefix = "mean") %>% left_join(bcn_weather %>% pivot_wider(id_cols = date, names_from = weather_type, values_from = bcn_max, names_prefix = "max")) %>% left_join(bcn_weather %>% pivot_wider(id_cols = date, names_from = weather_type, values_from = bcn_min, names_prefix = "min")) %>% ungroup() %>% mutate(
  FW = as.integer(meanVVM10 <= (6*3.6)*1000/(60*60)), 
  FH = case_when(meanHRM < 40~0, meanHRM >95~0, (meanHRM >=40 & meanHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FT = case_when(meanTM<=15~0, meanTM>30~0, (meanTM>15 & meanTM <=20)~ (.2*meanTM)-3, (meanTM>20 & meanTM<=25)~1, (meanTM>25 & meanTM <= 30)~ (-.2*meanTM)+6),
  mwi = FW*FH*FT,
  FWx = as.integer(maxVVX10 <= (6*3.6)*1000/(60*60)), 
  FHx = case_when(minHRM < 40~0, maxHRM >95~0, (minHRM >=40 & maxHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FTx = case_when(minTX<=15~0, maxTX>30~0, (minTX>15 & maxTX <=20)~ (.2*meanTX)-3, (minTX>20 & meanTX<=25)~1, (minTX>25 & maxTX <= 30)~ (-.2*meanTX)+6),
  mwix = FWx*FHx*FTx,
  mwi_zero = mwi==0, FH_zero = FH ==0, mwi_zeros_past_14d = roll_sum(mwi_zero, n=14, align="right", fill=NA, normalize=FALSE, na.rm=TRUE), FH_zeros_past_14d = roll_sum(FH_zero, n=14, align="right", fill=NA, normalize=FALSE, na.rm=TRUE))

bcn_weather_lags_7d = bcn_weather_daily %>% pivot_longer(cols=-date, names_to = "weather_type", values_to="bcn_mean") %>% group_by(weather_type) %>% arrange(date) %>% mutate(mean_past_7days = roll_mean(bcn_mean, n = 7, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE)) %>% select(-bcn_mean) %>% pivot_wider(names_from = weather_type, values_from = mean_past_7days) %>% ungroup() %>% mutate(
  FW7 = as.integer(meanVVM10 <= (6*3.6)*1000/(60*60)), 
  FH7 = case_when(meanHRM < 40~0, meanHRM >95~0, (meanHRM >=40 & meanHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FT7 = case_when(meanTM<=15~0, meanTM>30~0, (meanTM>15 & meanTM <=20)~ (.2*meanTM)-3, (meanTM>20 & meanTM<=25)~1, (meanTM>25 & meanTM <= 30)~ (-.2*meanTM)+6),
  mwi7 = mwi, PPT7 = meanPPT24H)

bcn_weather_lags_14d = bcn_weather_daily %>% pivot_longer(cols=-date, names_to = "weather_type", values_to="bcn_mean") %>% group_by(weather_type) %>% arrange(date) %>% mutate(mean_past_14days = roll_mean(bcn_mean, n = 14, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE)) %>% select(-bcn_mean) %>% pivot_wider(names_from = weather_type, values_from = mean_past_14days) %>% ungroup() %>% mutate(
  FW14 = as.integer(meanVVM10 <= (6*3.6)*1000/(60*60)), 
  FH14 = case_when(meanHRM < 40~0, meanHRM >95~0, (meanHRM >=40 & meanHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FT14 = case_when(meanTM<=15~0, meanTM>30~0, (meanTM>15 & meanTM <=20)~ (.2*meanTM)-3, (meanTM>20 & meanTM<=25)~1, (meanTM>25 & meanTM <= 30)~ (-.2*meanTM)+6),
  mwi14 = mwi, PPT14 = meanPPT24H)


bcn_weather_lags_30d = bcn_weather_daily %>% pivot_longer(cols=-date, names_to = "weather_type", values_to="bcn_mean") %>%  group_by(weather_type) %>% arrange(date) %>% mutate(mean_past_30days = roll_mean(bcn_mean, n = 30, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE)) %>% select(-bcn_mean) %>% pivot_wider(names_from = weather_type, values_from = mean_past_30days) %>% ungroup() %>% mutate(
  FW30 = as.integer(meanVVM10 <= (6*3.6)*1000/(60*60)), 
  FH30 = case_when(meanHRM < 40~0, meanHRM >95~0, (meanHRM >=40 & meanHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FT30 = case_when(meanTM<=15~0, meanTM>30~0, (meanTM>15 & meanTM <=20)~ (.2*meanTM)-3, (meanTM>20 & meanTM<=25)~1, (meanTM>25 & meanTM <= 30)~ (-.2*meanTM)+6),
  mwi30 = mwi, PPT30 = meanPPT24H)


bcn_weather_lags_21d_lag7 = bcn_weather_daily %>% pivot_longer(cols=-date, names_to = "weather_type", values_to="bcn_mean") %>%  group_by(weather_type) %>% arrange(date) %>% mutate(mean_past_21days = roll_mean(bcn_mean, n = 21, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE)) %>% select(-bcn_mean) %>% pivot_wider(names_from = weather_type, values_from = mean_past_21days) %>% ungroup() %>% mutate(
  FW21 = as.integer(meanVVM10 <= (6*3.6)*1000/(60*60)), 
  FH21 = case_when(meanHRM < 40~0, meanHRM >95~0, (meanHRM >=40 & meanHRM <= 95)~((meanHRM/55)-(40/55)) ),
  FT21 = case_when(meanTM<=15~0, meanTM>30~0, (meanTM>15 & meanTM <=20)~ (.2*meanTM)-3, (meanTM>20 & meanTM<=25)~1, (meanTM>25 & meanTM <= 30)~ (-.2*meanTM)+6),
  mwi21 = mwi, PPT21 = meanPPT24H, date = date+7) 


bcn_weather_ppt_lags = bcn_weather %>% filter(weather_type == "PPT24H") %>% arrange(date) %>% mutate(PPT_7d = roll_sum(bcn_mean, n = 7, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE),PPT_30d = roll_sum(bcn_mean, n = 30, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE), PPT_7d_8daysago = lag(PPT_7d, 8)) %>% select(-bcn_mean) 

# Saving ####
write_rds(bcn_weather_daily, "data/proc/a001_eic_bcn_weather_daily.Rds")

write_rds(bcn_weather_lags_7d, "data/proc/a001_eic_bcn_weather_lags_7d.Rds")

write_rds(bcn_weather_lags_14d, "data/proc/a001_eic_bcn_weather_lags_14d.Rds")

write_rds(bcn_weather_lags_30d, "data/proc/a001_eic_bcn_weather_lags_30d.Rds")

write_rds(bcn_weather_lags_21d_lag7, "data/proc/a001_eic_bcn_weather_lags_21d_lag7.Rds")

write_rds(bcn_weather_ppt_lags, "data/proc/a001_eic_bcn_weather_ppt_lags.Rds")

# ggplot(bcn_weather_daily %>% filter(year(date)>=2018), aes(x=date, y=meanTM)) + geom_line()
# ggplot(bcn_weather_daily %>% filter(year(date)>=2018), aes(x=date, y=mwix)) + geom_line()
# ggplot(bcn_weather_daily %>% filter(year(date)>=2018), aes(x=date, y=meanRS24H)) + geom_line()

range(bcn_weather_daily$date)
