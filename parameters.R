
end_date =  as.POSIXct(strptime('2016-01-01 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))
android_start_date = as.POSIXct(strptime('2014-06-14 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))
ios_start_date = as.POSIXct(strptime('2014-06-24 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))
season_day_0_2014 = as.POSIXct(strptime('2014-01-01 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))
season_day_0_2015 = as.POSIXct(strptime('2015-01-01 00:00:00.000000+0000', format="%Y-%m-%d %H:%M:%S%OS%z", tz="gmt"))

Apr1Julian = as.POSIXlt("01-03-2015", format = "%d-%m-%Y")$yday
Dec1Julian = as.POSIXlt("01-12-2015", format = "%d-%m-%Y")$yday
Aug3Julian = as.POSIXlt("03-08-2015", format = "%d-%m-%Y")$yday
Jun21Julian = as.POSIXlt("21-06-2015", format = "%d-%m-%Y")$yday
Sep23Julian = as.POSIXlt("23-09-2015", format = "%d-%m-%Y")$yday
Sep1Julian = as.POSIXlt("01-09-2015", format = "%d-%m-%Y")$yday
Sep30Julian = as.POSIXlt("30-09-2015", format = "%d-%m-%Y")$yday


PROJ4_UTM_SPAIN = " +proj=utm +zone=32N +ellps=WGS84"
PROJ4_UTM_SPAIN_31N = " +proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
LONLAT = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

spain_mainland_bb = c(-9.381002903,4.4056243896,35.9826789363,43.8025372589)