# Add new 2017 data 

# see what data is new....

# current 2017 data
swns_stations_df_200 <- add_date_columns(swns_stations_df_200)
current_2017 <- swns_stations_df_200 %>%
  filter(year == 2017)

# new 2017 data from david
new_2017_in <- read.csv(file.path("data","temperature2017ss.csv"))
names(new_2017)
# format data frame
new_2017 <- new_2017_in %>%
  mutate(date_time = as.Date(paste(yr,month,days, sep = "-"),
                             format = "%Y-%m-%d")) %>%
  select(stationid, date_time, temp_mean = avgtempc,
         temp_min = mintempc, temp_max = maxtempc)


# only keep new_2017 rows that arent in current_2017
new_2017_aj <- new_2017 %>%
  anti_join(current_2017, by = c("stationid","date_time")) %>%
  # only keep stations from other years (geographic data present)
  filter(stationid %in% current_2017$stationid) %>%
  # join easting and northing data
  left_join(as.data.frame(swns_stations_sp), by = "stationid")

# extract constant raster data to table
new_2017_constant_rasters <- swnsmodelr::extract_constant_raster_values(new_2017_aj,
                                                                        rasters_list)

# extract solar rad data to table
#a)
solar_irradiance_rasters_df_2017 <- make_temporal_raster_df(
  in_folder = "F:\\GOES_200",
  start_date = ymd('2017-01-01'),
  end_date   = ymd('2017-12-31'),
  date_chars = c(16,-5),
  date_format = "%Y_%j",
  extension = ".tif")
#b)
new_2017_solar_radiation <- extract_temporal_raster_values(temporal_rasters_df = solar_irradiance_rasters_df_2017,
                                                           temperatures_df = new_2017_constant_rasters,
                                                           col_name = "sum_irradiance",
                                                           verbose = TRUE)

# add dates 
new_2017_200 <- add_date_columns(new_2017_solar_radiation)

# combine current and new 2017 data frames
all_2017 <- bind_rows(new_2017_200, swns_stations_df_200)


# select stations for modelling



                                                      