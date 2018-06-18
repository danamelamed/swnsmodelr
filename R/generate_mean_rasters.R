# Calculate Daily Averages

temp_max_df <- temp_max_min_df %>%
  filter(str_detect(name, "max")) %>%
  dplyr::rename(temp_max = name)
temp_min_df <- temp_max_min_df %>%
  filter(str_detect(name, "min")) %>%
  dplyr::rename(temp_min = name)

temp_max_min_df <- temp_max_df %>%
  inner_join(temp_min_df, "date_time")


for(i in seq_along(temp_max_min_df[[1]])){
  #Daily Average
  
  date.now <- temp_max_min_df$date_time[[i]]
  temp_max_raster <- raster(temp_max_min_df$temp_max[[i]])
  temp_min_raster <- raster(temp_max_min_df$temp_min[[i]])
  temp_avg_raster <- (temp_max_raster + temp_min_raster)/2
  #plot(temp_avg_raster)
  print(date.now)
  temp_avg_raster@crs <- crs("+proj=utm +zone=20 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  writeRaster(temp_avg_raster, paste0("Z:\\Dana\\Daily_Averages\\",
                                      "temp_mean",
                                      date.now,
                                      ".tif"),
              overwrite = TRUE)