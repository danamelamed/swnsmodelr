ext_stations_sp <- ext_stations_df %>%
                      filter(duplicated(stationid) == FALSE)
coordinates(ext_stations_sp) = ~ EASTING + NORTHING
plot(ext_stations_sp)
ext_stations_sp <- ext_stations_sp2
devtools::use_data(ext_stations_sp, overwrite = TRUE)


ggplot() + 
  geom_point(data = model_stations_df,
             mapping = aes(x = EASTING, y = NORTHING), colour = "blue") +
  geom_point(data = ext_stations_df_200,
             mapping = aes(x = EASTING, y = NORTHING), colour = "red")


test_stn <- swns_stations_df %>% filter(stationid == "6456")
ggplot(data = test_stn, mapping = aes(x = date_time, y = temp_mean)) +
  geom_point()


model_stations_df_200 <- swns_stations_df_200 %>%
                     filter(stationid %in% stations_sp$stationid |
                              stationid %in% c("6354","47187","6456"))

model_stations_df <- model_stations_df_200 %>%
                      select(stationid, temp_min, temp_max, temp_mean, EASTING, NORTHING)

model_stations_sp <- model_stations_df %>%
                      filter(!duplicated(stationid)) %>%
                      select(stationid, EASTING, NORTHING)
coordinates(model_stations_sp) = ~ EASTING + NORTHING

devtools::use_data(model_stations_sp, overwrite = TRUE)

ext_stations_df_200 <- swns_stations_df_200 %>%
  filter((stationid %in% stations_sp$stationid) == FALSE &
          ( stationid %in% c("6354","47187","6456")) == FALSE)

ext_stations_df <- ext_stations_df_200 %>%
  select(stationid, temp_min, temp_max, temp_mean, EASTING, NORTHING)

ext_stations_sp <- ext_stations_df %>%
  filter(!duplicated(stationid)) %>%
  select(stationid, EASTING, NORTHING)
coordinates(ext_stations_sp) = ~ EASTING + NORTHING

devtools::use_data(ext_stations_df_200, overwrite = TRUE)
