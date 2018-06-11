# calculate average of all GDDs for growing season in six years

gs_GDD5 <- swnsmodelr::make_temporal_raster_df('Z://Dana//ProjectOutputs//MonthlyGDD5_GrowingSeasons',
                                               start_date = ymd('2012-04-01'),
                                               end_date = ymd('2017-11-30'),
                                               date_chars = c(6, 15),
                                               date_format = "%Y-%m-%d")

gdd5_tot <- gs_GDD5 %>% filter(month(date_time) == 11)
par(mfrow = c(2,3))
gdd5_tot_paths <- gdd5_tot$path_field
gdd5_tot_rasters <- lapply(FUN = raster::raster, X = gdd5_tot_paths)
gdd5_tot_rasters %>% brick() %>% plot()
par(mfrow = c(3,2))

brick <- brick(gdd5_tot_rasters)
avg <- sum(brick)/6
plot(avg)

writeRaster(avg,"Z://Dana//ProjectOutputs//GDD5_avg_12_16_gs.tif")
