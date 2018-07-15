# Prepare input rasters

## 1. Data
### Study area polygon
study_area <- rgdal::readOGR(dsn = file.path("E:","study_area"), layer = "study_area")

### DEM 
DEM <- raster::raster("C://nsdnr//dp055v2gr//e055ns20//grid//dem020hy")

### Other input rasters


## Crop, reproject and resample the DEM
DEM <- raster::crop(DEM, study_area) # crop & reproject
temp_raster <- raster(ext = extent(DEM),
                      resolution = res(DEM)) # dummy raster to resample to
DEM <- raster::resample(DEM, temp_raster, method = "bilinear") 
par(mfrow = c(2,2))


# Create raster brick
## Set exactly similar properties by resampling to solar raster
solar_raster_df <- make_temporal_raster_df("E://GOES_200m",
                                        ymd('2016-06-01'),
                                        ymd('2016-06-08'),
                                        date_chars= c(16,-4),
                                        date_format = "%Y_%j")
solar_raster <- raster(solar_raster_df[[1]][[1]])
for(i in seq_along(rasters_list)){
  rasters_list[[i]] <- raster::resample(rasters_list[[i]], solar_raster)
}


# Modify the Aspect and PTOC Rasters 

##### Recalculate Aspect Raster #####
asp_raster_in <- raster(file.path("Rasters","200","asp.tif"))
asp_raster_out <- abs(asp_raster - 180)
writeRaster(asp_raster_out, file.path("Rasters","200","asp.tif"),
            overwrite = TRUE)

##### Limit Proximity to the Coastline Raster #####
limit <- 30000 # metres
ptoc_raster_in <- raster(file.path("Rasters","200","ptoc.tif"))
ptoc_raster_out <- ptoc_raster_in
ptoc_raster_out[ptoc_raster_in >= limit] <- limit

gam_six_years_lim <- gam(temp_mean ~ 
                           s(dem, k = 9) +
                           s(ptoc, k = 3) + 
                           s(sum_irradiance, k= 9) + 
                           s(east,north),
                         data = model_stations_df)


# Add stations easting and northing to data frame

ns_stations_in <- rgdal::readOGR(dsn = file.path("E:","WeatherData"), layer = "NSWeatherStns")
ns_stations <- ns_stations %>% dplyr::select(stationid = StnID, NORTHING = Northing, EASTING = Easting)

daily_temperatures_df <- left_join(daily_temperatures_df, ns_stations, by = "stationid")