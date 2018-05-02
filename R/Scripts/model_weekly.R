

# Make weekly models for mean temperature modelling

start_date = ymd('2012-01-01')
end_date   = ymd('2017-12-31')



# Sample of using package

#### Defining Input Matrix ####
# 1. Bring in rasters from project

# Set start and end dates of input data
start_date = ymd('2012-01-01') 
end_date   =  ymd('2012-12-31')

# a) Rasters that are constant through time
rasters_names_list <- c("dem","tpi","asp","east","north","ptoc")
rasters_list <- lapply(FUN = raster,
                       X   = paste0("Rasters\\200\\",rasters_names_list, ".tif"))

# I want to change aspect so be more cirular
rasters_list[[3]][rasters_list[[3]] == -1 ] <- NA
rasters_list[[3]] <- abs(rasters_list[[3]] - 180)
names(rasters_list[[3]]) <- "asp_c"

# I want to cap PTOC to 30km
rasters_list[[6]][rasters_list[[6]] > 30000] <- 30000


# b) Rasters that vary throughout time
solar_irradiance_rasters_df <- make_temporal_raster_df(
  in_folder = "F:\\GOES_200",
  start_date = start_date,
  end_date   = end_date,
  date_chars = c(16,-5),
  date_format = "%Y_%j",
  extension = ".tif")

# Resample constant rasters to temporal rasters
# Testing supports basically the same, much faster
rasters_list <- lapply(FUN = raster::resample,
                       X   = rasters_list,
                       method = "bilinear",
                       raster(solar_irradiance_rasters_df[[1]][[1]]))

libary# Filter daily temperatures table by start and end date
daily_temperatures_filter <- filter(daily_temperatures_df,
                                    date_time >= start_date &
                                      date_time <= end_date)

# c) Add extracted values for each constant raster to temperature table
daily_temperatures_constant <- extract_constant_raster_values(
  temperatures_df = daily_temperatures_filter,
  rasters_list    = rasters_list)



# d) Add extracted values for temporally varying rasters to temperature table
daily_temperatures_temporal <- extract_temporal_raster_values(solar_irradiance_rasters_df, 
                                                         daily_temperatures_constant,
                                                         col_name = "sum_irradiance",
                                                         stations = stations_sp,
                                                         verbose  = TRUE)

model_df_200 <- dail_temperatures_temporal

# filter model table
model_df_200$sum_irradiance[model_df_200$sum_irradiance == 0] <- NA
df <- filter(model_df_200,
             date_time >= start_date &
               date_time <= end_date)


# count weeks
weeks <- df$week %>% unique() 
n_weeks <- length(weeks)
# store all summaries
summaries <- list()
summaries2 <- list() #without solar irradiace


# loop over weeks
for(i in 1:n_weeks){
  # filter by week
  df_week <- df %>% 
    filter(week == weeks[[i]])
  print(i)
  # generate weekly model
  m <- gam(temp_mean ~
             s(east, north) +
             s(dem, yday, k = 20)   +
             s(sum_irradiance, yday) + 
             s(tpi, yday)   +
             s(asp_c, yday) +
             s(ptoc, k = 3),
           data = df_week, na.action = na.omit)
  summaries[[i]] <- summary(m)
  print(summary(m))
  
  m2 <- gam(temp_mean ~
             s(east, north) +
             s(dem, yday)   +
             s(asp_c, yday) +
             s(tpi, yday)   +
             s(ptoc, k = 3),
           data = df_week, na.action = na.omit)
  summaries2[[i]] <- summary(m2)
  print(summary(m2))
  
  # loop through days in week
  for(j in 1:length(unique(df_week$yday))){
    date_now <- df_week$date_time[[j]]
    print(date_now)
    # create raster brick
    brick <- brick(rasters_list)
    brick <- brick_datetime_rasters(brick, rasters_list[[1]], date_now)
    
    # check for solar raster
    solar_irradiance_now <- solar_irradiance_rasters_df %>%
      filter(date_time == date_now)
    if(length(solar_irradiance_now[[1]] != 0)){
      # add solar raster to brick
      solar_raster <- raster(solar_irradiance_now[[1]][[1]])
      names(solar_raster) <- "sum_irradiance"
      solar_raster[solar_raster == 0] <- NA
      brick <- raster::addLayer(brick, solar_raster)
      
      # model over day
      prediction_raster <- predict(brick, m)
      plot(prediction_raster)
      
      # output prediction temp mean raster
      writeRaster(prediction_raster, paste0("Z:\\Dana\\Weekly\\Daily_Temp_Mean\\temp_mean",
                                            date_now,".tif"),
                  overwrite = TRUE)
    }else{
      # predict without solar raster
      # model over day
      prediction_raster <- predict(brick, m2)
      plot(prediction_raster)
      
      # output prediction temp mean raster
      writeRaster(prediction_raster, paste0("Z:\\Dana\\Weekly\\Daily_Temp_Mean\\temp_mean",
                                            date_now,".tif"),
                  overwrite = TRUE)
    }
    
    
  }
}


# Create GDDs
temp_mean_df <- make_temporal_raster_df("Z:\\Dana\\Weekly\\Daily_Temp_Mean",
                                        start_date,
                                        end_date,
                                        date_chars = c(10,19),
                                        date_format = "%Y-%m-%d")

generate_gdd_output(temp_mean_df, 
                    gdd_base = 5,
                    start_date = start_date,
                    end_date = end_date,
                    output_time_slice = "monthly",
                    growing_season = FALSE,
                    output_folder = "Z:\\Dana\\Weekly\\Monthly_GDD5",
                    plot_gdd_raster = TRUE)

ext_df <- extract_dated_rasters_stations(ext_df, temp_mean_df)
