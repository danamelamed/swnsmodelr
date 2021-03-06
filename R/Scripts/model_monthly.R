

# Make weekly models for mean temperature modelling

start_date = ymd('2015-01-01')
end_date   = ymd('2017-12-31')



# Sample of using package

#### Defining Input Matrix ####
# 1. Bring in rasters from project

# filter model table
df <- filter(model_stations_df,
             date_time >= start_date &
               date_time <= end_date)
df <- df %>% filter(week != 53)


# store all summaries
summaries <- list()
summaries2 <- list() #without solar irradiace

years <- 2015:2017
n_years <- years %>% length()

no_sol_model <- list()

for(j in 1:n_years){
  
  df_year <- filter(df,
                    year == years[[j]])
  
  weeks <- df_year$week %>% unique() 
  n_weeks <- length(weeks)
  # loop over weeks
  for(i in 1:n_weeks){
    
    ### GENERATE WEEKLY MODELS
    
    # Get weekly df
    weekly_df <- df_year %>% 
      filter(week == weeks[[i]])
    print(i)
    ### THE MODELS
    # There were erroneous zeroes in sum_irradiance which were converted to NA.
    # Because of that process, an error may error in generating the model.
    # If it does, a back-up model without sum_irradiance is run
    m <- try(gam(temp_mean ~
                   s(east,north,k = 9) +
                   s(dem, yday, k = 9) +
                   s(sum_irradiance, yday, k= 9) +
                   # s(tpi, yday, k = 9) +
                   s(ptoc,yday, k = 3) ,
                 # s(asp_c,yday, k= 9),
                 data = weekly_df))
    # Back-up model
    if("try-error" %in% class(m)){
      m <- gam(temp_mean ~
                 s(east,north, k = 9) +
                 s(dem, yday, k = 9) +
                 # s(sum_irradiance, yday) +
                 #s(tpi, yday, k = 9) +
                 s(ptoc,yday, k = 3) ,
               # s(asp_c,yday, k= 9),
               data = weekly_df)}
    # No temporal raster available
    m2 <- gam(temp_mean ~
                s(east,north, k = 9) +
                s(dem, yday, k= 9) +
                #  s(sum_irradiance, yday) +
                # s(tpi, yday, k = 9) +
                s(ptoc,yday, k = 3) ,
              # s(asp_c,yday, k= 9),
              data = weekly_df)
    
    
    print(summary(m))
    print(summary(m2))
    
    
    # Loop through days in modelled week
    for(j in 1:length(unique(weekly_df$yday))){
      
      # The date of the current raster
      date_now <- weekly_df$date_time[[j]]
      print(date_now)
      
      # Make a raster brick for the prediction
      brick <- brick(rasters_list)
      brick <- brick_datetime_rasters(brick, rasters_list[[1]], date_now)
      
      # check for solar raster
      solar_irradiance_now <- solar_irradiance_rasters_df %>%
        filter(date_time == date_now)
      
      # Solar raster is available...
      if(length(solar_irradiance_now[[1]] != 0)){
        # add solar raster to brick
        solar_raster <- raster(solar_irradiance_now[[1]][[1]])
        names(solar_raster) <- "sum_irradiance"
        brick <- raster::addLayer(brick, solar_raster)
        
        # model over day
        prediction_raster <- predict(brick, m)
        plot(prediction_raster)
        
        # output prediction temp mean raster
        writeRaster(prediction_raster, paste0("Z:\\Dana\\Weekly\\Daily_Temp_Mean_200_7\\temp_mean",
                                              date_now,".tif"),
                    overwrite = TRUE)
      }else{
        
        # Solar raster available, run m as model
        prediction_raster <- predict(brick, m2)
        plot(prediction_raster)
        
        # output prediction temp mean raster
        writeRaster(prediction_raster, paste0("Z:\\Dana\\Weekly\\Daily_Temp_Mean_200_7\\temp_mean",
                                              date_now,".tif"),
                    overwrite = TRUE)
        no_sol_model[[length(no_sol_model) + 1]] <- date_now
      }
      
      
    }
  }
  
}

start_date = ymd('2015-01-01')
end_date = ymd('2017-12-31')
# Create GDDs
temp_mean_df <- make_temporal_raster_df("Z:\\Dana\\Weekly\\Daily_Temp_Mean_200_7",
                                        start_date,
                                        end_date,
                                        date_chars = c(10,19),
                                        date_format = "%Y-%m-%d")

generate_gdd_output(temp_mean_df,
                    gdd_base = 5,
                    start_date = start_date,
                    end_date = end_date,
                    output_time_slice = "monthly",
                    growing_season = TRUE,
                    output_folder = "Z:\\Dana\\Weekly\\Monthly_GDD_200_7",
                    plot_gdd_raster = TRUE)

ext_df <- extract_dated_rasters_stations(ext_df, temp_mean_df)



models_2012_1 <- gam_models_1
models_2012_2 <- gam_models_2


capture.output(for (i in 1:length(models_2012_1)) print(summary(models_2012_1)),
               file = "F:\\test.txt")

val_knots_daily_df$timeframe <- "Daily"
val_knots_weekly_df$timeframe <- "Weekly"
val_knots_monthly_df$timeframe <- "Monthly"
val_knots_monthly_df$ind_var <- val_knots_monthly_df$temp_var
bind_knots <- bind_rows(val_knots_daily_df, val_knots_monthly_df, val_knots_weekly_df)

ggplot(data = bind_knots, aes(x = yday, y = gcv)) +
  geom_smooth(aes(colour = timeframe)) +
  facet_wrap(~knots, ncol = 4)

ggplot(data = bind_knots, aes(x = temp_mean, y = resid)) +
  geom_smooth(aes(colour = timeframe)) +
  facet_wrap(~knots, ncol = 4)


ggplot(data = bind_knots, aes(x = yday, y = abs_resid)) +
  geom_smooth(aes(colour = timeframe)) +
  facet_wrap(~knots, ncol = 4)

ggplot(data = bind_knots, aes(x = yday, y =resid)) +
  geom_smooth(aes(colour = timeframe)) +
  facet_wrap(~knots, ncol = 4)
bind_knots <- bind_knots %>% filter(ind_var == "mean" ) 
bind_knots$timeframe <- factor(bind_knots$timeframe, levels = c("Daily",
                                                                "Weekly",
                                                                "Monthly"))

ggplot(bind_knots, aes(resid, fill = knots))+
  labs(x = "Residuals",y = "Count") +
  geom_histogram(position = "dodge",  colour = "black", bins =10)+
  scale_fill_manual(values = c("steelblue2",
                               "royalblue1",
                               "blue3",
                               "grey")) +
  guides(fill = guide_legend(title = "Knots")) +
  facet_wrap(~timeframe, ncol = 3)

ggplot(bind_knots, aes(resid, fill = timeframe))+
  labs(x = "Residuals",y = "Count") +
  geom_histogram(position = "dodge",  colour = "black", bins = 15)+
  scale_fill_manual(values = c("brown1","navajowhite2","steelblue2")) +
  guides(fill = guide_legend(title = "Timeframe")) +
  facet_wrap(~knots, ncol = 4)

bind_val %>% filter(timeframe == "monthly") %>% 
  select(rsq) %>%
  unique() %>% 
  mutate(max = max(rsq), min = min(rsq))

bind_val %>% filter(timeframe == "weekly") %>% 
  select(rsq) %>%
  unique() %>% 
  mutate(max = max(rsq), min = min(rsq))


bind_val %>% filter(timeframe == "daily") %>% 
   filter(temp_var!= "min") %>%
  filter(rsq > 0.2) %>%
  select(rsq) %>%
  unique() %>% 
  mutate(max = max(rsq), min = min(rsq))


