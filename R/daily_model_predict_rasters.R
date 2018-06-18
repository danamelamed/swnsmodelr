#' Generate daily mean temperature raster with GAM
predict_mean_daily_rasters <- function(start_date,
                                          end_date,
                                          formula,
                                          alt_formula,
                                          temperatures_df,
                                          raster_brick,
                                          temporal_rasters_df = 1,
                                          output_folder,
                                          output_ext,
                                          verbose = TRUE
                                          ){

        
        start_date = ymd('2017-04-26')
        end_date   = ymd('2017-12-31')
        
        
        
        # Sample of using package
        
        
        # filter model table
        df <- filter(model_stations_df,
                     date_time >= start_date &
                       date_time <= end_date)
        
        
        
        
        years <- 2017
        n_years <- years %>% length()
        
        no_sol_model <- list()
        
        for(j in 1:n_years){
          
          df_year <- filter(df,
                            year == years[[j]])
          
          days <- df_year$yday %>% unique() 
          n_days <- length(days)
          # loop over weeks
          for(i in 1:n_days){
            
            ### GENERATE WEEKLY MODELS
            
            # Get weekly df
            daily_df <- df_year %>% 
              filter(yday == days[[i]])
            print(i)
            ### THE MODELS
            # There were erroneous zeroes in sum_irradiance which were converted to NA.
            # Because of that process, an error may error in generating the model.
            # If it does, a back-up model without sum_irradiance is run
            m <- try(gam(temp_mean ~
                           s(east,north) +
                           s(dem) +
                           s(sum_irradiance) +
                          # s(tpi, k=9) +
                           s(ptoc, k= 3) ,
                          # s(asp_c, k= 9),
                         data = daily_df))
            # Back-up model
            if("try-error" %in% class(m)){
              m <- gam(temp_mean ~
                         s(east,north) +
                         s(dem, k=9) +
                         # s(sum_irradiance) +
                        # s(tpi, k=9) +
                         s(ptoc) ,
                       #  s(asp_c, k= 9),
                       data = daily_df)}
            # No temporal raster available
            m2 <- gam(temp_mean ~
                        s(east,north) +
                        s(dem) +
                        #  s(sum_irradiance) +
                      #  s(tpi, k=9) +
                        s(ptoc, k=3) ,
                     #   s(asp_c, k= 9),
                      data = daily_df)
            
            if(verbose == TRUE)
            print(summary(m))
            print(summary(m2))
            
            
            
              
              # The date of the current raster
              date_now <- daily_df$date_time %>% unique()
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
                writeRaster(prediction_raster, paste0("Z:\\Dana\\Daily\\Daily_Temp_Mean_200_2\\temp_mean",
                                                      date_now,".tif"),
                            overwrite = TRUE)
              }else{
                
                # Solar raster available, run m as model
                prediction_raster <- predict(brick, m2)
                plot(prediction_raster)
                
                # output prediction temp mean raster
                writeRaster(prediction_raster, paste0("Z:\\Dana\\Daily\\Daily_Temp_Mean_200_2\\temp_mean",
                                                      date_now,".tif"),
                            overwrite = TRUE)
                no_sol_model[[length(no_sol_model) + 1]] <- date_now
              }
              
              
          }
          
          
        }
        
        start_date = ymd('2012-01-01')
        end_date = ymd('2014-12-31')
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
        
        
        
        
}



# example code for script 

predict_average_daily_rasters(start_date = ymd('2012-01-01'),
                               end_date = ymd('2017-12-31'),
                               formula = "temp_mean ~
                                          s(east,north) +
                                          s(dem) +
                                          s(sum_irradiance) +
                                          s(ptoc, k= 3)",
                               alt_formula = "temp_mean ~
                                          s(east,north) +
                                          s(dem) +
                                          s(ptoc, k= 3)",
                               temperatures_df = model_stations_df,
                               raster_brick = brick,
                               temporal_rasters_df = solar_irradiance_rasters_df,
                               output_folder = "F://daily_models//mean_temperature",
                               output_ext = "tif",
                               verbose = TRUE)