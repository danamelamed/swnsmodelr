#' Generate daily mean temperature raster with GAM
predict_mean_daily_rasters <- function(start_date,
                                          end_date,
                                          #formula,
                                          #alt_formula,
                                          temperatures_df,
                                          validation_df,
                                          brick,
                                          temporal_rasters_df = 1,
                                          output_folder,
                                          output_ext = ".tif",
                                          verbose = TRUE
                                          ){

        par(mfrow = c(2,2))
        for (i in 1:(start_date-end_date)[[1]]){
                date_now <- start_date  - 1 + i
                
                # subset stations by date
                daily_df = temperatures_df %>% filter(date_time == date_now)
                daily_val_df = validation_df %>% filter(date_time == date_now)
                
                
        
        
                ### THE MODELS
                # There were erroneous zeroes in sum_irradiance which were converted to NA.
                # Because of that process, an error may error in generating the model.
                # If it does, a back-up model without sum_irradiance is run
                m <- try(gam(temp_mean ~
                           s(east,north) +
                           s(east, k =3) +
                           s(north, k= 3) +
                           s(dem, k =3) +
                           s(sum_irradiance) +
                           s(tpi) +
                           s(ptoc, k= 3) +
                           s(asp, k= 3),
                         data = daily_df))
                
                
                
                # Back-up model
                if("try-error" %in% class(m)){
                        m <- gam(temp_mean ~
                                       s(east,north) +
                                       s(east, k =3) +
                                       s(north, k= 3)+
                                       s(dem, k =3) +
                                       s(tpi) +
                                       s(ptoc, k= 3) +
                                       s(asp, k= 3),
                               data = daily_df)}
        
                    
                 if(verbose == TRUE){
                   print(summary(m))}
                   
                    
                    
                    
                
                # check for solar raster
                solar_irradiance_now <- solar_irradiance_rasters_df %>%
                  filter(date_time == date_now)
                
                # Solar raster is available...
                if(length(solar_irradiance_now[[1]] != 0)){
                # add solar raster to brick
                  solar_raster <- raster(solar_irradiance_now[[1]][[1]])
                  names(solar_raster) <- "sum_irradiance"
                  brick_temp <- raster::addLayer(brick, solar_raster)
                        
                  # model over day
                  prediction_raster <- predict(brick_temp, m)
                  
                  
                  
                        
                  # output prediction temp mean raster
                  writeRaster(prediction_raster, file.path(output_folder,
                                                            paste0(date_now, output_ext)),
                                  overwrite = TRUE)
                  }else{
                        
                     # Solar raster available, run m as model
                     prediction_raster <- predict(brick, m)
                     
                        
                      # output prediction temp mean raster
                      writeRaster(prediction_raster, file.path(output_folder,
                                                               paste0(date_now, output_ext)),
                                  overwrite = TRUE)
                      
                  }
                
                
                daily_val_df <- add_predictions(daily_val_df, m)
                daily_val_df <- add_residuals(daily_val_df, m)
                
                # reorder validation stations and give new ID
                daily_val_df <- daily_val_df[order(daily_val_df$temp_mean),]
                daily_val_df <- daily_val_df %>% mutate(ID = 1:n())
                      
                # plot predicted raster 
                plot(prediction_raster)
                # add validation stations
                points(daily_val_df$east, daily_val_df$north, size = daily_val_df$resid)
                text(daily_val_df$east, daily_val_df$north,labels= daily_val_df$ID)
                
                
                # plot actual and predicted validation values
                
                
                plot(daily_val_df$ID,daily_val_df$temp_mean)
                points(daily_val_df$ID, daily_val_df$pred, pch=20 , col="red")
                
                
                
              
}
          
          
   
        
        
        
        
        
}



