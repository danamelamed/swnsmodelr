#' Generate daily mean temperature raster with GAM
predict_rasters <- function(start_date,
                                          end_date,
                                          #formula,
                                          #alt_formula,
                                          temperatures_df,
                                          validation_df,
                                          brick,
                                          temporal_rasters_df = 1,
                                          output_folder,
                                          output_ext = ".tif",
                                          verbose = TRUE,
                                          resid_plot = FALSE
                                          ){

        
        
        daily_val_df_out = validation_df %>% 
                filter(as.character(date_time) == "impossible")
        daily_val_df_month = validation_df %>% 
                filter(as.character(date_time) == "impossible")
        empty_df = validation_df %>% 
                filter(as.character(date_time) == "impossible")
        current_month = month(start_date)
        for (i in 1:(end_date-start_date)[[1]]){
                date_now <- (start_date + i) - 1 
                
                # subset stations by date
                daily_df = temperatures_df %>% filter(date_time == date_now)
                daily_val_df = validation_df %>% filter(date_time == date_now)
                
                
        
        
                ### THE MODELS
                # There were erroneous zeroes in sum_irradiance which were converted to NA.
                # Because of that process, an error may error in generating the model.
                # If it does, a back-up model without sum_irradiance is run
                m <- try(gam(temp_mean ~
                           s(east,north) +
                           #s(east, k =3) +
                           #s(north, k= 3) +
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
                                       #s(east, k =3) +
                                       #s(north, k= 3)+
                                       s(dem, k =3) +
                                       s(tpi) +
                                       s(ptoc, k= 3) +
                                       s(asp, k= 3),
                               data = daily_df)}
        
                    
                 if(verbose){
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
                daily_val_df <- daily_val_df %>% mutate(ID = 1:n()) %>%
                        filter(!is.na(resid))
                
                
                daily_val_df_out <- bind_rows(daily_val_df_out, daily_val_df)
               
               
                

                # plot rasters and residuals for each day
                if(verbose){
                # plot predicted raster 
                        par(mfrow = c(1,2))
                        plot(prediction_raster)
                        # add validation stations
                        points(daily_val_df$east, daily_val_df$north, size = daily_val_df$resid)
                        text(daily_val_df$east, daily_val_df$north,labels= daily_val_df$ID)
                        
                        
                        # plot actual and predicted validation values
                        plot(daily_val_df$ID,daily_val_df$temp_mean)
                        points(daily_val_df$ID, daily_val_df$pred, pch=20 , col="red")
                }
                
                #plot residual box plots for each month
                if(resid_plot){
                   print(date_now)
                   month_now <- month(date_now)
                   daily_val_df_month <- bind_rows(daily_val_df_month, daily_val_df)
                   if(month_now != current_month | date_now == (end_date-1)){
                           boxplot(resid ~ date_time, daily_val_df_month, ylim = c(-5,5))
                           current_month <- month_now
                           daily_val_df_month <- empty_df
                           
                   }
                        
                        
                        
                }
              
}
          
          
   return(daily_val_df_out)
        
        
        
        
        
}



