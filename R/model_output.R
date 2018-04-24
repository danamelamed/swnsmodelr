#' Models data and produces image outputs
#' @export

model_output <- function(max_model,
                         min_model,
                         max_alt_model,
                         min_alt_model,
                         output_folder,
                         output_start_date,
                         output_end_date,
                         output_format = "GTiff",
                         output_daily_max,
                         output_daily_min,
                         output_daily_avg,
                         constant_rasters_list,
                         temporal_rasters_df,
                         temporal_var_name){
  
  n_days <- (output_end_date - output_start_date + 1)[[1]]
  
  for(i in 1:n_days){
    
    # The current date
    date_now <- lubridate::ymd(output_start_date + i - 1)
    
    # Create brick of rasters used in model
    print("Bricking rasters...")
    brick <- raster::brick(constant_rasters_list)
    brick <- brick_datetime_rasters(brick, constant_rasters_list[[1]], date_now)
    
    # temporal_raster_df record for date_now
    temporal_raster_df_now <- temporal_rasters_df %>%
      dplyr::filter(date_time == date_now)
    
    # Check that temporal raster is available on this day
    if(length(temporal_raster_df_now != 0)){
      
      # If so...
      
      # Create raster object
      raster <- raster::raster(temporal_raster_df_now[[1]][[1]])
      
      # Add to brick
      names(raster)  <- temporal_var_name
      brick <- raster::addLayer(brick, raster)
      
      # Interpolate
      print("Modelling...")
      max_raster_out <- raster::predict(brick, max_model)
      min_raster_out <- raster::predict(brick, min_model)
    }else {
      
      # If not...
      
      # Interpolate without temporal varying raster input
      max_raster_out <- raster::predict(brick, max_model)
      min_raster_out <- raster::predict(brick, min_model)
    }
    
    # Calculate temp_avg raster
    avg_raster_out <- (max_raster_out + min_raster_out) / 2
    
    # Write outputs
    # Daily Minimum
    if(output_daily_min == TRUE){
      print("Writing daily minimum output...")
      min_raster_out@crs <- rasters_list[[1]]@crs
      raster::writeRaster(x = min_raster_out,
                  filename = paste0(output_folder,"\\",
                                   "daily_min_",
                                   date_now),
                  format = "GTiff",
                  overwrite = TRUE)
    } # Daily Maximum
    if(output_daily_max == TRUE){
      print("Writing daily maximum output...")
      max_raster_out@crs <- rasters_list[[1]]@crs
      raster::writeRaster(x = max_raster_out,
                  filename = paste0(output_folder,"\\",
                                   "daily_max_",
                                   date_now),
                  format = "GTiff",
                  overwrite = TRUE)
    } # Daily Average
    if(output_daily_avg == TRUE){
      print("Writing daily average output...")
      avg_raster_out@crs <- rasters_list[[1]]@crs
      raster::writeRaster(x = avg_raster_out,
                  filename = paste0(output_folder,"\\",
                                   "daily_avg_",
                                   date_now),
                  format = "GTiff",
                  overwrite = TRUE)
    }
    
  }
  
  # Completion message
  print(paste(date.now,"completed",sep = " "))
  print(Sys.time())
  
}

