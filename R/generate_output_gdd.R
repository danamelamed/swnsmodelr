#' Generates accumulated GDD output rasters
#' @param daily_avg_raster_df output from make_temporal_raster_df with folder 
#' of the daily temperature average rasters
#' @param output_time_slice "daily", "weekly", "monthly", or "yearly"
#' @export
#' 
generate_gdd_output <- function(daily_avg_raster_df,
                                gdd_base,
                                output_time_slice,
                                output_folder,
                                output_format = "GTiff"){
  
  # clip daily averages to start_date and end_date, add date fields, order
  daily_average_rasters_df_mod <- daily_average_rasters_df %>%
    dplyr::filter(date_time >= start_date &  date_time <= end_date) %>% # clip to start and end date
    dplyr::mutate(year  = year(date_time),                              # add date fields
           month = month(date_time),
           day   = day(date_time))  %>%
    dplyr::arrange(date_time)                                           # order by date
  
  
  # get all years in selected dates
  years <- daily_average_rasters_df_mod$year %>%
              unique()
  

  for(year_now in years){
    # Get daily average rasters in one year
    daily_average_rasters_yearly <- daily_average_rasters_df_mod %>%
      dplyr::filter(year == year_now)
    
    print("Creating daily average raster objects...")
    # Make raster objects from daily averages
    daily_average_rasters_list <- lapply(X = daily_average_rasters_yearly[[1]],
                                         FUN = raster)
    
    print("Generating daily GDD rasters...")
    # Apply GDD function
    apply_gdd <- function(raster, gdd_base){
      raster_out <- raster - gdd_base
      raster_out[raster_out < 0] <- 0
      return(raster_out)
    }
    daily_gdd_rasters_list <- lapply(X = daily_average_rasters_list, FUN = apply_gdd, gdd_base)
    
    
    gdd_out <- daily_gdd_rasters_list[[1]]
    acc_gdd <- gdd_out
    acc_gdd[acc_gdd] <- 0
    
    print("Accumulating GDD...")
    for(i in 2:(length(daily_gdd_rasters_list)+1)){
        date_now <- daily_average_rasters_yearly$date_time[[i-1]]
        if(output_time_slice == "daily"){
          print("Generating daily output")
          raster::writeRaster(x = gdd_out, 
                              filename = paste0(output_folder,'\\gdd',gdd_base,'_' ,date_now),
                              format   = output_format,
                              overwrite = TRUE)
          
        } else if(output_time_slice == "weekly"){
          print("Generating weekly output")
          # check if last of month
          if(wday(date_now) == 1){
            raster::writeRaster(x = gdd_out, 
                                filename = paste0(output_folder,'\\gdd',gdd_base,'_' ,date_now),
                                format   = output_format,
                                overwrite = TRUE)}
          }else if(output_time_slice == "monthly"){
            print("Generating monthly output")
          # check if last of month
          if(day(date_now) == days_in_month(date_now)[[1]]){
            raster::writeRaster(x = gdd_out, 
                                filename = paste0(output_folder,'\\gdd',gdd_base,'_' ,date_now),
                                format   = output_format,
                                overwrite = TRUE)
          }
        } else if(output_time_slice == "yearly"){
          print("Generating yearly output")
          # check if last day of year
          if(date_now == ymd(paste(year_now,12,31, sep = "-")))
            raster::writeRaster(x = gdd_out, 
                        filename = paste0(out_folder,'\\gdd',gdd_base,'_' ,date_now),
                        format   = output_format,
                        overwrite = TRUE)
        }
        
          
          
        # accumulate gdds
        acc_gdd <- gdd_out 
      }
    } 
  } 



