#' Generates accumulated GDD output rasters
#' @param daily_avg_raster_df output from make_temporal_raster_df with folder 
#' of the daily temperature average rasters
#' @param output_time_slice "daily", "weekly", "monthly", or "yearly"
#' @export
#' 
generate_gdd_output <- function(daily_average_rasters_df,
                                gdd_base,
                                start_date,
                                end_date,
                                output_time_slice,
                                growing_season = TRUE,
                                output_folder,
                                output_format = "GTiff",
                                plot_gdd_raster = FALSE){
  

  
  
  # Filter daily averages by start_date and end_date
  daily_average_rasters_df <- daily_average_rasters_df %>%
    dplyr::filter(date_time >= start_date &  date_time <= end_date) %>% # clip to start and end date
    dplyr::mutate(year  = lubridate::year(date_time),                              # add date fields
           month = lubridate::month(date_time),
           day   = wday(date_time))  %>%
    dplyr::arrange(date_time)                                           # order by date
  
  # Apply growing season restriction of april - november (inclusive)
  if(growing_season == TRUE){
    daily_average_rasters_df <- daily_average_rasters_df %>%
      dplyr::filter(between(month, 4, 11))
    print(unique(daily_average_rasters_df$month))
  }
  
  # get all years in selected dates
  years <- daily_average_rasters_df$year %>%
              unique()
  
  for(year_now in years){
    # Get daily average rasters in one year
    daily_average_rasters_year_now <- daily_average_rasters_df %>%
      dplyr::filter(year == year_now)
    
    
    # Make GDD raster of first day 
    # This day doesn't haven't any accumulation, stored before loop.
    daily_average_start <- raster(daily_average_rasters_year_now[[1]][[1]])
    gdd_out <- apply_gdd_base(raster = daily_average_start, gdd_base = gdd_base)
    
    
    
    # Start out with an accumulation layer with all zero  values
    acc_gdd <- gdd_out
    acc_gdd[acc_gdd] <- 0
    
    
    date_now <- daily_average_rasters_year_now$date_time[[1]]
    print("Accumulating GDD...")
        for(i in 2:length(daily_average_rasters_year_now[[1]])){
            

            if(output_time_slice == "daily"){
              writeGDDout(gdd_out, output_time_slice, gdd_base, date_now, plot_gdd_raster,
                          output_folder, output_format)  
              
             } else if(output_time_slice == "weekly"){
              # check if first day of week (sunday)
                  if(wday(date_now) == 1){ 
                    writeGDDout(gdd_out, output_time_slice, gdd_base, date_now, plot_gdd_raster,
                                output_folder, output_format) 
                    }
              } else if(output_time_slice == "monthly"){
              # check if last of month
                  if(day(date_now) == lubridate::days_in_month(date_now)[[1]]){
                    writeGDDout(gdd_out, output_time_slice, gdd_base, date_now, plot_gdd_raster,
                                output_folder, output_format) 
                  }
              } else if(output_time_slice == "yearly"){
              # check if last day of year
              if(date_now == ymd(paste(year_now,12,31, sep = "-")))
                
                writeGDDout(gdd_out,output_time_slice, gdd_base, date_now, plot_gdd_raster,
                            output_folder, output_format) 
            }
            
            
              
            
            
            # get next gdd day
            # Generate gdd for today, and add accumulation
            date_now <- daily_average_rasters_year_now$date_time[[i]]
            daily_average_now <- raster(daily_average_rasters_year_now[[1]][[i]])
            gdd_now <- apply_gdd_base(raster = daily_average_now, gdd_base = gdd_base)
            # accumulate gdds
            acc_gdd <- gdd_out
            gdd_out <- gdd_now + acc_gdd
            
            
            
          }
    } 
  } 



