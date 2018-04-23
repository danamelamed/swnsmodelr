#' Extracts raster values from a set of rasters in the temporal_raster_df
#' @return temperatures table with temporal raster values
#' @export
extract_temporal_raster_values <- function(temporal_rasters_df, 
                                           temperatures_df,
                                           col_name = "temporal_raster_value",
                                           stations = stations_sp,
                                           verbose  = TRUE){
  
  # Arrange by date
  temporal_rasters_df <- temporal_rasters_df %>%
    arrange(date_time)
  
  # Loop through each row in temporal_rasters_df
  for(i in seq_along(temporal_rasters_df[[1]])){
    
    # Current raster
    raster <- raster::raster(temporal_rasters_df[[1]][[i]])
    
    # Store date in names 
    date_now <- temporal_rasters_df[[2]][[i]]
    names(raster) <-  date_now # names to xYYYY.MM.DD
    
    # Project raster to stations proj4string
    raster <- raster::projectRaster(from = raster, crs = stations@proj4string)
    
    stations$extract <- raster::extract(raster, stations)
    names(stations)[names(stations) == "extract"] <- names(raster)
    
    # Print message for completed date
    if(verbose == TRUE){
      print(paste("Extracted raster values from",
                  date_now,
                  "completed.",
                  sep = " "))
    }
    
  }
  # Coerce stations to dataframe
  extracts_df <- as.data.frame(stations)
  
  # Melt dataframe by date
  extracts_df <- reshape2::melt(extracts_df, id = c("stationid","EASTING","NORTHING")) %>%
    dplyr::mutate(variable = str_sub(variable, 2, 11)) %>%
    dplyr::mutate(date_time = as.Date(variable, format = "%Y.%m.%d")) %>%
    dplyr::select(stationid, value, date_time) 
  
  # Rename new column
  names(extracts_df)[names(extracts_df) == "value"] <- col_name
  
  
  # Join back to temperatures_df
  out_df <- dplyr::left_join(temperatures_df, extracts_df,
                      by = c("stationid", "date_time"))
  
  
  
  
  return(out_df)
  
}


