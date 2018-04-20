#' Extracts constant raster values to daily temperatures table
#' 

#' @param temperatures_df a dataframe of temperature data. Must contain 'stationid' field
#' @param rasters_list a list of constant rasters used as input for modelling
#' @return input temperatures_df with added columns, one for each raster in rasters_list
#' @export
extract_constant_raster_values <- function(temperatures_df, rasters_list){
  
  # Loop through each raster in list
  for(i in seq_along(rasters_list)){
    # Extract raster values to spatial points dataframe of SWNS stations
    stations_sp$extract <- raster::extract(x = rasters_list[[i]],
                                           y = stations_sp)
    # Rename extracted values column to the raster name
    names(stations_sp)[names(stations_sp) == "extract"] <- names(rasters_list[[i]])
  }
  # Coerce spatial points dataframe to dataframe, for joining
  stations_sp_df <- as.data.frame(stations_sp) %>%
    dplyr::select(-EASTING, -NORTHING)  # remove unnecessary rows
  stations_sp_df$stationid <- as.character(stations_sp_df$stationid) # coerce to char from factor
  
  # Join extracted raster values at each station back to input temperatures table
  out_df <- dplyr::left_join(x = temperatures_df,
                             y = stations_sp_df,
                             by = "stationid")
  return(out_df)
}