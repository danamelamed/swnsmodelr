#' Extracts constant raster values to daily temperatures table
#' 

#' @param temperatures_df a dataframe of temperature data. Must contain 'stationid' field
#' @param rasters_list a list of constant rasters used as input for modelling
#' @param stations_sp a SpatialPointsDataFrame object of weather stations. names() must be "stationid"
#' with AN1, AN2, AN3 ... etc.
#' @return input temperatures_df with added columns, one for each raster in rasters_list
#' @export
extract_constant_raster_values <- function(temperatures_df,
                                           rasters_list){
  stations <- temperatures_df %>% 
    filter(!duplicated(stationid)) %>%
    dplyr::select(stationid, EASTING, NORTHING)
  coordinates(stations) = ~ EASTING + NORTHING
  
  # Loop through each raster in list
  for(i in seq_along(rasters_list)){
    raster <- rasters_list[[i]]
    # Extract raster values to spatial points dataframe of SWNS stations
    stations$extract <-(raster::extract(x = raster,
                                           y = stations))
    # Rename extracted values column to the raster name
    names(stations)[names(stations) == "extract"] <- names(raster)
  }
  # Coerce spatial points dataframe to dataframe, for joining
  stations_df <- as.data.frame(stations) %>%
    dplyr::select(-EASTING, -NORTHING)  # remove unnecessary rows
  stations_df$stationid <- as.character(stations_df$stationid) # coerce to char from factor
  
  # Join extracted raster values at each station back to input temperatures table
  out_df <- dplyr::left_join(x = temperatures_df,
                             y = stations_df,
                             by = "stationid")
  return(out_df)
}