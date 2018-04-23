#' Extracts raster values from a set of rasters in the temporal_raster_df
#' 
#' 
extract_temporal_raster_values <- function(temporal_rasters_df, 
                                           temperatures_df, 
                                           stations = stations_sp){
  
  
  # Loop through each row in temporal_rasters_df
  for(i in seq_along(temporal_rasters_df[[1]])){
    
    # Current raster
    raster <- raster::raster(temporal_rasters_df[[1]][[i]])
    
    # Store date in names 
    names(raster) <- temporal_rasters_df[[2]][[i]] # names to xYYYY.MM.DD
    
    # Project raster to stations proj4string
    raster <- raster::projectRaster(from = raster, crs = stations@proj4string)
    
    stations$extract <- extract(raster, stations)
    names(stations)[names(stations) == "extract"] <- names(raster)
    
  }
}


