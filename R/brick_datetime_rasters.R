#' Adds rasters of datetime fields to brick
brick_datetime_rasters <- function(brick, template_raster, date.now){
  date_field <- c("yday","week","year")
  
  
  make_date_raster <- function(date_field, raster, date){
    date_field_func <- match.fun(date_field)
    raster[!is.na(raster)] <- date_field_func(date)
    names(raster) <- toString(date_field)
    return(raster)
  }
  
  datetime_rasters <- lapply(X = date_field, 
                             FUN= make_date_raster,
                             raster = template_raster, 
                             date = date.now)
  brick <- addLayer(brick, datetime_rasters)
  
}