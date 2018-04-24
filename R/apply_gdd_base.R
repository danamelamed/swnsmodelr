#' Applies the base GDD level to an average daily raster
#' 

apply_gdd_base <- function(raster, gdd_base){
  raster_out <- raster - gdd_base
  raster_out[raster_out < 0] <- 0
  return(raster_out)
}