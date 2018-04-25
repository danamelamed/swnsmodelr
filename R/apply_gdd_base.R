#' Applies the base GDD level to an average daily raster
#' @param raster A daily average raster
#' @param gdd_base The set base temperature for setting GDD
#' @return The raster with the gdd_base subtracted, and all negatives set to 0

apply_gdd_base <- function(raster, gdd_base){
  raster_out <- raster - gdd_base
  raster_out[raster_out < 0] <- 0
  return(raster_out)
}