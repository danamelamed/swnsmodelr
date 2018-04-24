#' Writes the gdd output in the function generate_gdd_output
#' 
#' 
writeGDDout <- function(gdd_out,
                        output_time_slice,
                        gdd_base,
                        date_now,
                        plot_gdd_raster,
                        output_folder,
                        output_format){
  print(paste("Generating",output_time_slice,"output", sep = " "))
  raster::writeRaster(x = gdd_out, 
                      filename = paste0(output_folder,'\\gdd',gdd_base,'_' ,date_now),
                      format   = output_format,
                      overwrite = TRUE)
  print(paste0(output_folder,'\\gdd',gdd_base,'_' ,date_now))
  if(plot_gdd_raster == TRUE){
    print("Plotting GDD raster")
    plot(gdd_out, xlab = date_now)
  }
}
