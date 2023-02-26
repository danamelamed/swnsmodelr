create_rasters_list  <- function(in_folder,
                                raster_names,
                                resolution,
                                ext = '.tif'){
  
  rasters_dir <- in_folder
  
  
  all_rasters <-  file.path(rasters_dir,paste0(as.list(outer(raster_names,resolution,paste0)),ext))
  
  rasters_list <- list()
  c <- 1
  for(raster in all_rasters){
    try({rasters_list[[c]] <- raster(raster)
    c <- c+1
    }) 
  }  
  return(rasters_list)
  
  
  
  
  
  
}