# resample solar irradiance rasters
library(raster)
# to 20m...
output_folder <- 'D:\\GOES\\700m'
input_folder <- 'D:\\GOES\\700m_original'


raster700m <- raster('D:\\data\\rasters\\input_rasters\\dem700m.tif')



input_files <- list.files(input_folder)



for(tif in input_files){
  if(grepl('.tif',tif)){
    print(tif)
    if(tif %in% list.files(output_folder)){
      print('exists')
    }else{
     try(new_raster <- projectRaster(from = raster(file.path(input_folder,tif)),
                                 to   = raster700m,
                                 method = 'bilinear',
                                 filename = file.path(output_folder,tif),
                                 overwrite = FALSE))}
  }
}


