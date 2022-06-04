

# get all rasters as list of lists
variables <- list('dem','slp','asp','ptoc','tpi300m','tpi2000m')

raster_lists <- list()
for(j in 1:length(variables)){
  
  if(variables[j]=='tpi300m'){
    resolutions <- list('20','100','200')
  }else{
    resolutions <- list('20','100','200','700','1000')
  }
  inner_list <- list()
  for(i in 1:length(resolutions)){
    inner_list[[i]] <-raster(paste0('d:\\data\\rasters\\',resolutions[i],'m\\',variables[j],resolutions[i],'m.tif'))
  }
  raster_lists[[j]] <- inner_list
}



# create histograms for every raster... keep the bins the same as the 20m resolution raster
for(j in 1:length(raster_lists)){
  
  for(i in 1:length(raster_lists[[j]])){
    if(i == 1){
      # get breaks
      h <- hist(getValues(raster_lists[[j]][[i]]), plot = FALSE, main = variables[j])}
    
    h <- try(hist(getValues(raster_lists[[j]][[i]]), breaks = h$breaks
                  , main = paste(variables[j],resolutions[i])))  
    
    if("try-error" %in% class(h)){
      h <- hist(getValues(raster_lists[[j]][[i]], main = variables[j]))}
  }
}
par(mfrow=c(2,3))

