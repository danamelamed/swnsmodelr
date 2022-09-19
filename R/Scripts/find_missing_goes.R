#what goes rasters are missing
files <- list.files('D:\\GOES\\700m')


goes_rasters_df <- make_temporal_raster_df('D:\\GOES\\700m',
                                           as.Date('2012-01-01'),
                                           as.Date('2015-12-31'),
                                           c(16,-5),
                                           '%Y_%j') %>% arrange(date_time)
goes_rasters_dates <- lapply(X = goes_rasters_df[,2], FUN = as.numeric)%>% unlist() 
  
for(missing_date in 15340:16800){
  if(!missing_date %in% goes_rasters_dates){
    print(as.Date(missing_date,origin=ymd('1970-01-01')))
  } 
}



