#' Generate average GDD from a set of daily mean temperature rasters
#' @export
#' 
#' 
#' 
#
start_date <- ymd("2012-01-01")
end_date <- ymd("2012-12-31")
gdd_rasters_df <- make_temporal_raster_df(file.path("E:","Daily","Daily_GDD0"),
                                                       start_date,
                                                       end_date,
                                                       date_chars = c(6,-4),
                                                       date_format = "%Y-%m-%d")
# growing_season = TRUE
# generate_average_gdd <- function(gdd_rasters_df,
#                                  start_date,
#                                  end_date,
#                                  growing_season = TRUE){
  
  # Reduce to growing season if growing_season = TRUE
  if(growing_season == TRUE){
    gdd_rasters_df <- gdd_rasters_df %>% 
                                     filter(month(date_time) >= 4 &
                                              month(date_time) <= 11)
  }
  
  # Reduce to monthly accumulations
extra_days <- list()
j = 1
  for(i in seq_along(gdd_rasters_df[[1]])){
    date_now <- gdd_rasters_df[[2]][[i]]
   
    if(day(date_now) != days_in_month(date_now)[[1]])
      extra_days[j] <- i
    j = j + 1
  }
extra_days <- extra_days %>% unlist()
gdd_rasters_df <- gdd_rasters_df[-c(extra_days), ]
  

}
