#' Generate average GDD from a set of daily mean temperature rasters
#' @export
#' 
#' 
#' 
#
generate_average_gdd <- function (start_date ,
end_date ,
gdd_rasters_df ){
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