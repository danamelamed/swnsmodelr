#' Adds extra date columns to a dataframe 
#' @param date_time_df a dataframe with a date column called "date_time"
#' @export

add_date_columns <- function(date_time_df){
  
  date_time_df$yday  <- lubridate::week(date_time_df$date_time)
  date_time_df$week  <- lubridate::week(date_time_df$date_time)
  date_time_df$month <- lubridate::week(date_time_df$date_time)
  date_time_df$year  <- lubridate::week(date_time_df$date_time)
  
  return(date_time_df)
}