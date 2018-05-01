#' Adds extra date columns to a dataframe 
#' @param date_time_df a dataframe with a date column called "date_time"
#' @return The input dataframe with new columns for the yday, week, month 
#' and year
#' @export

add_date_columns <- function(date_time_df){
  
  date_time_df$yday  <- lubridate::yday(date_time_df$date_time)
  date_time_df$week  <- lubridate::week(date_time_df$date_time)
  date_time_df$month <- lubridate::month(date_time_df$date_time)
  date_time_df$year  <- lubridate::year(date_time_df$date_time)
  
  return(date_time_df)
}