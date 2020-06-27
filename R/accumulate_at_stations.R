#' Accumulates the values at stations over time
accumulate_at_stations <- function(temperatures_df,
                                   acc_col,
                                   stations_col = "stationid",
                                   dates_col = "date_time",
                                   out_col = 'out_col'){
  
  stations <- unique(temperatures_df[,names(temperatures_df) == stations_col])

  
  
  # discard rows where acc_col = NA
  df <- temperatures_df %>% dplyr::filter(!is.na(acc_col))
  
  # create output data frame
  output_df <- df %>% dplyr::filter(is.na(acc_col))
  out_col <- paste(acc_col,'acc',sep='_')
  # break down into stations, accumulate, put back together
  for(station in stations){
    station_df <- df %>% dplyr::filter(!!as.symbol(stations_col) == station) %>% 
      dplyr::arrange(!!as.symbol(stations_col) ) %>%   
      dplyr::mutate(out_col = cumsum(!!as.symbol(acc_col)))
    output_df <- dplyr::bind_rows(output_df, station_df)
    
  }
  names(output_df)[names(output_df) == 'out_col'] <- out_col
  return(output_df)
  
}