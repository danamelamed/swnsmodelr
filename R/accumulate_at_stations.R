#' Accumulates the values at stations over time
accumulate_at_stations <- function(temperatures_df,
                                   acc_col,
                                   stations_col = "stationid",
                                   dates_col = "date_time",
                                   out_col = 'out_col'){
  

  #rename columns for now
  names(temperatures_df)[names(temperatures_df) == dates_col] <- 'date_time'
  names(temperatures_df)[names(temperatures_df) == stations_col] <- 'stationid'
  names(temperatures_df)[names(temperatures_df) == acc_col] <- 'acc_col'
  temperatures_df <- temperatures_df %>% add_date_columns()
  
  # set to 0 rows where acc_col = NA
  df <- temperatures_df %>% 
    mutate(acc_col = if_else(is.na(acc_col),0,acc_col))%>%
    filter(acc_col > 0)
  
  # create output data frame
  
  output_df <- df %>% filter(date_time == "1-1-1")
 
  # break down into years
  for(y in unique(df$year)){
    year_df_now <- df %>% dplyr::filter(df$year == y)
    year_df_now_empty <- year_df_now %>% filter(date_time == "1-1-1")
    #output_df <- df  %>% dplyr::filter(is.na(acc_col))
    # break down into stations, accumulate, put back together
    for(station in unique(year_df_now$stationid)){
      station_df <- year_df_now  %>% dplyr::filter(stationid == station) %>% 
        dplyr::arrange(date_time) %>%   
        dplyr::mutate(out_col = cumsum(acc_col))
      year_df_now_empty <- dplyr::bind_rows(year_df_now_empty, station_df)
     
    }
    output_df <- dplyr::bind_rows(output_df,year_df_now_empty) 
  }
  
  
  #reset column names
  names(output_df)[names(output_df) == 'out_col'] <- out_col
  names(output_df)[names(output_df) == 'date_time'] <- dates_col
  names(output_df)[names(output_df) == 'stationid'] <- stations_col
  names(output_df)[names(output_df) == 'acc_col' ] <- acc_col
  
  return(output_df)
  
}
