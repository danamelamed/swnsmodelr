#' Add Growing Degree Day columns to a dataframe
#' 
#' @param temperatures_df Dataframe containing temperature data
#' @param gdd_bases_list List of temperature base values to calculate growing degree days (GDD)
#' @return  Dataframe with added GDD columns
#' @export
add_gdd_columns <- function(temperatures_df,
                                           gdd_bases_list){
  for(base in gdd_bases_list){
   temperatures_df <- temperatures_df %>%
     
      
      group_by(stationid,year(date_time)) %>%
      mutate(daily = ifelse(round(temp_mean - base)<0,
                            0,
                            round(temp_mean - base))) %>%
      mutate(gs = sum(daily,na.rm=TRUE)) %>%
      mutate(acc = cumsum(daily)) %>%
      ungroup() %>%
      group_by(stationid,year(date_time),month(date_time))%>%
      mutate(monthly = sum(daily,na.rm=TRUE)) %>%
      ungroup()
      
      new_daily = paste0('gdd',base,'_daily')
      new_gs = paste0('gdd',base)
      new_acc = paste0('gdd',base,'_acc')
      new_monthly = paste0('gdd',base,'_monthly')
      
      temperatures_df <- temperatures_df %>%
        rename(!!new_daily := daily,
               !!new_gs := gs,
               !!new_acc := acc,
               !!new_monthly := monthly) 
      
   
  }
  return(temperatures_df)
}