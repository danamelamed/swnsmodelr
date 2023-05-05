#' Add Growing Degree Day columns to a dataframe
#' 

#' @param temperatures_df
#' @param gdd_bases_list
#' @return output_df
#' #' @export
add_gdd_columns <- function(temperatures_df,
                                           gdd_bases_list){
  for(base in gdd_bases_list){
   temperatures_df <- temperatures_df %>%
     
      
      group_by(stationid,year(date_time)) %>%
      mutate(daily = round(temp_mean - base)) %>%
      mutate(daily = ifelse(is.na(daily),0,daily)) %>%
      mutate(gs = sum(gdd10_daily,0,na.rm=TRUE)) %>%
      mutate(acc = cumsum(daily)) %>%
      ungroup() %>%
      group_by(stationid,year(date_time),month(date_time))%>%
      mutate(monthly = sum(daily,na.rm=TRUE))
      
      new_daily = paste0('gdd',base,'_daily')
      new_gs = paste0('gdd',base)
      new_acc = paste0('gdd',base,'_acc')
      new_monthly = paste0('gdd')
      
      temperatures_df <- temperatures_df %>%
        rename(!!new_daily := daily,
               !!new_gs := gs,
               !!new_acc := acc,
               !!new_monthly := monthly) 
      
   
  }
  
}