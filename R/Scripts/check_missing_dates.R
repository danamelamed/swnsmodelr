# check dates missing at stations
start <- ymd('2012-04-30')
end <- ymd('2014-11-30')
date_range <- seq(start, end, by = 1) 

validation_stations_list <- validation_stations_df$stationid %>% unique()
for(station in validation_stations_list){
print(station)
  df_test <- stations_df %>% filter(stationid==station)
  d <- df_test$date_time

print(length(date_range[!date_range %in% d] ))
}
