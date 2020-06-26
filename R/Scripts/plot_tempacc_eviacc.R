# plot acc. evi with acc. T



evi_raster_df <- make_temporal_raster_df(file.path("E:","EVI_200m"),
                                         start_date = ymd('2012-04-01'),
                                         end_date = ymd('2016-11-30'),
                                         date_chars = c(5,-4),
                                         date_format = "%Y_%j")

evi_df <- extract_temporal_raster_values(evi_raster_df,swns_stations_df_200, col_name = "evi")

evi_df <- evi_df %>% filter(!is.na(evi))
evi_df <- evi_df %>% filter(!is.na(temp_mean))
evi_df <- evi_df %>% mutate(evi_div = evi/1000)
evi_df <- evi_df %>% mutate(evi_div_acc = cumsum(evi_div))
evi_df <- evi_df %>% mutate(temp_mean_acc = cumsum(temp_mean)) %>%
  mutate(temp_min_acc = cumsum(temp_min)) %>%
  mutate(temp_max_acc = cumsum(temp_max))


par(mfrow = c(3,2))
for(y in unique(evi_df$year)){
  evi_year <- evi_df %>% filter(year== y)
  for(i in unique(evi_df$stationid)){
    par(mar = c(5,5,2,5))
    evi_now <- evi_year %>% filter(stationid == i,
                                 !is.na(temp_mean),
                                 !is.na(evi))
    if(length(evi_now[[1]]) >5){
      evi_now <- evi_now %>% mutate(temp_mean_acc = cumsum(temp_mean),
                                    temp_min_acc  = cumsum(temp_min) ,
                                    temp_max_acc  = cumsum(temp_max) ,
                                    evi_acc       = cumsum(evi))
      with(evi_now, plot(yday, temp_mean_acc, col = "red", pch = 20,
                        ylab = "Temperature[Mean, Accumulated]",
                        xlab = "Day of the Year",
                        main = paste(i,y, "cor=", round(cor(temp_mean_acc,evi_acc),3),sep = " ")))
      par(new=T)
      with(evi_now, plot(yday, evi_acc, col = "dark green", pch = 20,
                         axes = F,
                         ylab = NA,
                         xlab = NA))
      axis(side =4)
      mtext(side = 4, line =3, expression(EVI/1000))
      
      legend("topleft",
             legend=c(expression(Temperature[MeanAccumulated]), expression(EVI/1000)),pch = 20, col = c("red","dark green"))
    }
  }
}
  