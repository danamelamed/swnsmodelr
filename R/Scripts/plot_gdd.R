# Plot GDD maps for report

# Plot monthly accumulated report

# GDD0 at monthly intervals
gdd0_raster_df <- make_temporal_raster_df(file.path("E:","Daily","Daily_GDD0"),
                                         start_date = ymd('2012-04-01'),
                                         end_date   = ymd('2017-11-30'),
                                         date_chars = c(6,-4),
                                         date_format = "%Y-%m-%d") %>% arrange(date_time)
gdd5_raster_df <- make_temporal_raster_df(file.path("E:","Daily","Daily_GDD5"),
                                          start_date = ymd('2012-04-01'),
                                          end_date   = ymd('2017-11-30'),
                                          date_chars = c(6,-4),
                                          date_format = "%Y-%m-%d") %>% arrange(date_time)
gdd10_raster_df <- make_temporal_raster_df(file.path("E:","Daily","Daily_GDD10"),
                                          start_date = ymd('2012-04-01'),
                                          end_date   = ymd('2017-11-30'),
                                          date_chars = c(7,-4),
                                          date_format = "%Y-%m-%d") %>% arrange(date_time)

library(RColorBrewer)

# Classic palette BuPu, with 4 colors
coul = brewer.pal(4, "Spectral") 

# I can add more tones to this palette :
coul = colorRampPalette(coul)(100)

# Plot it
pie(rep(1, length(coul)), col = coul , main="") 

pdf("E://test_gdd0.pdf",  width = 10, height = 15)
par(mfrow = c(2,2))
for(i in seq_along(gdd0_raster_df[[1]])){
  date_now <- gdd0_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd0_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,3300),
         col = rev(coul),
         main = paste0("GDD0: ",date_now))
  }
  
}
dev.off()

pdf("E://test_gdd5.pdf",  width = 10, height = 10)
par(mfrow = c(2,2), xpd = NA)
for(i in seq_along(gdd5_raster_df[[1]])){
  date_now <- gdd0_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd5_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,2200),
         col = rev(coul),
         main =  paste0("GDD5: ",date_now))
  }
  
}
dev.off()



pdf("E://test_gdd10.pdf",  width = 10, height = 10)
par(mfrow = c(2,2), xpd = NA)
for(i in seq_along(gdd10_raster_df[[1]])){
  date_now <- gdd10_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd10_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,1300),
         col = rev(coul),
         main =  paste0("GDD10: ",date_now))
  }
  
}
dev.off()


# make plots for each year to compare
# the november accumulated gdd for each year in full colour
pdf("E://test_gdd0_year.pdf",  width = 10, height = 15)
par(mfrow = c(2,2))
for(i in seq_along(gdd0_raster_df[[1]])){
  date_now <- gdd0_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd0_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,3300),
         col = rev(coul),
         main = paste0("GDD0: ",date_now))
  }
  
}
dev.off()

pdf("E://test_gdd5_year.pdf",  width = 10, height = 10)
par(mfrow = c(2,2), xpd = NA)
for(i in seq_along(gdd5_raster_df[[1]])){
  date_now <- gdd0_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd5_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,2200),
         col = rev(coul),
         main =  paste0("GDD5: ",date_now))
  }
  
}
dev.off()



pdf("E://test_gdd10.pdf",  width = 10, height = 10)
par(mfrow = c(2,2), xpd = NA)
for(i in seq_along(gdd10_raster_df[[1]])){
  date_now <- gdd10_raster_df$date_time[[i]]
  month_now <- month(date_now)
  
  if(day(date_now) == days_in_month(month_now)){
    raster_now <- gdd10_raster_df$path_field[[i]] %>% raster
    plot(raster_now, zlim = c(0,1300),
         col = rev(coul),
         main =  paste0("GDD10: ",date_now))
  }
  
}
dev.off()