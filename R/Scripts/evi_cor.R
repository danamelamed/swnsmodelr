# Plot EVI and GDD5
library(swnsmodelr)
# 1. Make a temporal raster data frame for both
evi_raster_df <- make_temporal_raster_df(file.path("F:","EVI_200m"),
                                         start_date = ymd('2012-04-01'),
                                         end_date = ymd('2017-11-29'),
                                         date_chars = c(5,-4),
                                         date_format = "%Y_%j")
gdd_raster_df <- make_temporal_raster_df(file.path("Z:","Dana","Daily","Daily_GDD5"),
                                         start_date = ymd('2012-04-01'),
                                         end_date = ymd('2017-11-29'),
                                         date_chars = c(6,-4),
                                         date_format = "%Y-%m-%d")
temp_raster_df <- make_temporal_raster_df(file.path("Z:","Dana","Daily","Daily_Temp_Mean"),
                                         start_date = ymd('2012-04-01'),
                                         end_date = ymd('2017-11-29'),
                                         date_chars = c(6,-4),
                                         date_format = "%Y-%m-%d")

# plot rasters side by side
years = 2012:2017
for(j in seq_along(years)){
    # only 2015
    evi_df <- filter(evi_raster_df, year(date_time) == as.character(years[[j]]) &
                       month(date_time) >= 4 & month(date_time) <= 11) 
    gdd_df <- filter(gdd_raster_df, year(date_time) == as.character(years[[j]]) &
                       date_time %in% evi_df$date_time)
    
    
    
    pdf(paste0("F://gdd_evi_compare//gdd_evi_",years[[j]],"a.pdf"), width = 10, height = 15)
    par(mfrow = c(4, 2))
    for(i in seq_along(evi_df[[1]])){
        date_now <- evi_df[[2]][[i]]
        evi_now  <- filter(evi_df, date_time == date_now)
        gdd_now  <- filter(gdd_df, date_time == date_now)
        plot(raster(gdd_now[[1]][[1]]), 
            col=rev( rainbow(1000, start=0,end=0.75)),
             main = paste("GDD5: ",date_now))
        plot(raster(evi_now[[1]][[1]]), 
             col=rev( rainbow(100, start=0,end=0.75)),
             zlim= c(500,7000),
             main = paste("EVI: ",date_now))
        
        print(date_now)
    }
    dev.off()
    
    pdf(paste0("F://gdd_evi_compare//gdd_evi_",years[[j]],"b.pdf"), width = 10, height = 15)
    par(mfrow = c(4, 2))
    for(i in (seq_along(evi_df[[1]]))){
        date_now <- evi_df[[2]][[i]]
        evi_now  <- filter(evi_df, date_time == date_now)
        gdd_now  <- filter(gdd_df, date_time == date_now)
        plot(raster(gdd_now[[1]][[1]]), 
             col=rev( rainbow(1000, start=0,end=0.75)),
             zlim= c(0,2150),
             main = paste("GDD5: ",date_now))
        plot(raster(evi_now[[1]][[1]]), 
             col=rev( rainbow(100, start=0,end=0.75)),
             zlim= c(500,7000),
             main = paste("EVI: ",date_now))
        
        print(date_now)
        }
    dev.off()
}

# Extract EVI values to stations

test_evi <- extract_temporal_raster_values(evi_df_2015,
                                                  swns_stations_df_200,
                                                  col_name = "evi")

for(i in (seq_along(evi_df_2015[[1]]))){
  date_now <- evi_df_2015[[2]][[i*2]]
  evi_now  <- filter(evi_df_2015, date_time == date_now)
  gdd_now  <- filter(gdd_df_2015, date_time == date_now)
  cor_evi <- corLocal(raster(evi_now[[1]][[1]]), raster(gdd_now[[1]][[1]]), method = "spearman")
}

test_evi <- filter(test_evi, !is.na(evi))



