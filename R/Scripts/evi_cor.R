# EVI and GDD Investigation
library(swnsmodelr)
# Make a temporal raster data frames
evi_raster_df <- make_temporal_raster_df(file.path("E:","EVI_200m"),
                                         start_date = ymd('2012-04-01'),
                                         end_date = ymd('2012-11-30'),
                                         date_chars = c(5,-4),
                                         date_format = "%Y_%j")
evi_raster_df <- evi_raster_df %>% filter(month(date_time) >= 4 &
                                            month(date_time) <= 11)
temp_raster_df <- make_temporal_raster_df(file.path("E:","Daily","Daily_Temp_Mean"),
                                         start_date = ymd('2012-03-01'),
                                         end_date = ymd('2012-11-30'),
                                         date_chars = c(10,19),
                                         date_format = "%Y-%m-%d")


# Create GDD accumulation rasters over 16 days
stations_df <- as.data.frame(swns_stations_sp)
gdd_acc <- list() # list to store accumulate gdd rasters
par(mfrow = c(4,2))
for(i in seq_along(evi_raster_df[[1]])){
    
    # set current date
    date_now <- evi_raster_df[[2]][[i]]
    
    # accumulate gdd from 16 days before evi
    temp_16_days  <- filter(temp_raster_df, date_time <= date_now &
                             date_time >= (date_now - 16))
    temp_rasters_16 <- lapply(FUN = raster,
                              X   = temp_16_days[[1]])
    gdd0_rasters_16 <- lapply(FUN = apply_gdd_base,
                              X = temp_rasters_16,
                              gdd_base = 0) %>% brick()
    gdd_acc[[i]] <- sum(gdd0_rasters_16)
    #write out the 16 day accumulation raster
    writeRaster(gdd_acc[[i]],
                file.path("E:","Daily","GDD0_16_day",
                          paste0("gdd0_16",date_now,".tif")))
    # extract and and store gdd values
    stations_df$extract <- raster::extract(gdd_acc[[i]], 
                                           swns_stations_sp)
    names(stations_df)[names(stations_df) == "extract"] <- as.character(date_now)
    
    print(date_now)
}

gdd_16_raster_df <- make_temporal_raster_df(file.path("E:","Daily","GDD0_16_day"),
                                            start_date = ymd('2012-04-01'),
                                            end_date = ymd('2017-11-30'),
                                            date_chars = c(9,-4),
                                            date_format = "%Y-%m-%d")

# make output maps
pdf(paste0("E://gdd_evi_compare//gdd0_16_5.pdf"), width = 10, height = 15)
par(mfrow = c(4,2))
for(i in seq_along(evi_raster_df[[1]])){
  # set current date
  date_now <- evi_raster_df[[2]][[i]]
  # get evi raster with current date 
  evi_now  <- filter(evi_raster_df, date_time == date_now)
  evi_raster_now <- raster(evi_now[[1]][[1]])
  
  gdd_now <- filter(gdd_16_raster_df, date_time == date_now)
  gdd_raster <- raster(gdd_now[[1]][[1]])
  
  # plot gdd and evi
  # gdd
  gdd_plot <- gdd_raster
  gdd_plot[gdd_plot <= 25] <- 25
  gdd_plot[gdd_plot >= 350] <- 350
  
  plot(gdd_plot,
       col = rev(rainbow(100, start = 0,end = 0.75)),
       zlim = c(25, 350),
       main = paste("GDD0: ",(date_now - 16)," - ",date_now)
  )
  
  # evi
  evi_plot <- (evi_raster_now + 2000) / 12000 # calibrate
  evi_plot[evi_plot <= 0.3] <- 0.3
  evi_plot[evi_plot >= 0.8] <- 0.75
  
  plot(evi_plot,
       col = rev(rainbow(100, start = 0,end = 0.75)),
       zlim = c(0.3, 0.8),
       main = paste("EVI: ",(date_now - 16)," - ",date_now)
  )
  
  print(date_now)
}
dev.off()


# Make grpah outputs 

#Extract EVI values to stations
evi_df <- extract_temporal_raster_values(evi_raster_df,
                                         swns_stations_df_200,
                                         col_name = "evi") %>%
          select(stationid, date_time, evi) %>% 
          filter(date_time %in% evi_raster_df$date_time) %>% # only keep original raster dates
          mutate(evi_dfevi_calib = (evi + 2000)/12000) # calibrate between 0 - 1

# Get predicted accumulated GDD for validation stations
gdd_acc_val <- extract_temporal_raster_values(gdd_16_raster_df,
                                              swns_stations_df,
                                              col_name = "gdd0_16") %>%
               select(stationid, date_time, gdd0_16) %>%
               filter(date_time %in% evi_raster_df$date_time)





# plots
stations_list <- gdd_evi_df %>%
                 filter(!duplicated(stationid)) %>%
                 select(stationid)
pdf("E://testplot.pdf")
for(i in seq_along(stations_list[[1]])){
    gdd_evi_df_now <- gdd_evi_df %>%
                      filter(stationid == stations_list[[1]][[i]])
    print(ggplot(data = gdd_evi_df_now) +
            geom_point(aes(x = yday, y= gdd_acc, colour = "blue")) +
            geom_smooth(aes(x = yday, y=gdd_acc, colour = "blue"), se = FALSE)+
            geom_point(aes(x = yday, y= evi_calib, colour = "green")) +
            geom_smooth(aes(x = yday, y=evi_calib, colour = "green"), se = FALSE)+
            facet_wrap(~ year, ncol = 2) +
            scale_colour_discrete(labels = c("GDD0","EVI")) +
            guides(colour = guide_legend(title = "Variable")) +
            labs(x = "Day of the Year", y = "Value", title = stations_list[[1]][[i]]))
  print(i)
  }
dev.off()

# zone 1 
# BI1 (tip of digby neck), AN2 (port royal)
# zone 2
# DNR025 south-west of annapolis basin
