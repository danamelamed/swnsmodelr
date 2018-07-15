## Characterize GDD Zones by input parameters

# Make each zone into polygon that can crop input parameters.. then make histograms?
rasters_brick <- brick(rasters_list)
env_rasters <- dropLayer(rasters_brick, c(3,4))

zone_polys <- list()
# make a list of polygons, for each zone
for(i in 1:5){
    zone <- gdd_zones
    zone[zone != i] <- NA
    zone_polys[[i]] <- rasterToPolygons(zone, dissolve = TRUE)
    zone_polys[[i]]@proj4string <- gdd_zones@crs
    print(i)
}



pdf(file = file.path("E:","test_zone_character3.pdf"))
for(j in seq_along(names(env_rasters))){
  
  raster_now <-env_rasters[[j]]
  raster_name <- names(raster_now)
    par(mfrow = c(3,2))
    for(i in 1:5){
      
      # plot(gdd_zones,
      #      col = rev(rainbow(n = 5,start = 0,end = 0.75)),
      #      main = "GDD Zones")
      plot(raster_now, 
           main = paste("Zone ",i,": ",raster_name))
      plot(zone_polys[[i]], add = TRUE, lwd = 0.1)
      values <- raster::extract(raster_now,
                                zone_polys[[i]]) %>%
        as.data.frame()
      names(values) <- "values"
      
      
      hist(values$values, freq = FALSE,
           breaks = 15,
           main = NA,
           xlab = paste(raster_name, " values"),
           xlim = c(minValue(raster_now), maxValue(raster_now))
           )
      
    }
}
dev.off()


par(mfrow = c(1,2))
plot(gdd_zones,
     col = rev(rainbow(n = 5,start = 0,end = 0.75)))
plot(eco_regions_2, add = TRUE)
plot(gdd_zones)
lapply(FUN = plot, X = zone_polys, add = TRUE)



# read eco regions, and get gdd 6ya inside each
eco_regions <- rgdal::readOGR(dsn = file.path("E:","ns_eco_regions"), layer = "ns_eco")
eco_regions_2 <- rgeos::gUnaryUnion(eco_regions, id = eco_regions@data$ECODISTRIC) %>%
                 as("SpatialPolygonsDataFrame")
extract_er <- extract(gdd_6ya, eco_regions_2)

# plot them as histograms of GDD
par(mfrow = c(3, 2))
for(i in seq_along(extract_er)){
 # plot(gdd_6ya)
  
  print(row.names(eco_regions_2@data)[[i]])
  eco_regions_sp$dummy[[i]] <- row.names(eco_regions_2@data)[[i]]
}


watersheds <- rgdal::readOGR(dsn = file.path("E:","ns"), layer = "ns")
plot(watersheds)
