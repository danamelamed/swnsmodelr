# reclassify six year average
gdd_6ya <- raster(file.path("E:","Daily","gdd5_6ya.tif"))

values <- gdd_6ya[gdd_6ya]

hist(values, breaks = 20, freq = TRUE)

classInt::classIntervals(values, n = 6, 
                         style = "jenks")

gdd_zones <- gdd_6ya
# natural breaks from arcgis
gdd_zones[gdd_zones <= 1770] <- 1
gdd_zones[gdd_zones > 1770 & gdd_zones <= 1870] <- 2
gdd_zones[gdd_zones > 1870 & gdd_zones <= 1930] <- 3
gdd_zones[gdd_zones > 1930 & gdd_zones <= 1990] <- 4
gdd_zones[gdd_zones > 1990] <- 5

writeRaster(gdd_zones,
            file.path("E:","Daily","gdd_zones.tif"))


plot(gdd_zones,
     col = rev(rainbow(n = 5,start = 0,end = 0.75)))




gdd_zones

