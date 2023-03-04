# 
# for(i in 4:11){
# #
# #
# # gs_df_in <- df %>%
# #   dplyr::filter(year==2013   & between(month,4,11))
# # gs_df <- gs_df_in %>%
# #  # select(stationid,temp_min,temp_max,date_time)%>%
# #   mutate(temp_mean=(temp_min+temp_max)/2)%>%
# #  # filter(stationid!='6456')%>%
# #  # filter(stationid!='47187')%>%
# #   dplyr::filter(stationid!='S100') %>% # 2012
# #  #   filter(stationid!='S120')%>%  # 2012, cold anyways
# #   dplyr::filter(stationid!='S160') %>% # 2012, early in season/cold
# #   dplyr::filter(stationid!='S20')%>% #2012
# #   dplyr::filter(stationid != 'S60') %>% #2012
# #   dplyr::filter(stationid != 'S80') %>% #2012
# #   dplyr::filter(stationid != 'CL1') %>%
# #  #  filter(stationid != 'SH6') %>% #2013
# #   mutate(gdd10_daily=ifelse(temp_mean>10,temp_mean-10,0))%>%
# #   group_by(stationid)%>%
# #   mutate(gdd10=sum(gdd10_daily,na.rm=TRUE))
# # # %>%
# # #   dplyr::filter(date_time==max(date_time))
# 
# 
# 
# 
#   # mutate(gdd10 = sum(ifelse(temp_mean-10 >= 0,
#   #                           temp_mean-10,
#   #                            0),na.rm = TRUE)) %>%
#   # mutate(gdd5 = sum(ifelse(temp_mean-5 > 0,
#   #                           temp_mean-5,
#   #                           0),na.rm = TRUE)) %>%
#   # mutate(gdd0 = sum(ifelse(temp_mean > 0,
#   #                          temp_mean,
#   #                          0),na.rm = TRUE)) %>%
#   #mutate(monthly_mean = mean(temp_mean))
# 
# ggplot(data= gs_df %>%
#          dplyr::filter(between(month(date_time),5,5))%>%
#          dplyr::filter(stationid=='WE2'
#                              # |stationid=='S120'
#                               ),
#        mapping=aes(x=date_time,y=temp_mean))+
#   geom_point(aes(col=stationid))
# 
# ggplot(data=df_13
#        ,aes(x=EASTING,y=NORTHING))+
#   geom_point(aes(col=gdd10)) +
#   scale_color_gradientn(colors=rev(rainbow(10)[1:8])) +
#   scale_size_continuous(range = c(0.1,10),breaks=c(-50,0,50,100)) +
#   coord_fixed()
# 
# 
# 
# m <- gam(gdd10~
#           # s(dem700m,k=5)    #k=5
#          s(dem700m,cp700m_2,k=2)           #k=2
#          +  s(east700m,north700m,k=10) #k=10
#          ,data=gs_df
#         ,method='REML'
#          #,select=TRUE
#          )
# plot.gam(m, residuals = TRUE,
#      pch = 1, cex = 1, shift = coef(m)[1])
# 
# gam_raster <- raster::predict(rasters_brick,m)
# plot(gam_raster, col=rev(rainbow(15)[1:12]))
# writeRaster(gam_raster,paste0('f:\\output\\gdd10\\test_gs12_test2.tif'),
#             overwrite=TRUE)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# cp700m_10km <- cp700m_2
# names(cp700m_10km) <- "cp700m_10km"
# cp700m_10km[cp700m_10km>10000]<-10000
# plot(cp700m_10km)
# 
# 
# 
# 
# 
# df_out <- df_in %>%
#   #1. select only relevant fields
#   select(stationid,temp_min,temp_max,date_time)%>%
#   #2. calculate temp_mean
#   mutate(temp_mean=(temp_min+temp_max)/2)%>%
#   #3. if temp_mean is above 10, minus 10, if not set to 0
#   mutate(gdd10_daily=ifelse(temp_mean>10,temp_mean-10,0))%>% #gdd10_daily
#   #4.  group by stations
#   group_by(stationid)%>%
#   #5. sum daily daily gdd
#   mutate(gdd10=sum(gdd10_daily))
