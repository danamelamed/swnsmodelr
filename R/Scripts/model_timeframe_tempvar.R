# Testing models with different daily temperature variables and timeframes

##### All years timeframe #####
temp_var <- list("min", "max", "mean")
all_years <- list()
all_years_val_list <- list()
for(i in seq_along(temp_var)){
  all_years[[i]] <- gam(formula(paste0("temp_",temp_var," ~
                                       s(dem,month) +
                                       s(ptoc,month, k= 3) +
                                       s(sum_irradiance, month) +
                                       s(tpi,month) +
                                       s(asp, month) +
                                       s(east,north) +
                                       s(week) +
                                       year")), 
                        data = model_stations_df)
  all_years_val_list[[i]] <- add_residuals(val_df_2012, all_years[[i]])

  
  if(is.na(all_years_val_list[[i]]$resid)){
    all_years[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                                         s(dem,month) +
                                         s(ptoc,month, k= 3) +
                                         s(tpi,month)+
                                         s(asp, month) +
                                         s(east,north) +
                                         s(week) +
                                         year")), 
                          data = model_stations_df)
    all_years_val_list[[i]] <- add_residuals(val_df_2012, all_years[[i]])
  }
  all_years_val_list[[i]]$timeframe <- "All years"
  all_years_val_list[[i]]$knots <- "No limit"
  all_years_val_list[[i]]$temp_var <- temp_var[[i]]
  all_years_val_list[[i]]$gcv <- all_years[[i]]$gcv.ubre
  all_years_val_list[[i]]$rsq <- summary(all_years[[i]])[[10]]
  all_years_val_list[[i]]$dev <- summary(all_years[[i]])[[14]]
  all_years_val_list[[i]]$abs_resid <- abs(all_years_val_list[[i]]$resid)
  for(l in seq_along(summary(all_years[[i]])[[7]])){
    all_years_val_list[[i]]$var_pval <- summary(all_years[[i]])[[8]][[l]]
    names(all_years_val_list[[i]])[names(all_years_val_list[[i]]) == "var_pval"] <- names(summary(all_years[[i]])[[7]])[[l]]
  }
}
all_years_val <- bind_rows(all_years_val_list)

##### Annual timeframe ####
annual <- list()
annual_val_list <- list()
for(i in seq_along(temp_var)){      
  annual[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                                      s(dem,week) +
                                      s(ptoc,week, k= 3)+
                                      s(sum_irradiance, week) +
                                      s(tpi,week)+
                                      s(asp, week) +
                                      s(east,week) +
                                      s(yday) +
                                      month")), 
                       data = model_df_2012)
  annual_val_list[[i]] <- add_residuals(val_df_2012, annual[[i]])

  
  if(is.na(annual_val_list[[i]]$resid)){
    annual[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                                        s(dem,week) +
                                        s(ptoc,week, k= 3)+
                                        s(tpi,week)+
                                        s(asp, week) +
                                        s(east,week) +
                                        s(yday) +
                                        month")), 
                         data = model_df_2012)
    annual_val_list[[i]] <- add_residuals(val_df_2012, annual[[i]])
    }
    for(l in seq_along(summary(annual[[i]])[[7]])){
      annual_val_list[[i]]$var_pval <- summary(annual[[i]])[[8]][[l]]
      names(annual_val_list[[i]])[names(annual_val_list[[i]]) == "var_pval"] <- names(summary(annual[[i]])[[7]])[[l]]
    }
    annual_val_list[[i]]$timeframe <- "Annual"
    annual_val_list[[i]]$temp_var <- temp_var[[i]]
    annual_val_list[[i]]$gcv <- annual[[i]]$gcv.ubre
    annual_val_list[[i]]$rsq <- summary(annual[[i]])[[10]]
    annual_val_list[[i]]$knots <- "No limit"
    annual_val_list[[i]]$dev <- summary(annual[[i]])[[14]]
    annual_val_list[[i]]$abs_resid <- abs(annual_val_list[[i]]$resid)
    
}
annual_val <- bind_rows(annual_val_list)

  

##### Monthly Timeframe #####
# Create list of three dataframes
monthly_val_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  monthly_val_list[[i]] <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                            val_stations_df = val_stations_df,
                                            years = 2012,
                                            months = 1:12,
                                            formula =paste0("temp_",temp_var[[i]],"~
                                                           s(east,north) +
                                                           s(dem, yday)  +
                                                           s(sum_irradiance, yday) +
                                                           s(tpi, yday) + 
                                                           s(asp, yday) +
                                                           s(ptoc, k = 3) +
                                                           week"),
                                            alt_formula =paste0("temp_",temp_var[[i]],"~
                                                               s(east,north, yday) +
                                                               s(dem, yday)  +
                                                               s(tpi, yday) + 
                                                               s(asp, yday)
                                                               s(ptoc, yday, k = 3) +
                                                               week")
  ) 
  monthly_val_list[[i]]$temp_var <- temp_var[[i]]
}

monthly_val <- dplyr::bind_rows(monthly_val_list)
monthly_val$timeframe <- "Monthly"


##### Weekly Timeframe #####
# Create list of three dataframes
weekly_val_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  weekly_val_list[[i]] <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                           val_stations_df = val_stations_df,
                                           years = 2012,
                                           weeks = 1:52,
                                           formula =paste0("temp_",temp_var[[i]],"~
                                                         s(east,north) +
                                                         s(dem, yday)  +
                                                         s(sum_irradiance, yday) +
                                                         s(asp, yday) +
                                                         s(tpi, yday) + 
                                                         s(ptoc,yday)"),
                                           alt_formula =paste0("temp_",temp_var[[i]],"~
                                                         s(east,north) +
                                                         s(dem, yday)  +
                                                         s(asp, yday) +
                                                         s(tpi, yday) + 
                                                         s(ptoc,yday)")
  ) 
  weekly_val_list[[i]]$temp_var <- temp_var[[i]]
  
}

weekly_val <- dplyr::bind_rows(weekly_val_list)
weekly_val$timeframe <- "Weekly"


##### Daily Timeframe #####
# Create list of three dataframes
daily_val_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  daily_val_list[[i]] <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                          val_stations_df = val_stations_df,
                                          years = 2012,
                                          days = 1:365,
                                          formula =paste0("temp_",temp_var[[i]],"~
                                                          s(east,north) +
                                                          s(dem)  +
                                                          s(sum_irradiance) +
                                                          s(tpi) + 
                                                          s(ptoc, k = 3)"),
                                          alt_formula =paste0("temp_",temp_var[[i]],"~
                                                              s(east,north) +
                                                              s(dem)  +
                                                              s(tpi) + 
                                                              s(ptoc, k = 3)")
  ) 
  daily_val_list[[i]]$temp_var <- temp_var[[i]]
  print(temp_var[[i]])
  
}
daily_val <- dplyr::bind_rows(daily_val_list)
daily_val$timeframe <- "Daily"



##### Plots #####
colours <- list("light blue","light pink","beige")
temp_vars <- c("min","max","mean")



val_list <- list(all_years_val,
                      annual_val,
                      monthly_val,
                      weekly_val,
                      daily_val)

bind_val <- bind_rows(all_years_val,
                      annual_val,
                      monthly_val,
                      weekly_val,
                      daily_val)
bind_val <- bind_val %>%
  mutate(temp_var_value = 1) %>%
  mutate(temp_var_value = if_else(temp_var == "mean",temp_mean, temp_var_value),
         temp_var_value = if_else(temp_var == "min", temp_min, temp_var_value),
         temp_var_value = if_else(temp_var == "max", temp_max, temp_var_value)) %>%
  mutate(timeframe = factor(timeframe, levels = c("All years","Annual","Monthly","Weekly","Daily")))

pdf("E://testplots.pdf",height = 7.5)
# Error
ggplot(data = bind_val) +
  geom_histogram(aes(x = resid,  fill = timeframe),
              colour = "black",binwidth = 1) +
  facet_grid(timeframe~temp_var) +
  labs(x = "Error at Validation Stations", y = "Count") +
  coord_cartesian(xlim = c(-10,10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none")

# GCV Scores
ggplot(data = bind_val) +
  geom_point(aes(x = yday, y = gcv, colour = timeframe),
              se = FALSE) +
  facet_grid(timeframe~temp_var) +
  labs(x = "Day of the Year (2012)", y = "GCV Score") +
  coord_cartesian(ylim = c(0,25)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none")

# Adjusted R Squared
ggplot(data = bind_val) +
  geom_point(aes(x = yday, y = rsq, colour = timeframe)) +
  facet_grid(timeframe~temp_var) +
  labs(x = "Day of the Year (2012)", y = "Adj. R^2") +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  coord_cartesian(ylim = c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none")


dev.off()

# makes plots for individual variable pvalues
bind_val_test <- bind_val %>%
  mutate(s_dem = 1) %>% 
  mutate(s_ptoc = 1) %>% 
  mutate(s_tpi = 1) %>% 
  mutate(s_asp = 1) %>% 
  mutate(s_sum_irradiance = 1) %>% 
  mutate(s_dem = if_else(!is.na(`s(dem,month)`), `s(dem,month)`, s_dem),
         s_dem = if_else(!is.na(`s(dem,week)`),  `s(dem,week)` , s_dem),
         s_dem = if_else(!is.na(`s(dem,yday)`),  `s(dem,yday)` , s_dem),
         s_dem = if_else(!is.na(`s(dem)`),       `s(dem)`      , s_dem)) %>% 
  mutate(s_ptoc = if_else(!is.na(`s(ptoc,month)`), `s(ptoc,month)`, s_ptoc),
         s_ptoc = if_else(!is.na(`s(ptoc,week)`),  `s(ptoc,week)` , s_ptoc),
         s_ptoc = if_else(!is.na(`s(ptoc,yday)`),  `s(ptoc,yday)` , s_ptoc),
         s_ptoc = if_else(!is.na(`s(ptoc)`),       `s(ptoc)`      , s_ptoc)) %>% 
  mutate(s_tpi = if_else(!is.na(`s(tpi,month)`), `s(tpi,month)`, s_tpi),
         s_tpi = if_else(!is.na(`s(tpi,week)`),  `s(tpi,week)` , s_tpi),
         s_tpi = if_else(!is.na(`s(tpi,yday)`),  `s(tpi,yday)` , s_tpi),
         s_tpi = if_else(!is.na(`s(tpi)`),       `s(tpi)`      , s_tpi)) %>% 
  mutate(s_asp = if_else(!is.na(`s(asp,month)`), `s(asp,month)`, s_asp),
         s_asp = if_else(!is.na(`s(asp,week)`),  `s(asp,week)` , s_asp),
         s_asp = if_else(!is.na(`s(asp,yday)`),  `s(asp,yday)` , s_asp)) %>% 
  mutate(s_sum_irradiance = if_else(!is.na(`s(sum_irradiance,yday)`),  `s(sum_irradiance,yday)` , s_sum_irradiance),
         s_sum_irradiance = if_else(!is.na(`s(sum_irradiance)`),       `s(sum_irradiance)`      , s_sum_irradiance))


ggplot(data = bind_val_test) +
  geom_point(aes(x = yday, y = s_dem, colour = timeframe)) +
  facet_grid(timeframe~temp_var) +
  labs(x = "Day of the Year (2012)", y = "Adj. R^2") +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  coord_cartesian(ylim = c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none")

ggplot(data = bind_val_test) +
  geom_point(aes(x = yday, y = s_tpi, colour = timeframe)) +
  facet_grid(timeframe~temp_var) 

ggplot(data = bind_val_test) +
  geom_point(aes(x = yday, y = s_ptoc, colour = timeframe)) +
  facet_grid(timeframe~temp_var) 
