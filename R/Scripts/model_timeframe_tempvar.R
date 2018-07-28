# Testing models with different daily temperature variables and timeframes

##### All years timeframe #####
temp_var <- list("min", "max", "mean")
all_years <- list()
all_years_val <- list()
for(i in seq_along(temp_var)){
  all_years_val[[i]] <- gam(formula(paste0("temp_",temp_var," ~
                                       s(dem,month) +
                                       s(ptoc,month, k= 3) +
                                       s(sum_irradiance, month) +
                                       s(tpi,month) +
                                       s(asp, month) +
                                       s(east,north) +
                                       s(week) +
                                       year")), 
                        data = model_stations_df)
  all_years_val[[i]] <- add_residuals(val_df_2012, all_years_val[[i]])

  
  if(is.na(all_years_val[[i]]$resid)){
    all_years_val[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                                         s(dem,month) +
                                         s(ptoc,month, k= 3) +
                                         s(tpi,month)+
                                         s(asp, month) +
                                         s(east,north) +
                                         s(week) +
                                         year")), 
                          data = model_stations_df)
    all_years_val[[i]] <- add_residuals(val_df_2012, all_years_val[[i]])
  }
  all_years_val[[i]]$timeframe <- "All years"
  all_years_val[[i]]$knots <- "No limit"
  all_years_val[[i]]$temp_var <- temp_var[[i]]
  all_years_val[[i]]$gcv <- all_years_val[[i]]$gcv.ubre
  all_years_val[[i]]$rsq <- summary(all_years_val[[i]])[[10]]
  all_years_val[[i]]$dev <- summary(all_years_val[[i]])[[14]]
  all_years_val[[i]]$abs_resid <- abs(all_years_val[[i]]$resid)
  for(l in seq_along(summary(all_years_val[[i]])[[7]])){
    all_years_val[[i]]$var_pval <- summary(all_years_val[[i]])[[8]][[l]]
    names(all_years_val[[i]])[names(all_years_val[[i]]) == "var_pval"] <- names(summary(all_years_val[[i]])[[7]])[[l]]
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

for(i in seq_along(val_list)){
df_val <- val_list[[i]]
print(ggplot(df_val) + 
  geom_histogram(aes(x = resid, fill  = temp_var), 
                 colour = "black",
                 binwidth = 2) + 
  facet_wrap(~ temp_var) + 
  scale_fill_manual(values = c("light blue","light pink","beige"),
                    guide = guide_legend(title = "Daily Temperature Variable"),
                    labels = c("Maximum","Minimum",'Mean')) +
  labs(title = paste0(letters[i], ") ",df_val$timeframe),
       x = "Error at Validation Stations") + 
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  coord_cartesian(xlim = c(-20,20),
                  ylim = c(0,5500)))
}
