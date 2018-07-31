# Testing models with different allowed knots

##### All years timeframe #####
knots <- list(1,5,9)
all_years_knots <- list()
all_years_val_knots_list <- list()
for(i in seq_along(knots)){
  all_years_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                       s(dem,month, k= ",knots[[i]],") +
                                       s(ptoc,month, k= 3) +
                                       s(sum_irradiance, month) +
                                       s(tpi,month, k= ",knots[[i]],") +
                                       s(asp, month, k= ",knots[[i]],") +
                                       s(east,north, k= ",knots[[i]],") +
                                       s(week, k= ",knots[[i]],") +
                                       year")), 
                        data = model_stations_df)
  all_years_val_knots_list[[i]] <- add_residuals(val_df_2012, all_years_knots[[i]])
  
  
  if(is.na(all_years_val_knots_list[[i]]$resid)){
    all_years_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                         s(dem,month, k= ",knots[[i]],") +
                                         s(ptoc,month, k= 3) +
                                         s(tpi,month, k= ",knots[[i]],")+
                                         s(asp, month, k= ",knots[[i]],") +
                                         s(east,north, k= ",knots[[i]],") +
                                         s(week, k= ",knots[[i]],") +
                                         year")), 
                          data = model_stations_df)
    all_years_val_knots_list[[i]] <- add_residuals(val_df_2012, all_years_knots[[i]])
  }
  all_years_val_knots_list[[i]]$timeframe <- "All years"
  all_years_val_knots_list[[i]]$knots <- knots[[i]]
  all_years_val_knots_list[[i]]$gcv <- all_years_knots[[i]]$gcv.ubre
  all_years_val_knots_list[[i]]$rsq <- summary(all_years_knots[[i]])[[10]]
  all_years_val_knots_list[[i]]$dev <- summary(all_years_knots[[i]])[[14]]
  all_years_val_knots_list[[i]]$abs_resid <- abs(all_years_val_knots_list[[i]]$resid)
  for(l in seq_along(summary(all_years_knots[[i]])[[7]])){
    all_years_val_knots_list[[i]]$var_pval <- summary(all_years_knots[[i]])[[8]][[l]]
    names(all_years_val_knots_list[[i]])[names(all_years_val_knots_list[[i]]) == "var_pval"] <- names(summary(all_years_knots[[i]])[[7]])[[l]]
  }
}
all_years_val_knots <- bind_rows(all_years_val_knots_list) 


##### Annual timeframe ####
annual_knots <- list()
annual_val_knots_list <- list()
for(i in seq_along(knots)){      
  annual_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                    s(dem,week, k= ",knots[[i]],") +
                                    s(ptoc,week, k= 3)+
                                    s(sum_irradiance, week, k= ",knots[[i]],") +
                                    s(tpi,week, k= ",knots[[i]],")+
                                    s(asp, week, k= ",knots[[i]],") +
                                    s(east,week, k= ",knots[[i]],") +
                                    s(yday, k= ",knots[[i]],") +
                                    month")), 
                       data = model_df_2012)
  annual_val_knots_list[[i]] <- add_residuals(val_df_2012, annual_knots[[i]])

  
  if(is.na(annual_val_knots_list[[i]]$resid)){
    annual_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                      s(dem,week, k= ",knots[[i]],") +
                                      s(ptoc,week, k= 3)+
                                      s(tpi,week, k= ",knots[[i]],")+
                                      s(asp, week, k= ",knots[[i]],") +
                                      s(east,week, k= ",knots[[i]],") +
                                      s(yday, k= ",knots[[i]],") +
                                      month")), 
                         data = model_df_2012)
    annual_val_knots_list[[i]] <- add_residuals(val_df_2012, annual_knots[[i]])
    }
    for(l in seq_along(summary(annual_knots[[i]])[[7]])){
      annual_val_knots_list[[i]]$var_pval <- summary(annual_knots[[i]])[[8]][[l]]
      names(annual_val_knots_list[[i]])[names(annual_val_knots_list[[i]]) == "var_pval"] <- names(summary(annual_knots[[i]])[[7]])[[l]]
    }
    annual_val_knots_list[[i]]$timeframe <- "Annual"
    annual_val_knots_list[[i]]$gcv <- annual_knots[[i]]$gcv.ubre
    annual_val_knots_list[[i]]$rsq <- summary(annual_knots[[i]])[[10]]
    annual_val_knots_list[[i]]$knots <- knots[[i]]
    annual_val_knots_list[[i]]$dev <- summary(annual_knots[[i]])[[14]]
    annual_val_knots_list[[i]]$abs_resid <- abs(annual_val_knots_list[[i]]$resid)
}
annual_val_knots <- bind_rows(annual_val_knots_list)


##### Monthly Timeframe #####
# Create list of three dataframes
month_val_knots_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(knots)){
  month_val_knots_list[[i]] <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                                 val_stations_df = val_stations_df,
                                                 years = 2012,
                                                 months = 1:12,
                                                 formula =paste0("temp_mean~
                                                           s(east,north, k= ",knots[[i]],") +
                                                           s(dem, yday, k= ",knots[[i]],")  +
                                                           s(sum_irradiance, yday, k= ",knots[[i]],") +
                                                           s(tpi, yday, k= ",knots[[i]],") + 
                                                           s(asp, yday, k= ",knots[[i]],") +
                                                           s(ptoc, k = 3) +
                                                           week"),
                                                 alt_formula =paste0("temp_mean~
                                                               s(east,north, yday, k= ",knots[[i]],") +
                                                               s(dem, yday, k= ",knots[[i]],")  +
                                                               s(tpi, yday, k= ",knots[[i]],") + 
                                                               s(asp, yday, k= ",knots[[i]],")
                                                               s(ptoc, yday, k = 3) +
                                                               week")
  ) 
  month_val_knots_list[[i]]$knots <- knots[[i]]
}

monthly_val_knots <- dplyr::bind_rows(month_val_knots_list)
monthly_val_knots$timeframe <- "Monthly"


##### Weekly Timeframe #####
# Create list of three dataframes
weekly_val_knots_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(knots)){
  weekly_val_knots_list[[i]] <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                               val_stations_df = val_stations_df,
                                               years = 2012,
                                               weeks = 1:52,
                                               formula =paste0("temp_mean~
                                                         s(east,north, k= ",knots[[i]],") +
                                                         s(dem, yday, k= ",knots[[i]],")  +
                                                         s(sum_irradiance, yday, k= ",knots[[i]],") +
                                                         s(asp, yday, k= ",knots[[i]],") +
                                                         s(tpi, yday, k= ",knots[[i]],") + 
                                                         s(ptoc,yday, k= 3)"),
                                               alt_formula =paste0("temp_mean~
                                                         s(east,north, k= ",knots[[i]],") +
                                                         s(dem, yday, k= ",knots[[i]],")  +
                                                         s(asp, yday, k= ",knots[[i]],") +
                                                         s(tpi, yday, k= ",knots[[i]],") + 
                                                         s(ptoc,yday, k= 3)")
  ) 
  weekly_val_knots_list[[i]]$knots <- knots[[i]]
  
}

weekly_val_knots <- dplyr::bind_rows(weekly_val_knots_list)
weekly_val_knots$timeframe <- "Weekly"


##### Daily Timeframe #####
# Create list of three dataframes
daily_val_knots_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(knots)){
  daily_val_knots_list[[i]] <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_stations_df,
                                             years = 2012,
                                             days = 1:365,
                                             formula =paste0("temp_mean~
                                                             s(east,north, k= ",knots[[i]],") +
                                                             s(dem, k= ",knots[[i]],")  +
                                                             s(sum_irradiance, k= ",knots[[i]],") +
                                                             s(tpi, k= ",knots[[i]],") + 
                                                             s(ptoc, k = 3)"),
                                             alt_formula =paste0("temp_mean~
                                                                 s(east,north, k= ",knots[[i]],") +
                                                                 s(dem, k= ",knots[[i]],")  +
                                                                 s(tpi, k= ",knots[[i]],") + 
                                                                 s(ptoc, k = 3)")
                                             ) 
  daily_val_knots_list[[i]]$knots <- knots[[i]]
  print(temp_var[[i]])
  
}
daily_val_knots <- dplyr::bind_rows(daily_val_knots_list)
daily_val_knots$timeframe <- "Daily"


##### Plots #####
colours <- list("light green","light pink","light blue")
knots_list <- c(1,5,9)
# all years
for(i in seq_along(knots_list)){
  all_years_now <- all_years_val_knots %>% filter(knots == knots_list[[i]])
  print(ggplot(data = all_years_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(all_years_now$timeframe[[1]]," : k = ",
                              all_years_now$knots[[1]], 
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)) )
}

# one year
for(i in seq_along(knots_list)){
  annual_now <- annual_val_knots %>% filter(knots == knots_list[[i]])
  print(ggplot(data = annual_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(annual_now$timeframe[[1]]," : k = ",
                              annual_now$knots[[1]], 
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)) )
}

# monthly
for(i in seq_along(knots_list)){
  monthly_now <- monthly_val_knots %>% filter(knots == knots_list[[i]])
  print(ggplot(data = monthly_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(monthly_now$timeframe[[1]]," : k = ",
                              monthly_now$knots[[1]], 
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)) )
}


# weekly
for(i in seq_along(knots_list)){
  weekly_now <- weekly_val_knots %>% filter(knots == knots_list[[i]])
  print(ggplot(data = weekly_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(weekly_now$timeframe[[1]]," : k = ",
                              weekly_now$knots[[1]], 
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)) )
}




# daily
for(i in seq_along(knots_list)){
  daily_now <- daily_val_knots %>% filter(knots == knots_list[[i]])
  print(ggplot(data = daily_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(daily_now$timeframe[[1]]," : k = ",
                              daily_now$knots[[1]], 
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)) )
}

daily_val_knots$knots <- as.character(knots)

val_knots_list <- list(all_years_val_knots,
                 annual_val_knots,
                 monthly_val_knots,
                 weekly_val_knots,
                 daily_val_knots)

#### Plots ####
pdf("E://testpdf.pdf", height = 2)
# Error at validation stations histogram
for(i in seq_along(val_knots_list)){
  df_val_now <- val_knots_list[[i]]
  df_val <- val_list[[i]] %>% 
    filter(temp_var == "mean") %>%
    mutate(knots = "No limit") %>%
    bind_rows(df_val_now)
  df_val$knots <- as.character(df_val$knots)
  print(ggplot(df_val) + 
          geom_histogram(aes(x = resid, fill  = knots), 
                         colour = "black",
                         binwidth = 2) + 
          facet_wrap(~ knots) + 
          scale_fill_manual(values = c("salmon","forest green","royal blue","dark grey"),
                            guide = guide_legend(title = "Value of K"))  +
          labs(title = paste0(letters[i], ") ",df_val$timeframe),
               x = "Error at Validation Stations") + 
         # theme(strip.background = element_blank(), strip.text = element_blank()) +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,5500)))
}


# Error at validation station by temp_mean
for(i in seq_along(val_knots_list)){
  df_val_now <- val_knots_list[[i]]
  df_val <- val_list[[i]] %>% 
    filter(temp_var == "mean") %>%
    mutate(knots = "No limit") %>%
    bind_rows(df_val_now)
  df_val$knots <- as.character(df_val$knots)
  print(ggplot(df_val) + 
          geom_smooth(aes(x = temp_mean, y = resid, colour  = knots)) + 
          facet_wrap(~ knots) + 
          scale_colour_manual(values = c("salmon","forest green","royal blue","dark grey"),
                            guide = guide_legend(title = "Value of K")) +
          labs(title = paste0(letters[i], ") ",df_val$timeframe),
               x = "Daily Mean Temperature",
               y = "Error at Validation Stations") + 
       #   theme(strip.background = element_blank(), strip.text = element_blank()) +
          coord_cartesian(ylim = c(-10,10)))
}

# GCV scores smooth
for(i in seq_along(val_knots_list)){
  df_val_now <- val_knots_list[[i]]
  df_val <- val_list[[i]] %>% 
    filter(temp_var == "mean") %>%
    mutate(knots = "No limit") %>%
    bind_rows(df_val_now)
  df_val$knots <- as.character(df_val$knots)
  print(ggplot(df_val) + 
          geom_smooth(aes(y = gcv, x =  yday, colour  = knots)) + 
          facet_wrap(~ knots) + 
          scale_colour_manual(values = c("salmon","forest green","royal blue","dark grey"),
                            guide = guide_legend(title = "Value of K")) +
          labs(title = paste0(letters[i], ") ",df_val$timeframe),
               y = "GCV Scores",
               x = "Day of the Year 2012") + 
        #  theme(strip.background = element_blank(), strip.text = element_blank()) +
          coord_cartesian(ylim = c(0,20)))
}

# Adjusted R2 scores smooth
for(i in seq_along(val_knots_list)){
  df_val_now <- val_knots_list[[i]]
  df_val <- val_list[[i]] %>% 
    filter(temp_var == "mean") %>%
    mutate(knots = "No limit") %>%
    bind_rows(df_val_now)
  df_val$knots <- as.character(df_val$knots)
  print(ggplot(df_val) + 
          geom_smooth(aes(y = rsq, x =  yday, colour  = knots)) + 
          facet_wrap(~ knots) + 
          scale_colour_manual(values = c("salmon","forest green","royal blue","dark grey"),
                              guide = guide_legend(title = "Value of K")) +
          labs(title = paste0(letters[i], ") ",df_val$timeframe),
               y = "Adjusted Rsquared",
               x = "Day of the Year 2012") + 
         # theme(strip.background = element_blank(), strip.text = element_blank()) +
          coord_cartesian(ylim = c(0,1)))
}
dev.off()
