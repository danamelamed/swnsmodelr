# Model Testing
library(swnsmodelr)
library(ggplot2)
# Add date columns to data frames
model_stations_df <- add_date_columns(model_stations_df)
val_stations_df   <- add_date_columns(val_stations_df)


model_df_2012 <- model_stations_df %>% filter(year == 2012) # remove asp_c from these
val_df_2012 <- val_stations_df %>% filter(year == 2012)

#### all years and one year ##### 
all_years <- list()
all_years_val <- list()
one_year <- list()
one_year_val <- list()
# 1: max, 2: min 3: mean
temp_var <- list("min", "max", "mean")
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
      all_years_val[[i]] <- add_residuals(val_df_2012, all_years[[i]])
      all_years_val[[i]] <- all_years_val[[i]] %>% select(-dem, -sum_irradiance, -ptoc,
                                                          -tpi, -asp, -east, - north)
      all_years_val[[i]]$timeframe <- "All years"
      all_years_val[[i]]$knots <- "No limit"
      all_years_val[[i]]$temp_var <- temp_var[[i]]
      all_years_val[[i]]$gcv <- all_years[[i]]$gcv.ubre
      all_years_val[[i]]$rsq <- summary(all_years[[i]])[[10]]
      all_years_val[[i]]$dev <- summary(all_years[[i]])[[14]]
      all_years_val[[i]]$abs_resid <- abs(all_years_val[[i]]$resid)
      for(l in seq_along(summary(all_years[[i]])[[7]])){
        all_years_val[[i]]$var_pval <- summary(all_years[[i]])[[8]][[l]]
        names(all_years_val[[i]])[names(all_years_val[[i]]) == "var_pval"] <- names(summary(all_years[[i]])[[7]])[[l]]
      }
      
      if(is.na(all_years_val[[i]]$abs_resid)){
        all_years[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                         s(dem,month) +
                         s(ptoc,month, k= 3) +
                         s(tpi,month)+
                         s(asp, month) +
                         s(east,north) +
                         s(week) +
                         year")), 
                              data = model_stations_df)
        all_years_val[[i]] <- add_residuals(val_df_2012, all_years[[i]])
        all_years_val[[i]]$timeframe <- "All years"
        all_years_val[[i]]$knots <- "No limit"
        all_years_val[[i]]$temp_var <- temp_var[[i]]
        all_years_val[[i]]$gcv <- all_years[[i]]$gcv.ubre
        all_years_val[[i]]$rsq <- summary(all_years[[i]])[[10]]
        all_years_val[[i]]$dev <- summary(all_years[[i]])[[14]]
        all_years_val[[i]]$abs_resid <- abs(all_years_val[[i]]$resid)
        for(l in seq_along(summary(all_years[[i]])[[7]])){
          all_years_val[[i]]$var_pval <- summary(all_years[[i]])[[8]][[l]]
          names(all_years_val[[i]])[names(all_years_val[[i]]) == "var_pval"] <- names(summary(all_years[[i]])[[7]])[[l]]
        }
      }
      print(i)
}
      
for(i in seq_along(temp_var)){      
      one_year[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                        s(dem,week) +
                        s(ptoc,week, k= 3)+
                        s(sum_irradiance, week) +
                        s(tpi,week)+
                        s(asp, week) +
                        s(east,week) +
                        s(yday) +
                        month")), 
                      data = model_df_2012)
      one_year_val[[i]] <- add_residuals(val_df_2012, one_year[[i]])
      one_year_val[[i]] <- one_year_val[[i]] %>% select(-dem, -sum_irradiance, -ptoc,
                                                          -tpi, -asp, -east, - north)
      one_year_val[[i]]$timeframe <- "Yearly"
      one_year_val[[i]]$knots <- "No limit"
      one_year_val[[i]]$temp_var <- temp_var[[i]]
      one_year_val[[i]]$gcv <- one_year[[i]]$gcv.ubre
      one_year_val[[i]]$rsq <- summary(one_year[[i]])[[10]]
      one_year_val[[i]]$dev <- summary(one_year[[i]])[[14]]
      one_year_val[[i]]$abs_resid <- abs(one_year_val[[i]]$resid)
      for(l in seq_along(summary(one_year[[i]])[[7]])){
        one_year_val[[i]]$var_pval <- summary(one_year[[i]])[[8]][[l]]
        names(one_year_val[[i]])[names(one_year_val[[i]]) == "var_pval"] <- names(summary(one_year[[i]])[[7]])[[l]]
      }
      
      if(is.na(one_year_val[[i]]$abs_resid)){
        one_year[[i]] <- gam(formula(paste0("temp_",temp_var[[i]]," ~
                        s(dem,week) +
                                    s(ptoc,week, k= 3)+
                                    s(tpi,week)+
                                    s(asp, week) +
                                    s(east,week) +
                                    s(yday) +
                                    month")), 
                             data = model_df_2012)
        one_year_val[[i]] <- add_residuals(val_df_2012, one_year[[i]])
        one_year_val[[i]]$timeframe <- "Yearly"
        one_year_val[[i]]$temp_var <- temp_var[[i]]
        one_year_val[[i]]$gcv <- one_year[[i]]$gcv.ubre
        one_year_val[[i]]$rsq <- summary(one_year[[i]])[[10]]
        one_year_val[[i]]$knots <- "No limit"
        one_year_val[[i]]$dev <- summary(one_year[[i]])[[14]]
        one_year_val[[i]]$abs_resid <- abs(one_year_val[[i]]$resid)
        for(l in seq_along(summary(one_year[[i]])[[7]])){
          one_year_val[[i]]$var_pval <- summary(one_year[[i]])[[8]][[l]]
          names(one_year_val[[i]])[names(one_year_val[[i]]) == "var_pval"] <- names(summary(one_year[[i]])[[7]])[[l]]
        }
      }
      

}



#### weekly####
# Generate models with daily mins, maxs and means


# Create list of three dataframes
val_df_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  val_df_list[[i]] <- validate_weekly_GAMs(model_stations_df = model_stations_df,
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
                                                         s(ptoc,yday)"),
                                           verbose = TRUE
  ) 
  val_df_list[[i]]$temp_var <- temp_var[[i]]
  
}

val_df <- dplyr::bind_rows(val_df_list)
val_df$knots <- "No limit"


val_knots_df <- list()
for(j in seq_along(temp_var)){# Generate models with daily mins, maxs and means
  
  val_no_limit<- filter(val_df, temp_var == temp_var[[j]])
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_stations_df,
                                             years = 2012,
                                             weeks = 1:52,
                                             formula =paste0("temp_",temp_var[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday, k=",knots[[i]],")  +
                                                                 s(sum_irradiance, yday, k=",knots[[i]],") +
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(tpi, yday, k=",knots[[i]],") + 
                                                                 s(ptoc,yday, k=3)"),
                                             alt_formula =paste0("temp_",temp_var[[j]]," ~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday, k=",knots[[i]],")  +
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(tpi, yday, k=",knots[[i]],") + 
                                                                 s(ptoc,yday, k=3)"),
                                             verbose = TRUE
    ) 
    val_df_list[[i]]$knots <- as.character(knots[[i]])
    val_df_list[[i]]$temp_var <- temp_var[[j]]
  }
  
  val_knots_df[[j]] <- dplyr::bind_rows(val_no_limit , val_df_list)
}

val_knots_weekly_df <- dplyr::bind_rows(val_knots_df)

#### daily ####
# Generate models with daily mins, maxs and means


# Create list of three dataframes
val_df_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  val_df_list[[i]] <- validate_daily_GAMs(model_stations_df = model_stations_df,
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
  val_df_list[[i]]$temp_var <- temp_var[[i]]
  print(temp_var[[i]])

  
}

val_df <- dplyr::bind_rows(val_df_list)
val_df$knots <- "No limit"


val_knots_df <- list()
for(j in seq_along(temp_var)){# Generate models with daily mins, maxs and means
  
  val_no_limit<- filter(val_df, temp_var == temp_var[[j]])
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                            val_stations_df = val_stations_df,
                                            years = 2012,
                                            days = 1:365,
                                            formula =paste0("temp_",temp_var[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, k=",knots[[i]],")  +
                                                                 s(sum_irradiance, k=",knots[[i]],") +
                                                                 s(tpi, k=",knots[[i]],") + 
                                                                 s(ptoc, k=3)"),
                                            alt_formula =paste0("temp_",temp_var[[j]]," ~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, k=",knots[[i]],")  +
                                                                 s(tpi, k=",knots[[i]],") + 
                                                                 s(ptoc, k=3)")
    ) 
    val_df_list[[i]]$knots <- as.character(knots[[i]])
    val_df_list[[i]]$temp_var <- temp_var[[j]]
  }
  
  val_knots_df[[j]] <- dplyr::bind_rows(val_no_limit , val_df_list)
}

val_knots_daily_df <- dplyr::bind_rows(val_knots_df)


#### monthly ####
# Generate models with daily mins, maxs and means


# Create list of three dataframes
val_df_list <- list()
# Loop over each option: min, max and mean
for(i in seq_along(temp_var)){
  val_df_list[[i]] <- validate_monthly_GAMs(model_stations_df = model_stations_df,
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
  val_df_list[[i]]$temp_var <- temp_var[[i]]
}

val_df <- dplyr::bind_rows(val_df_list)
val_df$knots <- "No limit"

knots <- list(1, 5 ,9)

val_knots_df <- list()
for(j in seq_along(temp_var)){# Generate models with daily mins, maxs and means
  
 
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_stations_df,
                                             years = 2012,
                                             months = 1:12,
                                             formula =paste0("temp_",temp_var[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday,k=",knots[[i]],")  +
                                                                 s(sum_irradiance, yday, k=",knots[[i]],") +
                                                                 s(tpi,yday, k=",knots[[i]],") + 
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(ptoc,yday, k=3) +
                                                             week"),
                                             alt_formula =paste0("temp_",temp_var[[j]]," ~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday,k=",knots[[i]],")  +
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(tpi,yday, k=",knots[[i]],") + 
                                                                 s(ptoc, yday,k=3) +
                                                                 week")
    ) 
    val_df_list[[i]]$knots <- as.character(knots[[i]])
    val_df_list[[i]]$temp_var <- temp_var[[j]]
  }
  
  val_knots_df[[j]] <- dplyr::bind_rows(val_df_list)
}

val_knots_monthly_df <- dplyr::bind_rows(val_knots_df, val_df)



val_knots_monthly_df$timeframe <- "Monthly"
val_knots_weekly_df$timeframe <- "Weekly"
val_knots_daily_df$timeframe <- "Daily"
#### bind and wrangle data ####
all_years_val_df <- bind_rows(unlist(all_years_val))
one_year_val-df <- bind_rows(unlist(one_year_val))

# plot pvals of terms###
dem_pvals <- bind_cols(select(all_years_val, `s(dem,month)`),
                       select(one_year_val, `s(dem,month)`),
                       select(monthly_val, `s(dem,week)`))




#### plot residuals daily vs weekly
d_vs_w <- bind_val %>% filter(timeframe %in% c("daily","weekly"),
                              knots == 9)
ggplot(d_vs_w, aes(x =  resid)) +
  geom_density(aes(colour = timeframe))

mean <- bind_val %>% filter(temp_var == "mean" & timeframe != "weekly_knots")

# bind knots frames
knots_df <- bind_rows(val_knots_monthly_df, val_knots_weekly_df, val_knots_daily_df)
ggplot(data = mean, aes(x = temp_mean, y = abs_resid)) +
  geom_point(aes(colour = timeframe_2)) +
  facet_wrap(~knots)



## Plots

colours <- list("light blue","light pink","beige")
# all years
for(i in seq_along(temp_var)){
all_years_now <- all_years_val[[i]]
print(ggplot(data = all_years_now) +
  geom_histogram(aes(x = resid),
               colour = "black",
               fill = colours[[i]],
               binwidth = 2) +
  labs(title = paste0(all_years_now$timeframe[[1]]," : ",
                    all_years_now$temp_var[[1]], 
                    " daily temperature model"),
       x = "Residuals at Validation Stations") +
  coord_cartesian(xlim = c(-20,20),
                  ylim = c(0,1500)) )
}

# one year
for(i in seq_along(temp_var)){
one_year_now <- one_year_val[[i]]
print(ggplot(data = one_year_now) +
  geom_histogram(aes(x = resid),
                 colour = "black",
                 fill = colours[[i]],
                 binwidth = 2) +
  labs(title = paste0(one_year_now$timeframe[[i]]," : ",
                      one_year_now$temp_var[[i]], 
                      " daily temperature model"),
       x = "Residuals at Validation Stations") +
  coord_cartesian(xlim = c(-20,20),
                  ylim = c(0,1500)))
}

# weekly
for(i in seq_along(temp_var)){
  weekly_now <- val_knots_weekly_df %>% filter(knots == "No limit" &
                                             temp_var == temp_var[[i]])
  print(ggplot(data = weekly_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(weekly_now$timeframe[[i]]," : ",
                              weekly_now$temp_var[[i]],
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,2000)))

}




# daily
for(i in seq_along(temp_var)){
  temp_now <- temp_var[[i]]
  daily_now <- daily_val_df %>% filter(knots == "No limit" &
                                                 temp_var == temp_now)
  
  print(ggplot(data = daily_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(daily_now$timeframe[[1]]," : ",
                              daily_now$temp_var[[1]],
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,2000)))
  
}


# monthly
for(i in seq_along(temp_var)){
  monthly_now <- val_knots_monthly_df %>% filter(knots == "No limit" &
                                                 temp_var == temp_var[[i]])
  print(ggplot(data = monthly_now) +
          geom_histogram(aes(x = resid),
                         colour = "black",
                         fill = colours[[i]],
                         binwidth = 2) +
          labs(title = paste0(weekly_now$timeframe[[i]]," : ",
                              weekly_now$temp_var[[i]],
                              " daily temperature model"),
               x = "Residuals at Validation Stations") +
          coord_cartesian(xlim = c(-20,20),
                          ylim = c(0,2000)))
  
}
