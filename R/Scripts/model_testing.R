# Testing time frames and knots
model_stations_df <- add_date_columns(model_stations_df)
val_stations_df   <- add_date_columns(val_stations_df)

df <- model_stations_df %>% filter(year == 2012)
val_2012 <- val_stations_df %>% filter(year == 2012)
#### all years and one year ##### 
# 1: max, 2: min 3: mean

all_years_mean <- gam(temp_mean ~
                   s(dem,month) +
                   s(ptoc,month, k= 3) +
                   s(sum_irradiance, month) +
                   s(tpi,month)+
                   s(asp, month) +
                   s(east,north) +
                   s(week) +
                   year, 
                 data = swns_stations_df_200)
all_years_val_mean <- add_residuals(val_2012, all_years_mean)
all_years_val_mean$timeframe <- "all years"
all_years_val_mean$temp_var <- "mean"
all_years_val_mean$gcv <- all_years_mean$gcv.ubre
all_years_val_mean$rsq <- summary(all_years_mean)[[10]]
all_years_val_mean$dev <- summary(all_years_mean)[[14]]
all_years_val_mean$abs_resid <- abs(all_years_val_mean$resid)

one_year_mean <- gam(temp_mean ~
                  s(dem,week) +
                  s(ptoc,week, k= 3)+
                  s(sum_irradiance, week) +
                  s(tpi,week)+
                  s(asp, week) +
                  s(east,week) +
                  s(yday) +
                  month, 
                data = df)
one_year_val_mean <- add_residuals(val_2012, one_year_mean)
one_year_val_mean$timeframe <- "yearly"
one_year_val_mean$temp_var <- "mean"
one_year_val_mean$gcv <- one_year_mean$gcv.ubre
one_year_val_mean$rsq <- summary(one_year_mean)[[10]]
one_year_val_mean$dev <- summary(one_year_mean)[[14]]
one_year_val_mean$abs_resid <- abs(one_year_val_mean$resid)
all_years_mean <- gam(temp_mean ~
                        s(dem,month) +
                        s(ptoc,month, k= 3) +
                        s(sum_irradiance, month) +
                        s(tpi,month)+
                        s(asp, month) +
                        s(east,north) +
                        s(week) +
                        year, 
                      data = swns_stations_df_200)
all_years_val_max <- add_residuals(val_2012, all_years_max)
all_years_val_max$timeframe <- "all years"
all_years_val_max$temp_var <- "max"
all_years_val_max$gcv <- all_years_max$gcv.ubre
all_years_val_max$rsq <- summary(all_years_max)[[10]]
all_years_val_max$dev <- summary(all_years_max)[[14]]
all_years_val_max$abs_resid <- abs(all_years_val_max$resid)

one_year_max <- gam(temp_max ~
                       s(dem,week) +
                       s(ptoc,week, k= 3)+
                       s(sum_irradiance, week) +
                       s(tpi,week)+
                       s(asp, week) +
                       s(east,week) +
                       s(yday) +
                       month, 
                     data = df)
one_year_val_max <- add_residuals(val_2012, one_year_max)
one_year_val_max$timeframe <- "yearly"
one_year_val_max$temp_var <- "max"
one_year_val_max$gcv <- one_year_max$gcv.ubre
one_year_val_max$rsq <- summary(one_year_max)[[10]]
one_year_val_max$dev <- summary(one_year_max)[[14]]
one_year_val_max$abs_resid <- abs(one_year_val_max$resid)

all_years_min <- gam(temp_min ~
                        s(dem,month) +
                        s(ptoc,month, k= 3) +
                        s(sum_irradiance, month) +
                        s(tpi,month)+
                        s(asp, month) +
                        s(east,north) +
                        s(week) +
                        year, 
                      data = swns_stations_df_200)
all_years_val_min <- add_residuals(val_2012, all_years_min)
all_years_val_min$timeframe <- "all years"
all_years_val_min$temp_var <- "min"
all_years_val_min$gcv <- all_years_min$gcv.ubre
all_years_val_min$rsq <- summary(all_years_min)[[10]]
all_years_val_min$dev <- summary(all_years_min)[[14]]
all_years_val_min$abs_resid <- abs(all_years_val_min$resid)

one_year_min <- gam(temp_min ~
                       s(dem,week) +
                       s(ptoc,week, k= 3)+
                       s(sum_irradiance, week) +
                       s(tpi,week)+
                       s(asp, week) +
                       s(east,week) +
                       s(yday) +
                       month, 
                     data = df)
one_year_val_min <- add_residuals(val_2012, one_year_min)
one_year_val_min$timeframe <- "yearly"
one_year_val_min$temp_var <- "min"
one_year_val_min$gcv <- one_year_min$gcv.ubre
one_year_val_min$rsq <- summary(one_year_min)[[10]]
one_year_val_min$dev <- summary(one_year_min)[[14]]
one_year_val_min$abs_resid <- abs(one_year_val_min$resid)


#### weekly####
# Generate models with daily mins, maxs and means
temp_var <- list("min", "max", "mean")

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
  val_df_list[[i]]$knots   <- "no limit"
}

val_df <- dplyr::bind_rows(val_df_list)
knots <- list(1, 5 ,9)
temp_vars<- list("min","max","mean")
val_knots_df <- list()
for(j in seq_along(temp_var)){# Generate models with daily mins, maxs and means
  
  val_no_limit<- filter(val_df, temp_var == temp_vars[[j]])
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_stations_df,
                                             years = 2012,
                                             weeks = 1:52,
                                             formula =paste0("temp_",temp_vars[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday, k=",knots[[i]],")  +
                                                                 s(sum_irradiance, yday, k=",knots[[i]],") +
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(tpi, yday, k=",knots[[i]],") + 
                                                                 s(ptoc,yday, k=3)"),
                                             alt_formula =paste0("temp_",temp_vars[[j]]," ~
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
temp_var <- list("min", "max", "mean")

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
  val_df_list[[i]]$knots   <- "no limit"
}

val_df <- dplyr::bind_rows(val_df_list)

knots <- list(1, 5 ,9)
temp_vars<- list("min","max","mean")
val_knots_df <- list()
for(j in seq_along(temp_vars)){# Generate models with daily mins, maxs and means
  
  val_no_limit<- filter(val_df, temp_var == temp_vars[[j]])
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                            val_stations_df = val_stations_df,
                                            years = 2012,
                                            days = 1:365,
                                            formula =paste0("temp_",temp_vars[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, k=",knots[[i]],")  +
                                                                 s(sum_irradiance, k=",knots[[i]],") +
                                                                 s(tpi, k=",knots[[i]],") + 
                                                                 s(ptoc, k=3)"),
                                            alt_formula =paste0("temp_",temp_vars[[j]]," ~
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
temp_var <- list("min", "max", "mean")

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
  val_df_list[[i]]$knots   <- "no limit"
}

val_df <- dplyr::bind_rows(val_df_list)

knots <- list(1, 5 ,9)
temp_vars<- list("min","max","mean")
val_knots_df <- list()
for(j in seq_along(temp_vars)){# Generate models with daily mins, maxs and means
  
  val_no_limit<- filter(val_df, temp_var == temp_vars[[j]])
  # Create list of three dataframes
  val_df_list<- list()
  # Loop over each option: min, max and mean
  for(i in seq_along(knots)){
    val_df_list[[i]] <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_stations_df,
                                             years = 2012,
                                             months = 1:12,
                                             formula =paste0("temp_",temp_vars[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday,k=",knots[[i]],")  +
                                                                 s(sum_irradiance, yday, k=",knots[[i]],") +
                                                                 s(tpi,yday, k=",knots[[i]],") + 
                                                                 s(asp, yday, k=",knots[[i]],") +
                                                                 s(ptoc,yday, k=3) +
                                                             week"),
                                             alt_formula =paste0("temp_",temp_vars[[j]]," ~
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
  
  val_knots_df[[j]] <- dplyr::bind_rows(val_no_limit , val_df_list)
}

val_knots_monthly_df <- dplyr::bind_rows(val_knots_df)

#### bind and wrangle data ####
bind_val <- bind_rows(all_years_val_mean,     one_year_val_mean,    
                      all_years_val_max,     one_year_val_max, 
                      all_years_val_min,     one_year_val_min, 
                      all_years_val_min, one_year_val_min,  all_years_val_max, one_year_val_max, 
                      val_knots_monthly_df, val_knots_weekly_df, val_knots_daily_df)
# change labels
bind_val <- bind_val %>%
  mutate(temp_var_2 = if_else(temp_var == "max","Max", temp_var),
         temp_var_2 = if_else(temp_var == "min","Min", temp_var_2),
         temp_var_2 = if_else(temp_var == "mean","Mean", temp_var_2))

bind_val <- bind_val %>%
  mutate(timeframe_2 = if_else(timeframe == "all years","All Years", timeframe),
         timeframe_2 = if_else(timeframe == "yearly","Yearly", timeframe_2),
         timeframe_2 = if_else(timeframe == "monthly","Monthly", timeframe_2),
         timeframe_2 = if_else(timeframe == "weekly","Weekly", timeframe_2),
         timeframe_2 = if_else(timeframe == "daily","Daily", timeframe_2),
         timeframe_2 = if_else(timeframe =="weekly_knots","Weekly w/Knots", timeframe_2))
bind_val$timeframe_2 <- factor(bind_val$timeframe_2, 
                               levels = c("All Years", "Yearly", "Monthly",
                                          "Weekly","Daily","Weekly w/Knots"))

scores_df <- filter(bind_val, between(month, 4, 11))

# plot pvals of terms###
dem_pvals <- bind_cols(select(all_years_val, `s(dem,month)`),
                       select(one_year_val, `s(dem,month)`),
                       select(monthly_val, `s(dem,week)`))

##### east and north####
ggplot(data = all_years_val) +
  geom_density(aes(x =   `s(dem,month)`, colour = timeframe)) +
  geom_density(aes(x = `s(dem,month)`, colour = timeframe))+
  geom_density(data = one_year_val,
              aes(x =  `s(dem,week)`, colour = timeframe))+
  geom_density(data = monthly_val,
              aes(x =  `s(dem,yday)`, colour = timeframe)) +
  geom_density(data = weekly_val,
              aes(x = `s(dem,yday)`, colour = timeframe)) +
  geom_density(data = daily_val,
              aes(x = `s(dem)`, colour = timeframe))

ggplot(data = all_years_val) +
  geom_smooth(aes(x = yday, y = `s(dem,month)`, colour = timeframe)) +
  geom_smooth(aes(x = yday, y  = `s(dem,month)`, colour = timeframe))+
  geom_smooth(data = one_year_val,
             aes(x = yday, y = `s(dem,week)`, colour = timeframe))+
  geom_smooth(data = monthly_val,
             aes(x = yday, y = `s(dem,yday)`, colour = timeframe)) +
  geom_smooth(data = weekly_val,
             aes(x = yday, y = `s(dem,yday)`, colour = timeframe)) +
  geom_smooth(data = daily_val,
             aes(x = yday, y = `s(dem)`, colour = timeframe)) +
  scale_y_continuous(limits = c(0,1))


ggplot(data = all_years_val) +
  geom_histogram(aes(x = `s(dem,month)`))
ggplot(data = monthly_val) +
  geom_histogram(aes(x = `s(dem,yday)`))
ggplot(data = weekly_val) +
  geom_histogram(aes(x = `s(dem,yday)`))
ggplot(data = daily_val) +
  geom_histogram(aes(x = `s(dem)`))



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
