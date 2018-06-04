# Testing time frames

#### mean ##### 
df <- model_stations_df %>% filter(year == 2012)
val_2012 <- val_stations_df %>% filter(year == 2012)

all_years <- gam(temp_mean ~
                   s(dem,month) +
                   s(ptoc,month) +
                   s(sum_irradiance, month) +
                   s(tpi,month)+
                   s(asp_c, month) +
                   s(east,north) +
                   s(week) +
                   year, 
                 data = swns_stations_df_200)
all_years_val <- add_residuals(val_2012, all_years)
all_years_val$timeframe <- "all years"
all_years_val$temp_var <- "mean"
all_years_val$gcv <- all_years$gcv.ubre
all_years_val$rsq <- summary(all_years)[[10]]
all_years_val$dev <- summary(all_years)[[14]]
all_years_val$abs_resid <- abs(all_years_val$resid)

one_year <- gam(temp_mean ~
                  s(dem,week) +
                  s(ptoc,week)+
                  s(sum_irradiance, week) +
                  s(tpi,week)+
                  s(asp_c, week) +
                  s(east,week) +
                  s(yday) +
                  month, 
                data = df)
one_year_val <- add_residuals(val_2012, one_year)
one_year_val$timeframe <- "yearly"
one_year_val$temp_var <- "mean"
one_year_val$gcv <- one_year$gcv.ubre
one_year_val$rsq <- summary(one_year)[[10]]
one_year_val$dev <- summary(one_year)[[14]]
one_year_val$abs_resid <- abs(one_year_val$resid)


monthly_val <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                     val_stations_df = val_2012,
                                     years = 2012,
                                     months = 1:12,
                                     formula = "temp_mean ~
                                     s(dem,yday) +
                                     s(ptoc,yday) +
                                     s(sum_irradiance, yday) +
                                     s(tpi,yday) +
                                     s(asp_c, yday) +
                                     s(east,north) +
                                     week",
                                     alt_formula = "temp_mean ~
                                     s(dem,yday) +
                                     s(ptoc,yday) +
                                     s(tpi,yday) +
                                     s(asp_c, yday) +
                                     s(east,north) +
                                     week")
monthly_val$timeframe <- "monthly"
monthly_val$temp_var   <- "mean"

weekly_val <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                   val_stations_df = val_2012,
                                   years = 2012,
                                   weeks = 1:52,
                                   formula = "temp_mean ~
                                   s(dem,yday) +
                                   s(ptoc,yday) +
                                   s(sum_irradiance, yday) +
                                   s(tpi,yday) +
                                   s(asp_c, yday) +
                                   s(east,north)",
                                   alt_formula = "temp_mean ~
                                   s(dem,yday) +
                                   s(ptoc,yday) +
                                   s(tpi,yday) +
                                   s(asp_c, yday) +
                                   s(east,north)")

weekly_val$timeframe <- "weekly"
weekly_val$temp_var   <- "mean"


daily_val <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                 val_stations_df = val_2012,
                                 years = 2012,
                                 days = 1:365,
                                 formula = "temp_mean ~
                                 s(dem) +
                                 s(ptoc) +
                                 s(sum_irradiance) +
                                 #s(tpi) +
                                 #s(asp_c) +
                                 s(east,north)",
                                 alt_formula = "temp_mean ~
                                 s(dem) +
                                 s(ptoc) +
                                 # s(tpi) +
                                 s(asp_c) +
                                 s(east,north)")

daily_val$timeframe <- "daily"
daily_val$temp_var   <- "mean"
#### min ####
all_years_min <- gam(temp_min ~
                       s(dem,month) +
                       s(ptoc,month) +
                       s(sum_irradiance, month) +
                       s(tpi,month)+
                       s(asp_c, month) +
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
                      s(ptoc,week)+
                      s(sum_irradiance, week) +
                      s(tpi,week)+
                      s(asp_c, week) +
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

monthly_val_min <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                         val_stations_df = val_2012,
                                         years = 2012,
                                         months = 1:12,
                                         formula = "temp_min ~
                                         s(dem,yday) +
                                         s(ptoc,yday) +
                                         s(sum_irradiance, yday) +
                                         s(tpi,yday) +
                                         s(asp_c, yday) +
                                         s(east,north) +
                                         week",
                                         alt_formula = "temp_min ~
                                         s(dem,yday) +
                                         s(ptoc,yday) +
                                         s(tpi,yday) +
                                         s(asp_c, yday) +
                                         s(east,north) +
                                         week")
monthly_val_min$timeframe <- "monthly"
monthly_val_min$temp_var <- "min"

weekly_val_min <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                       val_stations_df = val_2012,
                                       years = 2012,
                                       weeks = 1:52,
                                       formula = "temp_min ~
                                       s(dem,yday) +
                                       s(ptoc,yday) +
                                       s(sum_irradiance, yday) +
                                       s(tpi,yday) +
                                       s(asp_c, yday) +
                                       s(east,north)",
                                       alt_formula = "temp_min ~
                                       s(dem,yday) +
                                       s(ptoc,yday) +
                                       s(tpi,yday) +
                                       s(asp_c, yday) +
                                       s(east,north)")

weekly_val_min$timeframe <- "weekly"
weekly_val_min$temp_var <- "min"

weekly_val_knots_min <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_2012,
                                             years = 2012,
                                             weeks = 1:52,
                                             formula = "temp_min ~
                                         s(dem,yday, k= 9) +
                                         s(ptoc,yday, k= 3) +
                                         s(sum_irradiance, yday, k= 9) +
                                         s(tpi,yday, k= 9) +
                                         s(asp_c, yday, k= 9) +
                                         s(east,north, k= 9)",
                                             alt_formula = "temp_min ~
                                         s(dem,yday, k= 9) +
                                         s(ptoc,yday, k= 3) +
                                         s(tpi,yday, k= 9) +
                                         s(asp_c, yday, k= 9) +
                                         s(east,north, k= 9)")
weekly_val_knots_min$timeframe <- "weekly_knots"
weekly_val_knots_min$temp_var  <- "min"



daily_val_min <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                     val_stations_df = val_2012,
                                     years = 2012,
                                     days = 1:365,
                                     formula = "temp_min ~
                                 s(dem) +
                                 s(ptoc) +
                                 s(sum_irradiance) +
                                 #s(tpi) +
                                 #s(asp_c) +
                                 s(east,north)",
                                     alt_formula = "temp_min ~
                                 s(dem) +
                                 s(ptoc) +
                                 # s(tpi) +
                                 s(asp_c) +
                                 s(east,north)")
daily_val_min$timeframe <- "daily"
daily_val_min$temp_var  <- "min"
#### max ####
all_years_max <- gam(temp_max ~
                       s(dem,month) +
                       s(ptoc,month) +
                       s(sum_irradiance, month) +
                       s(tpi,month)+
                       s(asp_c, month) +
                       s(east,north) +
                       s(week) +
                       year, 
                     data = swns_stations_df_200)
all_years_val_max <- add_residuals(val_2012, all_years_max)
all_years_val_max$gcv <- all_years_max$gcv.ubre
all_years_val_max$rsq <- summary(all_years_max)[[10]]
all_years_val_max$dev <- summary(all_years_max)[[14]]
all_years_val_max$abs_resid <- abs(all_years_val_max$resid)




all_years_val_max$timeframe <- "all years"
all_years_val_max$temp_var <- "max"

one_year_max <- gam(temp_max ~
                      s(dem,week) +
                      s(ptoc,week)+
                      s(sum_irradiance, week) +
                      s(tpi,week)+
                      s(asp_c, week) +
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

monthly_val_max <- validate_monthly_GAMs(model_stations_df = model_stations_df,
                                         val_stations_df = val_2012,
                                         years = 2012,
                                         months = 1:12,
                                         formula = "temp_max ~
                                         s(dem,yday) +
                                         s(ptoc,yday) +
                                         s(sum_irradiance, yday) +
                                         s(tpi,yday) +
                                         s(asp_c, yday) +
                                         s(east,north) +
                                         week",
                                         alt_formula = "temp_max ~
                                         s(dem,yday) +
                                         s(ptoc,yday) +
                                         s(tpi,yday) +
                                         s(asp_c, yday) +
                                         s(east,north) +
                                         week")
monthly_val_max$timeframe <- "monthly"
monthly_val_max$temp_var <- "max"

weekly_val_max <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                       val_stations_df = val_2012,
                                       years = 2012,
                                       weeks = 1:52,
                                       formula = "temp_max ~
                                       s(dem,yday) +
                                       s(ptoc,yday) +
                                       s(sum_irradiance, yday) +
                                       s(tpi,yday) +
                                       s(asp_c, yday) +
                                       s(east,north)",
                                       alt_formula = "temp_max ~
                                       s(dem,yday) +
                                       s(ptoc,yday) +
                                       s(tpi,yday) +
                                       s(asp_c, yday) +
                                       s(east,north)")

weekly_val_max$timeframe <- "weekly"
weekly_val_max$temp_var <- "max"



daily_val_max <- validate_daily_GAMs(model_stations_df = model_stations_df,
                                     val_stations_df = val_2012,
                                     years = 2012,
                                     days = 1:365,
                                     formula = "temp_max ~
                                 s(dem) +
                                 s(ptoc) +
                                 s(sum_irradiance) +
                                 #s(tpi) +
                                 #s(asp_c) +
                                 s(east,north)",
                                     alt_formula = "temp_max ~
                                 s(dem) +
                                 s(ptoc) +
                                 # s(tpi) +
                                 #s(asp_c) +
                                 s(east,north)")
daily_val_max$timeframe <- "daily"
daily_val_max$temp_var  <- "max"


# now with knots...
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
                                                         s(asp_c, yday) +
                                                         s(tpi, yday) + 
                                                         s(ptoc,yday)"),
                                           alt_formula =paste0("temp_",temp_var[[i]],"~
                                                         s(east,north) +
                                                         s(dem, yday)  +
                                                         s(asp_c, yday) +
                                                         s(tpi, yday) + 
                                                         s(ptoc,yday)")
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
                                                                 s(asp_c, yday, k=",knots[[i]],") +
                                                                 s(tpi, yday, k=",knots[[i]],") + 
                                                                 s(ptoc,yday, k=3)"),
                                             alt_formula =paste0("temp_",temp_vars[[j]]," ~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday, k=",knots[[i]],")  +
                                                                 s(asp_c, yday, k=",knots[[i]],") +
                                                                 s(tpi, yday, k=",knots[[i]],") + 
                                                                 s(ptoc,yday, k=3)")
    ) 
    val_df_list[[i]]$knots <- as.character(knots[[i]])
    val_df_list[[i]]$temp_var <- temp_var[[j]]
  }
  
  val_knots_df[[j]] <- dplyr::bind_rows(val_no_limit , val_df_list)
}

val_knots_df <- dplyr::bind_rows(val_knots_df)

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
                                                          #s(tpi) + 
                                                          s(ptoc, k = 3)"),
                                          alt_formula =paste0("temp_",temp_var[[i]],"~
                                                              s(east,north) +
                                                              s(dem)  +
                                                              #s(tpi) + 
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
                                                           s(asp_c, yday) +
                                                           s(ptoc, k = 3) +
                                                           week"),
                                           alt_formula =paste0("temp_",temp_var[[i]],"~
                                                               s(east,north, yday) +
                                                               s(dem, yday)  +
                                                               s(tpi, yday) + 
                                                               s(asp_c, yday)
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
                                             months= 1:12,
                                             formula =paste0("temp_",temp_vars[[j]],"~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday,k=",knots[[i]],")  +
                                                                 s(sum_irradiance, yday, k=",knots[[i]],") +
                                                                 s(tpi,yday, k=",knots[[i]],") + 
                                                                 s(asp_c, yday, k=",knots[[i]],") +
                                                                 s(ptoc,yday, k=3) +
                                                             week"),
                                             alt_formula =paste0("temp_",temp_vars[[j]]," ~
                                                                 s(east,north, k=",knots[[i]],") +
                                                                 s(dem, yday,k=",knots[[i]],")  +
                                                                 s(asp_c, yday, k=",knots[[i]],") +
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
bind_val <- bind_rows(all_years_val,     one_year_val,     monthly_val,     weekly_val,     daily_val,
                      all_years_val_min, one_year_val_min, monthly_val_min, weekly_val_min, daily_val_min,
                      all_years_val_max, one_year_val_max, monthly_val_max, weekly_val_max, daily_val_max,
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
