# Test multiple time scales for modelling

# Use all data at once
m1 <- gam(temp_mean ~
            year +
            s(yday) + 
            s(dem, week) +
            s(ptoc, week) +
            s(tpi, week) +
            s(asp_c, week) +
            s(east, north),
          data = swns_stations_df_200)


# how about daily?

df <- swns_stations_df_200 %>% 
  filter(year == 2012 & yday %in% c(122)) 

# 
# c(122, 183, 45, 306)
ggplot(data = df,
       mapping = aes(x = dem, y = temp_mean))+
  geom_point() +
  geom_smooth() +
  facet_wrap(~yday)


m <- gam(temp_mean ~ s(dem, yday) + year + month, data = swns_stations_df_200)
m <- gam(temp_mean ~ s(asp_c, yday) + year + month, data = swns_stations_df_200)
m <- gam(temp_mean)


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

weekly_val_knots <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                   val_stations_df = val_2012,
                                   years = 2012,
                                   weeks = 1:52,
                                   formula = "temp_mean ~
                                                  s(dem,yday, k= 9) +
                                                  s(ptoc,yday, k= 3) +
                                                  s(sum_irradiance, yday, k= 9) +
                                                  s(tpi,yday, k= 9) +
                                                  s(asp_c, yday, k= 9) +
                                                  s(east,north, k= 9)",
                                   alt_formula = "temp_mean ~
                                                  s(dem,yday, k= 9) +
                                                  s(ptoc,yday, k= 3) +
                                                  s(tpi,yday, k= 9) +
                                                  s(asp_c, yday, k= 9) +
                                                  s(east,north, k= 9)")

weekly_val_knots$timeframe <- "weekly_knots"
weekly_val_knots$temp_var   <- "mean"


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

# ggplot(mapping = aes(x = temp_mean, y = resid)) +
#  # geom_point(data = all_years_val, colour = "red") +
#   #geom_point(data = one_year_val, colour = "black") +
#   #geom_point(data = monthly_val, colour = "blue") +
#   geom_point(data = weekly_val, colour  = "orange") +
#   geom_point(data = daily_val, colour  = "green") +
#   geom_point(data = weekly_val_knots, colour = "purple")
# 
# ggplot(mapping = aes(x = resid)) +
#   geom_density(data = all_years_val, colour = "red") +
#   geom_density(data = one_year_val, colour = "black") +
#   geom_density(data = monthly_val, colour = "blue") +
#   geom_density(data = weekly_val, colour  = "orange") +
#  geom_density(data = daily_val, colour  = "green")
# 
# ggplot(data = one_year_val, aes(x = temp_mean, y = resid)) +
#   geom_smooth()
# ggplot(data = monthly_val, aes(x = temp_mean, y = resid)) +
#   geom_smooth()


##### repeat with min 
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

# repeat with max
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

weekly_val_knots_max <- validate_weekly_GAMs(model_stations_df = model_stations_df,
                                             val_stations_df = val_2012,
                                             years = 2012,
                                             weeks = 1:52,
                                             formula = "temp_max ~
                                             s(dem,yday, k= 9) +
                                             s(ptoc,yday, k= 3) +
                                             s(sum_irradiance, yday, k= 9) +
                                             s(tpi,yday, k= 9) +
                                             s(asp_c, yday, k= 9) +
                                             s(east,north, k= 9)",
                                         alt_formula = "temp_max ~
                                         s(dem,yday, k= 9) +
                                         s(ptoc,yday, k= 3) +
                                         s(tpi,yday, k= 9) +
                                         s(asp_c, yday, k= 9) +
                                         s(east,north, k= 9)")
weekly_val_knots_max$timeframe <- "weekly_knots"
weekly_val_knots_max$temp_var  <- "max"



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
                                 s(asp_c) +
                                 s(east,north)")
daily_val_max$timeframe <- "daily"
daily_val_max$temp_var  <- "max"

bind_val <- bind_rows(all_years_val,     one_year_val,     monthly_val,     weekly_val,     daily_val,
                      all_years_val_min, one_year_val_min, monthly_val_min, weekly_val_min, daily_val_min,
                      all_years_val_max, one_year_val_max, monthly_val_max, weekly_val_max, daily_val_max)#,
                    #  weekly_val_knots, weekly_val_knots_max, weekly_val_knots_min)

# arrange by 

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


# give number to timeframe to order
bind_val$timeframe <- factor(bind_val$timeframe, levels = c("all years",
                                                            "yearly",
                                                            "monthly",
                                                            "weekly",
                                                            "daily"))
bind_val$timeframe <- as.character(bind_val$timeframe)
bind_val$timeframe <- factor(bind_val$timeframe, 
                             levels = c("all years", "yearly", "monthly",
                                        "weekly","daily"))

bind_val$timeframe_2 <- factor(bind_val$timeframe_2, 
                             levels = c("All Years", "Yearly", "Monthly",
                                        "Weekly","Daily","Weekly w/Knots"))

df <- bind_val %>% filter(temp_var == "max")



ggplot(data = bind_val, aes(x = resid)) +
  geom_freqpoly(aes(colour = temp_var), size = 1) +
  labs(x = "Residuals", y ="Density") +
  facet_wrap(~timeframe)

ggplot(data = bind_val, aes(x = temp_var, y= resid)) +
  geom_violin(aes(colour = temp_var), shape = 1) +
  labs(x = "temp_mean",y = "Residuals") +
  facet_wrap(~timeframe, scale = "free")

# try bar chart things
# GOOD
ggplot(bind_val, aes(resid, fill = temp_var_2))+
  labs(x = "Residuals",y = "Count") +
  geom_histogram(position = "dodge",  colour = "black", bins = 15)+
  scale_fill_manual(values = c("brown1","navajowhite2","steelblue2")) +
  guides(fill = guide_legend(title = "Daily Temperature Variable")) +
  facet_wrap(~timeframe_2, ncol = 2)

df <- bind_val %>% filter(stationid %in% c("AN3","YA4","6516","6383","DNR016","KE5"))
ggplot(bind_val, aes(x = temp_mean, y = abs_resid, colour = temp_var_2))+
  labs(x = "Residuals",y = "Count") +
  geom_smooth()+
  scale_colour_manual(values = c("brown1","navajowhite2","steelblue2")) +
  guides(fill = guide_legend(title = "Daily Temperature Variable")) +
  facet_wrap(~timeframe_2, ncol = 2)

ggplot(bind_val, aes(x = yday, y = gcv, colour = temp_var_2))+
  labs(x = "Residuals",y = "Count") +
  geom_smooth()+
  scale_colour_manual(values = c("brown1","navajowhite2","steelblue2")) +
  guides(fill = guide_legend(title = "Daily Temperature Variable")) +
  facet_wrap(~timeframe_2, ncol = 2)




ggplot(data = df, aes(x = resid)) +
  geom_density(aes(color = timeframe), size = 1) +
  scale_colour_manual(values = c("brown1",
                                 "goldenrod3",
                                 "deepskyblue3",
                                 "orchid2",
                                 "green4")) +
  labs(x = "Residuals", y ="Density")



ggplot(bind_val, aes(x = yday, y = resid, colour = temp_var_2)) +
  geom_point() +
  facet_wrap(~timeframe_2, ncol = 2)

# residuals by temperature mean
ggplot(data = df, aes(x = resid )) +
  geom_density(aes(color = timeframe), size = 1) +
  facet_wrap(~temp_var) +
  scale_colour_manual(values = c("salmon",
                                 "forestgreen",
                                 "black",
                                 "magenta2",
                                 "deepskyblue3")) +
  scale_x_continuous(limits = c(-10,10))


ggplot(data = bind_val, aes(x = temp_mean , y=resid)) +
  geom_point(aes(color = timeframe), shape = 1) +
  facet_wrap(~temp_var_2) +
  scale_colour_manual(values = c("salmon",
                                 "forestgreen",
                                 "black",
                                 "magenta2",
                                 "deepskyblue3")) +
  scale_x_continuous(limits = c(-10,10))




