# Testing models with different allowed knots

##### All years timeframe #####
knots <- list(1,5,9)
all_years_knots <- list()
all_years_val_knots <- list()
for(i in seq_along(knots)){
  all_years_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                       s(dem,month, k= ",knots,") +
                                       s(ptoc,month, k= 3) +
                                       s(sum_irradiance, month) +
                                       s(tpi,month, k= ",knots,") +
                                       s(asp, month, k= ",knots,") +
                                       s(east,north, k= ",knots,") +
                                       s(week, k= ",knots,") +
                                       year")), 
                        data = model_stations_df)
  all_years_val_knots[[i]] <- add_residuals(val_df_2012, all_years_knots[[i]])
  
  
  if(is.na(all_years_val_knots[[i]]$resid)){
    all_years_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                         s(dem,month, k= ",knots,") +
                                         s(ptoc,month, k= 3) +
                                         s(tpi,month, k= ",knots,")+
                                         s(asp, month, k= ",knots,") +
                                         s(east,north, k= ",knots,") +
                                         s(week, k= ",knots,") +
                                         year")), 
                          data = model_stations_df)
    all_years_val_knots[[i]] <- add_residuals(val_df_2012, all_years_knots[[i]])
  }
  all_years_val_knots[[i]]$timeframe <- "All years"
  all_years_val_knots[[i]]$knots <- knots[[i]]
  all_years_val_knots[[i]]$temp_var <- temp_var[[i]]
  all_years_val_knots[[i]]$gcv <- all_years_knots[[i]]$gcv.ubre
  all_years_val_knots[[i]]$rsq <- summary(all_years_knots[[i]])[[10]]
  all_years_val_knots[[i]]$dev <- summary(all_years_knots[[i]])[[14]]
  all_years_val_knots[[i]]$abs_resid <- abs(all_years_val_knots[[i]]$resid)
  for(l in seq_along(summary(all_years_knots[[i]])[[7]])){
    all_years_val_knots[[i]]$var_pval <- summary(all_years_knots[[i]])[[8]][[l]]
    names(all_years_val_knots[[i]])[names(all_years_val_knots[[i]]) == "var_pval"] <- names(summary(all_years_knots[[i]])[[7]])[[l]]
  }
}
all_years_val_knots_df <- bind_rows(all_years_val_knots) 


##### Annual timeframe ####
annual_knots <- list()
annual_val_knots <- list()
for(i in seq_along(knots)){      
  annual_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                    s(dem,week, k= ",knots,") +
                                    s(ptoc,week, k= 3)+
                                    s(sum_irradiance, week, k= ",knots,") +
                                    s(tpi,week, k= ",knots,")+
                                    s(asp, week, k= ",knots,") +
                                    s(east,week, k= ",knots,") +
                                    s(yday, k= ",knots,") +
                                    month")), 
                       data = model_df_2012)
  annual_val_knots[[i]] <- add_residuals(val_df_2012, annual_knots[[i]])

  
  if(is.na(annual_val_knots[[i]]$resid)){
    annual_knots[[i]] <- gam(formula(paste0("temp_mean ~
                                      s(dem,week, k= ",knots,") +
                                      s(ptoc,week, k= 3)+
                                      s(tpi,week, k= ",knots,")+
                                      s(asp, week, k= ",knots,") +
                                      s(east,week, k= ",knots,") +
                                      s(yday, k= ",knots,") +
                                      month")), 
                         data = model_df_2012)
    annual_val_knots[[i]] <- add_residuals(val_df_2012, annual_knots[[i]])
    }
    for(l in seq_along(summary(annual_knots[[i]])[[7]])){
      annual_val_knots[[i]]$var_pval <- summary(annual_knots[[i]])[[8]][[l]]
      names(annual_val_knots[[i]])[names(annual_val_knots[[i]]) == "var_pval"] <- names(summary(annual_knots[[i]])[[7]])[[l]]
    }
    annual_val_knots[[i]]$timeframe <- "Annual"
    annual_val_knots[[i]]$temp_var <- temp_var[[i]]
    annual_val_knots[[i]]$gcv <- annual_knots[[i]]$gcv.ubre
    annual_val_knots[[i]]$rsq <- summary(annual_knots[[i]])[[10]]
    annual_val_knots[[i]]$knots <- knots[[i]]
    annual_val_knots[[i]]$dev <- summary(annual_knots[[i]])[[14]]
    annual_val_knots[[i]]$abs_resid <- abs(annual_val_knots[[i]]$resid)
}


