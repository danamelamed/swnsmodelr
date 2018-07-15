#' Provides validation data and statistics on weekly GAMs
#' @export
validate_weekly_GAMs <-  function(model_stations_df,
                                  val_stations_df,
                                  years, # list of years ex: 2012:2017
                                  weeks, # list of weeks ex: 1:52
                                  formula,
                                  alt_formula,
                                  verbose = FALSE
                                  ){
  
  # Add date fields to dataframe
  model_stations_df <- add_date_columns(model_stations_df)
  val_stations_df   <- add_date_columns(val_stations_df)
  
  # Select years and weeks to model
  
  
  # List of annual dataframes
  weekly_res_years <- list()
  alt_formula_weeks <- list()
  # Loop through years
  for(j in seq_along(years)){
    
    # List of weekly
    weekly_res <-list()
    for(i in seq_along(weeks)){
      
      # Weekly model df
      weekly_df <- model_stations_df %>%
        filter(week == weeks[[i]], year == years[[j]])
      
      # Weekly validation df
      weekly_val_df <- val_stations_df %>%
        filter(week == weeks[[i]], year == years[[j]])
      
      
      # Model
      # The model will fail if insufficient sum_irradiance has been extracted
      # The logic below will model without sum_irradiance in that case
      
      m <-try( gam(formula(formula), data = weekly_df,  na.action = na.omit))
   
      if("try-error" %in% class(m)){
       m <- gam(formula(alt_formula), data = weekly_df, na.action = na.omit)
        alt_formula_weeks[[length(alt_formula_weeks)+1]] <- paste(years[[j]],weeks[[i]],sep = "-")
      } 
      
      if(verbose == TRUE){
        print(m)
      }
      
      # Store stats
      weekly_res[[i]] <- modelr::add_residuals(data = weekly_val_df, model = m)
      weekly_res[[i]]$gcv <- m$gcv.ubre.dev
      weekly_res[[i]]$rsq <- summary(m)[[10]]
      weekly_res[[i]]$dev <- summary(m)[[14]]
      weekly_res[[i]]$abs_resid <- abs(weekly_res[[i]]$resid)
      for(l in seq_along(summary(m)[[7]])){
        weekly_res[[i]]$var_pval <- summary(m)[[8]][[l]]
        names(weekly_res[[i]])[names(weekly_res[[i]]) == "var_pval"] <- names(summary(m)[[7]])[[l]]
      }
    }
    
    # Bind weekly dataframes into annual
    weekly_res_years[[j]] <- dplyr::bind_rows(weekly_res)
  }
  
  # Bind annual dataframes into final dataframe
  weekly_res_df <- dplyr::bind_rows(weekly_res_years)
  
  return(weekly_res_df)
}
