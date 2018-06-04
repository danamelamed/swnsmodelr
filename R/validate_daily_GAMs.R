#' Provides validation data and statistics on daily GAMs
#' @export
validate_daily_GAMs <-  function(model_stations_df,
                                   val_stations_df,
                                   years, # list of years ex: 2012:2017
                                   days, # list of days ex: 1:52
                                   formula,
                                   alt_formula ,
                                   verbose = FALSE
){
  
  # Add date fields to dataframe
  model_stations_df <- add_date_columns(model_stations_df)
  val_stations_df   <- add_date_columns(val_stations_df)
  
  # Select years and days to model
  
  
  # List of annual dataframes
  daily_res_years <- list()

  # Loop through years
  for(j in seq_along(years)){
    
    # List of daily
    daily_res <-list()
    for(i in seq_along(days)){
      
      # daily model df
      daily_df <- model_stations_df %>%
        filter(yday == days[[i]], year == years[[j]])
      
      # daily validation df
      daily_val_df <- val_stations_df %>%
        filter(yday == days[[i]], year == years[[j]])
      
      
      # Model
      # The model will fail if insufficient sum_irradiance has been extracted
      # The logic below will model without sum_irradiance in that case
      
      m <-try( gam(formula(formula), data = daily_df,  na.action = na.omit))
      
      if("try-error" %in% class(m)){
        m <- gam(formula(alt_formula), data = daily_df, na.action = na.omit)
        print("oops")
     
      }
      
      if(verbose == TRUE){
        print(m)
      }
      
      # Store stats
      daily_res[[i]] <- modelr::add_residuals(data = daily_val_df, model = m)
      daily_res[[i]]$gcv <- m$gcv.ubre.dev
      daily_res[[i]]$rsq <- summary(m)[[10]]
      daily_res[[i]]$dev <- summary(m)[[14]]
      daily_res[[i]]$abs_resid <- abs(daily_res[[i]]$resid)
      print(summary(m))
      # get pvalues for each term
      for(l in seq_along(summary(m)[[7]])){
        daily_res[[i]]$var_pval <- summary(m)[[8]][[l]]
        names(daily_res[[i]])[names(daily_res[[i]]) == "var_pval"] <- names(summary(m)[[7]])[[l]]
      }
    }
    
    # Bind daily dataframes into annual
    daily_res_years[[j]] <- dplyr::bind_rows(daily_res)
  }
  
  # Bind annual dataframes into final dataframe
  daily_res_df <- dplyr::bind_rows(daily_res_years)
  
  return(daily_res_df)
}
