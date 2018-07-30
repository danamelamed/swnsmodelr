#' Provides validation data and statistics on monthly GAMs
#' @export
validate_monthly_GAMs <-  function(model_stations_df,
                                  val_stations_df,
                                  years, # list of years ex: 2012:2017
                                  months, # list of months ex: 1:52
                                  formula,
                                  alt_formula ,
                                  verbose = FALSE
){
  
  # Add date fields to dataframe
  model_stations_df <- add_date_columns(model_stations_df)
  val_stations_df   <- add_date_columns(val_stations_df)
  
  # Select years and months to model
  
  
  # List of annual dataframes
  monthly_res_years <- list()
  alt_formula_weeks <- list()
  # Loop through years
  for(j in seq_along(years)){
    
    # List of monthly
    monthly_res <-list()
    for(i in seq_along(months)){
      
      # monthly model df
      monthly_df <- model_stations_df %>%
        filter(month == months[[i]], year == years[[j]])
      
      # monthly validation df
      monthly_val_df <- val_stations_df %>%
        filter(month == months[[i]], year == years[[j]])
      
      
      # Model
      # The model will fail if insufficient sum_irradiance has been extracted
      # The logic below will model without sum_irradiance in that case
      
      m <-try( gam(formula(formula), data = monthly_df,  na.action = na.omit))
      
      if("try-error" %in% class(m)){
        m <- gam(formula(alt_formula), data = monthly_df, na.action = na.omit)
        alt_formula_weeks[[length(alt_formula_weeks)+1]] <- paste(years[[j]],months[[i]],sep = "-")
        print(alt_formula_weeks)
      }
      
      if(verbose == TRUE){
        print(m)
      }
      
      # Store stats
      monthly_res[[i]] <- modelr::add_residuals(data = monthly_val_df, model = m)
      monthly_res[[i]]$gcv <- m$gcv.ubre.dev
      monthly_res[[i]]$rsq <- summary(m)[[10]]
      monthly_res[[i]]$dev <- summary(m)[[14]]
      monthly_res[[i]]$abs_resid <- abs(monthly_res[[i]]$resid)
      
      for(l in seq_along(summary(m)[[7]])){
        monthly_res[[i]]$var_pval <- summary(m)[[8]][[l]]
        names(monthly_res[[i]])[names(monthly_res[[i]]) == "var_pval"] <- names(summary(m)[[7]])[[l]]
      }
      
      if(is.na(monthly_res[[i]]$resid)){
        # Store stats
        monthly_res[[i]] <- modelr::add_residuals(data = monthly_val_df, model = m)
        monthly_res[[i]]$gcv <- m$gcv.ubre.dev
        monthly_res[[i]]$rsq <- summary(m)[[10]]
        monthly_res[[i]]$dev <- summary(m)[[14]]
        monthly_res[[i]]$abs_resid <- abs(monthly_res[[i]]$resid)
        
        for(l in seq_along(summary(m)[[7]])){
          monthly_res[[i]]$var_pval <- summary(m)[[8]][[l]]
          names(monthly_res[[i]])[names(monthly_res[[i]]) == "var_pval"] <- names(summary(m)[[7]])[[l]]
        }
      }
    
    }
    
    # Bind monthly dataframes into annual
    monthly_res_years[[j]] <- dplyr::bind_rows(monthly_res)
  }
  
  # Bind annual dataframes into final dataframe
  monthly_res_df <- dplyr::bind_rows(monthly_res_years)
  
  return(monthly_res_df)
}
