extract_monthly_spvs <- function(df,
                             dep_vars,
                             ind_vars,
                             years,
                             months){
  # get only specified year of data
  df <- dplyr::filter(df, years == year)
  df_month_bind <- list()
  for(i in seq_along(months)){
    
    df_month <- dplyr::filter(df, month == months[[i]])
    
    dep_var_df <- list()
    for(j in seq_along(dep_vars)){
      
      ind_var_df <- list()
      for(k in seq_along(ind_vars)){
        ind_var_df[[k]] <- df_month
        if(ind_vars[[k]] == "east,north"){
          formula = paste0(dep_vars[[j]],"~ s(east,north) + week")
          ind_var_df[[k]]$ind_var <- "(E,N)"
        }else{
          formula <- paste0(dep_vars[[j]],"~ s(",
                            ind_vars[[k]], ", yday) + week")
          ind_var_df[[k]]$ind_var <- ind_vars[[k]]
        }
        
        f <- as.formula(formula)
        
        m <- gam(f, data = df_month)
        
        
        
        ind_var_df[[k]]$pval <- summary.gam(m)$s.pv
        ind_var_df[[k]]$dep_var <- dep_vars[[j]]
      }
      dep_var_df[[j]] <- dplyr::bind_rows(ind_var_df)
    }
    df_month_bind[[i]] <- dplyr::bind_rows(dep_var_df)
  }
  df_out <- dplyr::bind_rows(df_month_bind)
}



