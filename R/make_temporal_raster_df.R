#' Make dataframe for temporal rasters path and datetime
#' 
#' @param  in_folder The folder location of a set of rasters
#' @param  start_date Earliest date to include in output
#' @param  end_date Last date to include in output 
#' @param  date_chars The characters in the file names that define the date
#' @param  date_format The date format of the date_chars 
#' @return Dataframe with two columns: path to temporally varying rasters, and the date
#' @export
make_temporal_raster_df <- function(in_folder,
                                    start_date,
                                    end_date, 
                                    date_chars, 
                                    date_format, 
                                    extension = "tif"){
  # Returns a dataframe of raster paths and corresponding dates
  
  # Put files into a dataframe
  out_df <- as.data.frame(paste0(list.files(path = in_folder, 
                                            pattern = paste0(".",
                                                             extension,
                                                             "$"))),
                          stringsAsFactors = FALSE)
  
  # Name the paths column
  names(out_df) <- "path_field"
  
  # Add datetime variable column
  out_df <- dplyr::mutate(out_df,
                   date_time = as.Date(
                     stringr::str_sub(path_field, date_chars[[1]], date_chars[[2]]),
                     format = date_format
                   ))
  
  # Complete paths_field with full path
  out_df <- out_df %>%
                   mutate(path_field = paste(in_folder, path_field, sep = "\\")
                  
  )
  
  # If date field has <NA>, warn user that date characters are wrong
  if(is.na(out_df$date_time[[1]])){
    warning("Date characters or format is incorrrectly specified, check date_time field")
  }
  
  
  # Return temporal raster df
  return(out_df)
}