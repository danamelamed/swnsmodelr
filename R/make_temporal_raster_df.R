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
  
  # return dataframe of paths and dates for each raster in folder
  
  # list paths from in_folder and put into df
  out_df <- as.data.frame(paste0(list.files(path = in_folder, 
                                            pattern = paste0(".",extension))),
                          stringsAsFactors = FALSE)
  
  # change name of paths column
  names(out_df) <- "paths_field"
  # add date_time column
 
  out_df <- out_df %>%
    mutate(date_time = as.Date(
      str_sub(paths_field, date_chars[[1]], date_chars[[2]]),
      format = date_format),
      paths_field = paste(in_folder, paths_field, sep = "\\")) %>%
    filter(date_time >= start_date &
             date_time <= end_date)
  
  return(out_df)
}