#' Daily temperatures in SWNS
#' @format A dataframe containing daily temperature readings from weather stations in SWNS
#'         from 2012 - 2017
#' \describe{
#'     \item{stationid}{stationd identification code}
#'     \item{date_time}{date of temperature reading}
#'     \item{temp_min}{maximum daily temperature (C)}
#'     \item{temp_mean}{average daily temperature (C)}
#'     \item{temp_min}{minimum daily temperature (C)}
#'     \item{EASTING}{easting value of station in UTM zone 20}
#'     \item{NORTHING}{northing value of station in UTM zone 20}}
"daily_temperatures_df"

#' Station points in SWNS
#' @format Spatial Points Data Frame
"stations_sp"