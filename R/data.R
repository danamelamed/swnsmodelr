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

#' Temperatures and variables at 700m resolution
#' @format dataframe
#' \describe{
#'     \item{X}{unique number for each record}
#'     \item{stationid}{stationid identification code}
#'     \item{date_time}{date of temperature reading}
#'     \item{temp_min}{maximum daily temperature (C)}
#'     \item{temp_mean}{average daily temperature (C)}
#'     \item{temp_min}{minimum daily temperature (C)}
#'     \item{EASTING}{easting value of station in UTM zone 20}
#'     \item{NORTHING}{northing value of station in UTM zone 20}
#'     \item{dem}{value extracted from dem, elevation in metres}
#'     \item{tpi}{value extracted from tpi raster, topographic position index, ranges 0 - 1}
#'     \item{asp_c}{aspect with abs(x - 180) applied to it}
#'     \item{east}{value extracted from easting raster}
#'     \item{north}{value extracted from northing raster}
#'     \item{ptoc}{value extracted from proxmity to coast raster}
#'     \item{sum_irradiance}{value extracted from series of daily GOES rasters}}
"model_df_700"



