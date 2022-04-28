#' Hurricane Floyd tracks data
#'
#' A dataframe containing hurricane best tracks for Hurricane Floyd in
#' 1999. This dataframe is included for use as an example hurricane
#' tracks dataframe in the package documentation. This data originally
#' came from the Extended Best Track dataset:
#' \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#'
#' @format A dataframe with 48 rows and 4 variables:
#' \describe{
#'     \item{date}{A character string giving the date and time of the observation}
#'     \item{latitude}{A numeric vector giving the storm's latitude at that
#'                     observation time}
#'     \item{longitude}{A numeric vector giving the storm's longitude at that
#'                     observation time}
#'     \item{wind}{A numeric vector giving the estimated maximum sustained
#'                 wind of that storm at that observation time, in knots}
#' }
#'
#' @source
#' \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
"floyd_tracks"

#' Hurricane Katrina tracks data
#'
#' A dataframe containing hurricane best tracks for Hurricane Katrina in
#' 2005. This dataframe is included for use as an example hurricane
#' tracks dataframe in the package documentation. This data originally
#' came from the Extended Best Track dataset:
#' \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#'
#' @format A dataframe with 48 rows and 4 variables:
#' \describe{
#'     \item{date}{A character string giving the date and time of the observation}
#'     \item{latitude}{A numeric vector giving the storm's latitude at that
#'                     observation time}
#'     \item{longitude}{A numeric vector giving the storm's longitude at that
#'                     observation time}
#'     \item{wind}{A numeric vector giving the estimated maximum sustained
#'                 wind of that storm at that observation time, in knots}
#' }
#'
#' @source
#' \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
"katrina_tracks"

#' Typhoon Saomai tracks data
#'
#' A dataframe containing tracking data for Typhoon Saomai in
#' 2006. This dataframe is included for use as an example storm
#' tracks dataframe in the package documentation. This data originally
#' came from the International Best Track Archive for Climate Stewardship (IBTrACS):
#' \url{https://www.ncdc.noaa.gov/ibtracs/}
#'
#' @format A dataframe with 67 rows and 5 variables:
#' \describe{
#'     \item{iso_time}{A date-time value giving the time of the observation (in UTC)}
#'     \item{lat}{A numeric vector giving the storm's latitude at that
#'                     observation time}
#'     \item{lon}{A numeric vector giving the storm's longitude at that
#'                     observation time}
#'     \item{usa_wind}{A numeric vector giving the estimated maximum sustained
#'                 wind of that storm at that observation time, in knots}
#'     \item{usa_rmw}{A numeric vector giving the estimated radius to maximum wind, in
#'                 nautical miles.}
#' }
#'
#' @source
#' \url{https://www.ncei.noaa.gov/products/international-best-track-archive?name=sources}
"saomai_tracks"

#' Typhoon Mangkhut tracks data
#'
#' A dataframe containing tracking data for Typhoon Mangkhut in
#' 2018. This dataframe is included for use as an example storm
#' tracks dataframe in the package documentation. This data originally
#' came from the International Best Track Archive for Climate Stewardship (IBTrACS):
#' \url{https://www.ncdc.noaa.gov/ibtracs/}
#'
#' @format A dataframe with 99 rows and 5 variables:
#' \describe{
#'     \item{iso_time}{A date-time value giving the time of the observation (in UTC)}
#'     \item{lat}{A numeric vector giving the storm's latitude at that
#'                     observation time}
#'     \item{lon}{A numeric vector giving the storm's longitude at that
#'                     observation time}
#'     \item{usa_wind}{A numeric vector giving the estimated maximum sustained
#'                 wind of that storm at that observation time, in knots}
#'     \item{usa_rmw}{A numeric vector giving the estimated radius to maximum wind, in
#'                 nautical miles.}
#' }
#'
#' @source
#' \url{https://www.ncei.noaa.gov/products/international-best-track-archive?name=sources}
"mangkhut_tracks"

#' A few Australian storm tracks datasets
#'
#' A dataframe containing tracking data for three cyclones in
#' Australia (Larry in 2006, George in 2007, Yasi in 2011, and Marcia in 2015). This dataframe is
#' included for use as example storm
#' tracks dataframes in the package documentation. This data originally
#' came from the International Best Track Archive for Climate Stewardship (IBTrACS):
#' \url{https://www.ncdc.noaa.gov/ibtracs/}
#'
#' @format A dataframe with 99 rows and 5 variables:
#' \describe{
#'     \item{iso_time}{A date-time value giving the time of the observation (in UTC)}
#'     \item{lat}{A numeric vector giving the storm's latitude at that
#'                     observation time}
#'     \item{lon}{A numeric vector giving the storm's longitude at that
#'                     observation time}
#'     \item{usa_wind}{A numeric vector giving the estimated maximum sustained
#'                 wind of that storm at that observation time, in knots}
#'     \item{usa_rmw}{A numeric vector giving the estimated radius to maximum wind, in
#'                 nautical miles.}
#'     \item{bom_wind}{A numeric value giving the estimate maximum mean wind around the
#'                 cyclone, based on data from the Australian Tropical Cyclone Warning Centres.}
#' }
#'
#' @source
#' \url{https://www.ncei.noaa.gov/products/international-best-track-archive?name=sources}
"australian_tracks"

#' Land-sea mask
#'
#' A dataframe with gridded locations worldwide indicating
#' whether each point is land or water. This land-sea mask is used to identify
#' whether hurricane center observations are more likely over land or water,
#' so an appropriate conversion factor can be used to estimate gradient winds
#' from sustained surface winds.
#'
#' @format A dataframe with 1,082,401 rows and 3 variables:
#' \describe{
#'    \item{longitude}{A numeric vector with the longitude of the grid point}
#'    \item{latitude}{A numeric vector with the latitude of the grid point}
#'    \item{land}{A factor specifying whether that grid point is land or water}
#' }
"landmask"

#' Eastern U.S. county latitude and longitudes
#'
#' A dataframe containing locations of population mean centers for counties in
#' the eastern United States. Each county is identified by its 5-digit Federal
#' Information Processing Standard (FIPS) code. This dataframe can be used to
#' model storm winds at each county center. This dataset was put together using
#' a dataframe from the U.S. Census Bureau, which was pulled from the website
#' listed in "Source".
#'
#' @format A dataframe with 2,396 rows and 3 variables:
#' \describe{
#'    \item{fips}{A character vector giving the county's five-digit Federal
#'                Information Processing Standard (FIPS) code}
#'    \item{glat}{A numeric vector giving the latitude of the population mean
#'                    center of each county}
#'    \item{glon}{A numeric vector giving the longitude of the population mean
#'                     center of each county}
#'    \item{glandsea}{A logical vector specifying whether each grid point is over
#'                     land (TRUE) or over water (FALSE).}
#' }
#'
#' @source
#'
#' \url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}
"county_points"
