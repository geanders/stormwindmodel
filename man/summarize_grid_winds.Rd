% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_wind_code.R
\name{summarize_grid_winds}
\alias{summarize_grid_winds}
\title{Generate wind summaries for grid points}
\usage{
summarize_grid_winds(
  grid_winds,
  gust_duration_cut = 20,
  sust_duration_cut = 20,
  tint = 0.25
)
}
\arguments{
\item{grid_winds}{A matrix where each column is a time series of modeled wind speeds at
a location, as created by \code{\link{calc_grid_winds}}.}

\item{gust_duration_cut}{The wind speed, in meters per second, to use as a
cutoff point for determining the duration of gust winds. The function
will calculate the minutes during the storm when surface-level gust winds
were above this speed at the location}

\item{sust_duration_cut}{The wind speed, in meters per second, to use as a
cutoff point for determining the duration of gust winds. The function
will calculate the minutes during the storm when surface-level gust winds
were above this speed at the location.}

\item{tint}{Interval (in hours) to which to interpolate the tracks. The
default is 0.25 (i.e., 15 minutes).}
}
\value{
Returns a dataframe with wind characteristics for each
   location. The wind characteristics given are:
   \itemize{
     \item{\code{vmax_gust}: Maximum value of surface-level (10 meters)
       gust winds, in meters per second, over the length of the
       storm at the given location}
     \item{\code{vmax_sust}: Maximum value of surface-level (10 meters)
       sustained winds, in meters per second, over the length of the
       storm at the given location}
     \item{\code{gust_duration}: Length of time, in minutes, that
       surface-level gust winds were above a specified value (default is
       20 meters per second)}
     \item{\code{sust_duration}: Length of time, in minutes, that
       surface-level sustained winds were above a specified value (default is
       20 meters per second}
   }
}
\description{
Summarizes the wind time series for grid points, as
created by \code{\link{calc_grid_winds}}.
}
