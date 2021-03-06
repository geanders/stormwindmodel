% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{landmask}
\alias{landmask}
\title{Land-sea mask}
\format{
A dataframe with 1,082,401 rows and 3 variables:
\describe{
   \item{longitude}{A numeric vector with the longitude of the grid point}
   \item{latitude}{A numeric vector with the latitude of the grid point}
   \item{land}{A factor specifying whether that grid point is land or water}
}
}
\usage{
landmask
}
\description{
A dataframe with gridded locations worldwide indicating
whether each point is land or water. This land-sea mask is used to identify
whether hurricane center observations are more likely over land or water,
so an appropriate conversion factor can be used to estimate gradient winds
from sustained surface winds.
}
\keyword{datasets}
