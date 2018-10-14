
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://api.travis-ci.org/geanders/stormwindmodel.svg?branch=master)](https://travis-ci.org/geanders/stormwindmodel)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/stormwindmodel)](https://cran.r-project.org/package=stormwindmodel)

## Overview

The `stormwindmodel` package was created to allow users to model wind
speeds at grid points in the United States based on “best tracks”
hurricane tracking data, using a model for wind speed developed by
Willoughby and coauthors (2006). The package includes functions for
interpolating hurricane tracks and for modeling and mapping wind speeds
during the storm. It includes population mean center locations for all
U.S. counties, which can be used to map winds by county; however, other
grid point locations can also be input for modeling. Full details on how
this model is fit are provided in the “Details” vignette of the
`stormwindmodel` package.

This package is currently in development on GitHub. You can install it
using the `install_github` function from the `devtools` package
using:

``` r
devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE)
```

## Package example data

For examples, the package includes data on the tracks of Hurricane Floyd
in 1999 and Hurricane Katrina in 2005. You can load these example best
tracks data sets using:

``` r
library(stormwindmodel)
data("floyd_tracks")
head(floyd_tracks)
#> # A tibble: 6 x 4
#>   date         latitude longitude  wind
#>   <chr>           <dbl>     <dbl> <dbl>
#> 1 199909071800     14.6     -45.6    25
#> 2 199909080000     15       -46.9    30
#> 3 199909080600     15.3     -48.2    35
#> 4 199909081200     15.8     -49.6    40
#> 5 199909081800     16.3     -51.1    45
#> 6 199909090000     16.7     -52.6    45
data("katrina_tracks")
head(katrina_tracks)
#> # A tibble: 6 x 4
#>   date         latitude longitude  wind
#>   <chr>           <dbl>     <dbl> <dbl>
#> 1 200508231800     23.1     -75.1    30
#> 2 200508240000     23.4     -75.7    30
#> 3 200508240600     23.8     -76.2    30
#> 4 200508241200     24.5     -76.5    35
#> 5 200508241800     25.4     -76.9    40
#> 6 200508250000     26       -77.7    45
```

This example data includes the following columns:

  - `date`: Date and time of the observation (in UTC)
  - `latitude`, `longitude`: Location of the storm at that time
  - `wind`: Maximum wind speed at that time (knots)

You can input other storm tracks into the wind modeling functions in the
`stormwindmodel` package, but you must have your storm tracks in the
same format as these example dataframes and with these columns names to
input the tracks to the functions in `stormwindmodel`. If necessary, use
`rename` from `dplyr` to rename columns and `convert_wind_speed` from
`weathermetrics` to convert windspeed into knots.

The `stormwindmodel` package also includes a dataset with the location
of the population mean center of each U.S. county (`county_points`).
This dataset can be used as the grid point inputs if you want to model
storm-related winds for counties. These counties are listed by Federal
Information Processing Standard (FIPS) number, which uniquely identifies
each U.S. county. This dataset comes from the US Census [file of county
population mean center
locations](http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt),
as of the 2010 Census.

``` r
data(county_points)
head(county_points)
#>   gridid     glat      glon
#> 1  01001 32.50039 -86.49416
#> 2  01003 30.54892 -87.76238
#> 3  01005 31.84404 -85.31004
#> 4  01007 33.03092 -87.12766
#> 5  01009 33.95524 -86.59149
#> 6  01011 32.11633 -85.70119
```

You can use a different dataset of grid points to model winds at other
U.S. locations, including across evenly spaced grid points. However, you
will need to include these grid points in a dataframe with a similar
format to this example dataframe, with columns for each grid point id
(`gridid`— these IDs can be random but should be unique across grid
points), and `glat` and `glon` for latitude and longitude of each grid
point.

## Basic example

The main function of this package is `get_grid_winds`. It inputs storm
tracks for a tropical cyclone (`hurr_track`) and a dataframe with grid
point locations (`grid_df`). It models winds during the tropical storm
at each grid point and outputs summaries of wind during the storm at
each grid point from the storm. The wind measurements generated for each
grid point are:

  - `vmax_gust`: Maximum 10-m 1-minute gust wind experienced at the grid
    point during the storm
  - `vmax_sust`: Maximum 10-m 1-minute sustained wind experienced at the
    grid point during the storm
  - `gust_dur`: Duration gust wind was at or above a specified speed
    (default is 20 m/s), in minutes
  - `sust_dur`: Duration sustained wind was at or above a specified
    speed (default is 20 m/s), in minutes

To get modeled winds for Hurricane Floyd at U.S. county centers, you can
run:

``` r
floyd_winds <- get_grid_winds(hurr_track = floyd_tracks,
                              grid_df = county_points)
floyd_winds %>%
  dplyr::select(gridid, vmax_gust, vmax_sust, gust_dur, sust_dur) %>%
  slice(1:6)
#>   gridid vmax_gust vmax_sust gust_dur sust_dur
#> 1  01001  2.969661  1.993061        0        0
#> 2  01003  1.929468  1.294945        0        0
#> 3  01005  4.800611  3.221887        0        0
#> 4  01007  2.312488  1.552005        0        0
#> 5  01009  2.609132  1.751095        0        0
#> 6  01011  4.076456  2.735877        0        0
```

If you use the `coutny_points` data that comes with the package for the
`grid_df` argument, you will model winds for county centers. In this
case, the `gridid` is a county FIPS, and the `stormwindmodel` package
has a function called `map_wind` for mapping the estimated winds for
each county. By default, it maps the maximum sustained wind in each
county during the storm in meters per second.

``` r
map_wind(floyd_winds)
```

![](README-unnamed-chunk-7-1.png)<!-- -->

## Further functionality

### Options for modeling winds

You can input the track for any Atlantic Basin tropical storm into
`get_grid_winds`, as long as you convert it to meet the following format
requirements:

  - Is a dataframe of class `tbl_df` (you can use the `tbl_df` function
    from `dplyr` to do this)
  - Has the following columns:
      - `date`: A character vector with date and time (in UTC),
        expressed as YYYYMMDDHHMM.
      - `latitude`: A numeric vector with latitude in decimal degrees.
      - `longitude`: A numeric vector with longitude in decimal degrees.
      - `wind`: A numeric vector with maximum storm wind speed in knots

For the grid point locations at which to model, you can input a
dataframe with grid points anywhere in the eastern half of the United
States. For example, you may want to map wind speeds for Hurricane
Katrina by census tract in Orleans Parish, LA. The following code shows
how a user could do that with the `stormwindmodel` package.

First, the `tigris` package can be used to pull US Census tract
shapefiles for a county. You can use the following code to pull these
census tract file shapefiles for Orleans Parish in Louisiana:

``` r
library(tigris)
new_orleans <- tracts(state = "LA", county = c("Orleans")) 
```

This shapefile gives the polygon for each census tract. You can use the
`gCentroid` function from the `rgeos` package to determine the location
of the center of each census tract:

``` r
library(rgeos)
new_orleans_tract_centers <- gCentroid(new_orleans, byid = TRUE)@coords
head(new_orleans_tract_centers)
#>             x        y
#> 141 -89.96588 30.05844
#> 142 -90.07139 29.94885
#> 143 -90.03572 29.96558
#> 144 -90.03305 29.99398
#> 145 -90.07957 29.99610
#> 146 -90.07068 29.92515
```

With some cleaning, you can get this data to the format required for the
`get_grid_winds` function. In particular, you should add the tract id
from the original shapefiles as the grid id, as this will help you map
the modeled wind results:

``` r
new_orleans_tract_centers <- new_orleans_tract_centers %>%
  tbl_df() %>%
  mutate(gridid = unique(new_orleans@data$TRACTCE)) %>%
  dplyr::rename(glat = y, 
                glon = x)
head(new_orleans_tract_centers)
#> # A tibble: 6 x 3
#>    glon  glat gridid
#>   <dbl> <dbl> <chr> 
#> 1 -90.0  30.1 001745
#> 2 -90.1  29.9 013400
#> 3 -90.0  30.0 013600
#> 4 -90.0  30.0 013700
#> 5 -90.1  30.0 013800
#> 6 -90.1  29.9 014100
```

Here is a map of the census tracts, with the center point of each shown
with a red dot (note that an area over water is also included– this is
included as one of the census tract shapefiles pulled by `tigris` for
Orleans Parish):

``` r
library(sf)
new_orleans <- new_orleans %>% 
  st_as_sf()
new_orleans_centers <- new_orleans_tract_centers %>% 
  st_as_sf(coords = c("glon", "glat")) %>% 
  st_set_crs(4269)

library(ggplot2)
ggplot() + 
  geom_sf(data = new_orleans) + 
  geom_sf(data = new_orleans_centers, color = "red", size = 0.6)
```

![](README-unnamed-chunk-11-1.png)<!-- -->

Since the `new_orleans_tract_centers` is now in the appropriate format
to use with the `stormwindmodel` functions, you can input it directly
into `get_grid_winds` to model the winds from Hurricane Katrina at each
census tract
center:

``` r
new_orleans_tracts_katrina <- get_grid_winds(hurr_track = katrina_tracks, 
                                             grid_df = new_orleans_tract_centers)
head(new_orleans_tracts_katrina)
#>        glon     glat gridid vmax_gust vmax_sust gust_dur sust_dur
#> 1 -89.96588 30.05844 001745  64.40277  43.22333     1110      690
#> 2 -90.07139 29.94885 013400  59.19886  39.73078     1095      705
#> 3 -90.03572 29.96558 013600  61.05882  40.97908     1110      705
#> 4 -90.03305 29.99398 013700  60.94199  40.90066     1095      690
#> 5 -90.07957 29.99610 013800  58.36141  39.16873     1095      690
#> 6 -90.07068 29.92515 014100  59.37257  39.84736     1110      690
```

To plot these modeled winds, you can merge this modeled data back into
the “sf” version of the census tract shapefile data, joining by census
tract identification, and then add to the map. You can show wind speed
in this map with color.

``` r
new_orleans <- new_orleans %>% 
  left_join(new_orleans_tracts_katrina, by = c("TRACTCE" = "gridid"))
```

``` r
library(viridis)
ggplot() + 
  geom_sf(data = new_orleans, aes(fill = vmax_sust)) + 
  geom_sf(data = new_orleans_centers, color = "red", size = 0.6) + 
  scale_fill_viridis(name = "Maximum\nsustained\nwinds (m/s)")
```

![](README-unnamed-chunk-14-1.png)<!-- -->

There are also functions in this package that you can use to create a
time series of all modeled winds at a specific grid point throughout the
storm. For example, here is the code to calculate modeled wind at the
population mean center of Dare County, NC (FIPS: 37055) throughout
Hurricane
Floyd:

``` r
dare_county <- county_points %>% # Get grid point information for Dare County
  filter(gridid == "37055")

with_wind_radii <- floyd_tracks %>%
  create_full_track() %>% # Interpolate tracks to every 15 minutes
  add_wind_radii()        # Calculate required inputs for Willoughby wind model

dare_winds <- calc_grid_wind(grid_point = dare_county,          # Model winds at one grid point
                             with_wind_radii = with_wind_radii)

ggplot(dare_winds, aes(x = date, y = windspeed)) + 
  geom_line() + 
  xlab("Observation time (UTC)") + 
  ylab("Modeled surface wind (m / s)") 
```

![](README-unnamed-chunk-15-1.png)<!-- -->

For more details, see the “Details” vignette, which walks through all
steps of the modeling process.

### Options for mapping county-level winds

There are a number of options when mapping wind speeds using `map_wind`.

First, you can use the `add_storm_track` function to add the storm track
to the map. This function inputs one dataframe with tracking data (the
`floyd_tracks` example data that comes with the package in this case) as
well as the plot object created using `map_wind`, which is input using
the `plot_object` argument. In this example code, we’ve first created
the base map of winds by county using `map_wind` and then input that,
along with Floyd’s track data, into `add_storm_track` to create a map
with both winds and the storm tracks:

``` r
floyd_map <- map_wind(floyd_winds)
add_storm_track(floyd_tracks, plot_object = floyd_map)
```

![](README-unnamed-chunk-16-1.png)<!-- -->

You can also choose whether to map sustained or gust winds (`value`,
which can take “vmax\_gust” or “vmax\_sust”), as well as the unit to use
for wind speed (`wind_metric`, which can take values of “mps” \[the
default\] or “knots”).

``` r
map_wind(floyd_winds, value = "vmax_gust", wind_metric = "knots")
```

![](README-unnamed-chunk-17-1.png)<!-- -->

Finally, you can map a binary classification of counties with winds at
or above a certain break point. For example, to map counties with
sustained wind at or above 34 knots during the storm, you can run:

``` r
map_wind(floyd_winds, value = "vmax_sust", wind_metric = "knots",
         break_point = 34)
```

![](README-unnamed-chunk-18-1.png)<!-- -->

## Tracks data

You can get an R version of best tracks data for Atlantic basin storms
from 1988 to 2015 through the `hurricaneexposuredata` package (also in
development on GitHub):

``` r
devtools::install_github("geanders/hurricaneexposuredata")
```

Here are all the storms currently included in that dataset:

``` r
library(hurricaneexposuredata)
data("hurr_tracks")
hurr_tracks %>% 
  tidyr::separate(storm_id, c("storm", "year")) %>%
  dplyr::select(storm, year) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year) %>% 
  dplyr::summarize(storms = paste(storm, collapse = ", ")) %>% 
  knitr::kable()
```

| year | storms                                                                      |
| :--- | :-------------------------------------------------------------------------- |
| 1988 | Alberto, Beryl, Chris, Florence, Gilbert, Keith                             |
| 1989 | Allison, Chantal, Hugo, Jerry                                               |
| 1990 | Bertha, Marco                                                               |
| 1991 | Ana, Bob, Fabian, Notnamed                                                  |
| 1992 | Andrew, Danielle, Earl                                                      |
| 1993 | Arlene, Emily                                                               |
| 1994 | Alberto, Beryl, Gordon                                                      |
| 1995 | Allison, Dean, Erin, Gabrielle, Jerry, Opal                                 |
| 1996 | Arthur, Bertha, Edouard, Fran, Josephine                                    |
| 1997 | Subtrop, Ana, Danny                                                         |
| 1998 | Bonnie, Charley, Earl, Frances, Georges, Hermine, Mitch                     |
| 1999 | Bret, Dennis, Floyd, Harvey, Irene                                          |
| 2000 | Beryl, Gordon, Helene, Leslie                                               |
| 2001 | Allison, Barry, Gabrielle, Michelle                                         |
| 2002 | Arthur, Bertha, Cristobal, Edouard, Fay, Gustav, Hanna, Isidore, Kyle, Lili |
| 2003 | Bill, Claudette, Erika, Grace, Henri, Isabel                                |
| 2004 | Alex, Bonnie, Charley, Frances, Gaston, Hermine, Ivan, Jeanne, Matthew      |
| 2005 | Arlene, Cindy, Dennis, Emily, Katrina, Ophelia, Rita, Tammy, Wilma          |
| 2006 | Alberto, Beryl, Chris, Ernesto                                              |
| 2007 | Andrea, Barry, Erin, Gabrielle, Humberto, Noel                              |
| 2008 | Cristobal, Dolly, Edouard, Fay, Gustav, Hanna, Ike, Kyle, Paloma            |
| 2009 | Claudette, Ida                                                              |
| 2010 | Alex, Bonnie, Earl, Hermine, Nicole, Paula                                  |
| 2011 | Bret, Don, Emily, Irene, Lee                                                |
| 2012 | Alberto, Beryl, Debby, Isaac, Sandy                                         |
| 2013 | Andrea, Dorian, Karen                                                       |
| 2014 | Arthur                                                                      |
| 2015 | Ana, Bill, Claudette                                                        |

# References

<div id="refs" class="references">

<div id="ref-willoughby2006parametric">

Willoughby, HE, RWR Darling, and ME Rahn. 2006. “Parametric
Representation of the Primary Hurricane Vortex. Part II: A New Family of
Sectionally Continuous Profiles.” *Monthly Weather Review* 134 (4):
1102–20.

</div>

</div>
