
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/NA/NA.svg?branch=master)](https://travis-ci.org/NA/NA)

Overview
--------

You can use the `stormwindmodel` package to model wind speeds at grid points in the United States based on "best tracks" hurricane tracking data.

This package is currently in development on GitHub. You can install it using (you will need to install `devtools` if you do not already have it):

``` r
devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE)
```

The package includes data on the tracks of Hurricane Floyd in 1999 and Hurricane Katrina in 2005. You can load this example best tracks data using:

``` r
library(stormwindmodel)
data("floyd_tracks")
head(floyd_tracks)
#> # A tibble: 6 × 4
#>           date latitude longitude  wind
#>          <chr>    <dbl>     <dbl> <dbl>
#> 1 199909071800     14.6     -45.6    25
#> 2 199909080000     15.0     -46.9    30
#> 3 199909080600     15.3     -48.2    35
#> 4 199909081200     15.8     -49.6    40
#> 5 199909081800     16.3     -51.1    45
#> 6 199909090000     16.7     -52.6    45
```

``` r
data("katrina_tracks")
head(katrina_tracks)
#> # A tibble: 6 × 4
#>           date latitude longitude  wind
#>          <chr>    <dbl>     <dbl> <dbl>
#> 1 200508231800     23.1     -75.1    30
#> 2 200508240000     23.4     -75.7    30
#> 3 200508240600     23.8     -76.2    30
#> 4 200508241200     24.5     -76.5    35
#> 5 200508241800     25.4     -76.9    40
#> 6 200508250000     26.0     -77.7    45
```

This example data includes the following columns:

-   `date`: Date and time of the observation (in UTC)
-   `latitude`, `longitude`: Location of the storm at that time
-   `wind`: Maximum wind speed at that time (knots)

You must have your storm tracks in this format and with these columns names to input the tracks to the functions in `stormwindmodel`. If necessary, use `rename` from `dplyr` to rename columns and `convert_wind_speed` from `weathermetrics` to convert windspeed into knots.

The `stormwindmodel` package also includes a dataset with the location of the population mean center of each U.S. county (`county_points`). This dataset can be used as the grid point inputs if you want to model storm-related winds for counties. These counties are listed FIPS number. The population for \[year\] is also included. This dataset comes from the US Census \[website\].

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

This package uses the wind model developed by Willoughby et al. (2006) for modeling wind speed at each grid location. Full details on how this model is fit are provided in the "Details" vignetted of the `stormwindmodel` package.

Basic example
-------------

The main function of this package is `get_grid_winds`. It inputs storm tracks for a tropical cyclone and a dataframe with grid point locations and outputs summaries of wind at each grid point from the storm. The wind measurements generated for each grid point are:

-   `vmax_gust`: Maximum 10-m 1-minute gust wind experienced at the grid point during the storm
-   `vmax_sust`: Maximum 10-m 1-minute sustained wind experienced at the grid point during the storm
-   `gust_dur`: Duration gust wind was at or above a specified speed (default is 20 m/s) in minutes
-   `sust_dur`: Duration sustained wind was at or above a specified speed (default is 20 m/s) in minutes

To get modeled winds for Hurricane Floyd at U.S. county centers, you can run:

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

If you model winds for county centers, so the `gridid` is a county FIPS, the `stormwindmodel` package has a function called `map_wind` for mapping the estimated winds. By default, it maps the maximum sustained wind in each county during the storm in meters per second.

``` r
map_wind(floyd_winds)
```

![](README-unnamed-chunk-8-1.png)

Further functionality
---------------------

There are a number of options when mapping wind speeds using `map_wind`.

First, you can use the `map_storm_track` function to add the storm track to the map:

``` r
floyd_map <- map_wind(floyd_winds)
add_storm_track(floyd_tracks, plot_object = floyd_map)
```

![](README-unnamed-chunk-9-1.png)

You can also choose whether to map sustained or gust winds, as well as the unit to use for wind speed.

``` r
map_wind(floyd_winds, value = "vmax_gust", wind_metric = "knots")
```

![](README-unnamed-chunk-10-1.png)

Finally, you can map a binary classification of counties with winds at or above a certain break point. For example, to map counties with sustained wind at or above 34 knots during the storm, you can run:

``` r
map_wind(floyd_winds, value = "vmax_sust", wind_metric = "knots",
         break_point = 34)
```

![](README-unnamed-chunk-11-1.png)

Tracks data
-----------

You can get an R version of best tracks data for Atlantic basin storms from 1988 to 2015 through the `hurricaneexposuredata` package (also in development on GitHub):

``` r
devtools::install_github("geanders/hurricaneexposuredata")
```

Here are all the storms included in that dataset:

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

| year | storms                                                                                                                                                                                                    |
|:-----|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1988 | Alberto, Beryl, Chris, Debby, Ernesto, Notnamed, Florence, Gilbert, Helene, Isaac, Joan, Keith                                                                                                            |
| 1989 | Allison, Barry, Chantal, Dean, Erin, Felix, Gabrielle, Hugo, Iris, Jerry, Karen                                                                                                                           |
| 1990 | Arthur, Bertha, Cesar, Diana, Edouard, Fran, Gustav, Hortense, Isidore, Josephine, Klaus, Lili, Marco, Nana                                                                                               |
| 1991 | Ana, Bob, Claudette, Danny, Erika, Fabian, Grace, Notnamed                                                                                                                                                |
| 1992 | Subtrop1, Andrew, Bonnie, Charley, Danielle, Earl, Frances                                                                                                                                                |
| 1993 | Arlene, Bret, Cindy, Dennis, Emily, Floyd, Gert, Harvey                                                                                                                                                   |
| 1994 | Alberto, Beryl, Chris, Debby, Ernesto, Florence, Gordon                                                                                                                                                   |
| 1995 | Allison, Barry, Chantal, Dean, Erin, Felix, Gabrielle, Humberto, Iris, Jerry, Karen, Luis, Marilyn, Noel, Opal, Pablo, Roxanne, Sebastien, Tanya                                                          |
| 1996 | Arthur, Bertha, Cesar, Dolly, Edouard, Fran, Gustav, Hortense, Isidore, Josephine, Kyle, Lili, Marco                                                                                                      |
| 1997 | Subtrop, Ana, Bill, Claudette, Danny, Erika, Fabian, Grace                                                                                                                                                |
| 1998 | Alex, Bonnie, Charley, Danielle, Earl, Frances, Georges, Hermine, Ivan, Jeanne, Karl, Lisa, Mitch, Nicole                                                                                                 |
| 1999 | Arlene, Bret, Cindy, Dennis, Emily, Floyd, Gert, Harvey, Irene, Jose, Katrina, Lenny                                                                                                                      |
| 2000 | Alberto, Beryl, Chris, Debby, Ernesto, Florence, Gordon, Helene, Isaac, Joyce, Keith, Leslie, Michael, Nadine, Subtrop                                                                                    |
| 2001 | Allison, Barry, Chantal, Dean, Erin, Felix, Gabrielle, Humberto, Iris, Jerry, Karen, Lorenzo, Michelle, Noel, Olga                                                                                        |
| 2002 | Arthur, Bertha, Cristobal, Dolly, Edouard, Fay, Gustav, Hanna, Isidore, Josephine, Kyle, Lili                                                                                                             |
| 2003 | Ana, Bill, Claudette, Danny, Erika, Fabian, Grace, Henri, Isabel, Juan, Kate, Larry, Mindy, Nicholas, Odette, Peter                                                                                       |
| 2004 | Alex, Bonnie, Charley, Danielle, Earl, Frances, Gaston, Hermine, Ivan, Jeanne, Karl, Lisa, Matthew, Nicole, Otto                                                                                          |
| 2005 | Arlene, Bret, Cindy, Dennis, Emily, Franklin, Gert, Harvey, Irene, Jose, Katrina, Lee, Maria, Nate, Ophelia, Philippe, Rita, Stan, Subtrop, Tammy, Vince, Wilma, Alpha, Beta, Gamma, Delta, Epsilon, Zeta |
| 2006 | Zeta, Alberto, Notnamed, Beryl, Chris, Debby, Ernesto, Florence, Gordon, Helene, Isaac                                                                                                                    |
| 2007 | Andrea, Barry, Chantal, Dean, Erin, Felix, Gabrielle, Humberto, Ingrid, Jerry, Karen, Lorenzo, Melissa, Noel, Olga                                                                                        |
| 2008 | Arthur, Bertha, Cristobal, Dolly, Edouard, Fay, Gustav, Hanna, Ike, Josephine, Kyle, Laura, Marco, Nana, Omar, Paloma                                                                                     |
| 2009 | Ana, Bill, Claudette, Danny, Erika, Fred, Grace, Henri, Ida                                                                                                                                               |
| 2010 | Alex, Bonnie, Colin, Danielle, Earl, Fiona, Gaston, Hermine, Igor, Julia, Karl, Lisa, Matthew, Nicole, Otto, Paula, Richard, Shary, Tomas                                                                 |
| 2011 | Arlene, Bret, Cindy, Don, Emily, Franklin, Gert, Harvey, Irene, Jose, Katia, Unnamed, Lee, Maria, Nate, Ophelia, Philippe, Rina, Sean                                                                     |
| 2012 | Alberto, Beryl, Chris, Debby, Ernesto, Florence, Helene, Gordon, Isaac, Joyce, Kirk, Leslie, Michael, Nadine, Oscar, Patty, Rafael, Sandy, Tony                                                           |
| 2013 | Andrea, Barry, Chantal, Dorian, Erin, Fernand, Gabrielle, Td08, Humberto, Ingrid, Jerry, Karen, Lorenzo, Melissa, Unnamed                                                                                 |
| 2014 | Arthur, Td02, Bertha, Cristobal, Dolly, Edouard, Fay, Gonzalo, Hanna                                                                                                                                      |
| 2015 | Ana, Bill, Claudette, Danny, Erika, Fred, Grace, Henri, Td09, Ida, Joaquin, Kate                                                                                                                          |

References
==========

Willoughby, HE, RWR Darling, and ME Rahn. 2006. “Parametric Representation of the Primary Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles.” *Monthly Weather Review* 134 (4): 1102–20.
