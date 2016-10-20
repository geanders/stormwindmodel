
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

You can use the `stormwindmodel` package to model wind speeds at grid points in the United States based on "best tracks" hurricane tracking data.

This package is currently in development on GitHub. You can install it using (you will need to install `devtools` if you do not already have it):

``` r
devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE)
```

The package includes data on the tracks of Hurricane Floyd in 1999. You can load this example best tracks data using:

``` r
library(stormwindmodel)
data("floyd_tracks")
head(floyd_tracks)
#>        storm_id         date latitude longitude wind
#> 1563 Floyd-1999 199909071800     14.6     -45.6   25
#> 1564 Floyd-1999 199909080000     15.0     -46.9   30
#> 1565 Floyd-1999 199909080600     15.3     -48.2   35
#> 1566 Floyd-1999 199909081200     15.8     -49.6   40
#> 1567 Floyd-1999 199909081800     16.3     -51.1   45
#> 1568 Floyd-1999 199909090000     16.7     -52.6   45
```

This example data includes the following columns:

-   `storm_id`: Unique identifier for the tropical cyclone
-   `date`: Date and time of the observation (in UTC)
-   `latitude`, `longitude`: Location of the storm at that time
-   `wind`: Maximum wind speed at that time (knots)

You must have your storm tracks in this format and with these columns names to input the tracks to the functions in `stormwindmodel`. If necessary, use `rename` from `dplyr` to rename columns and `convert_wind_speed` from `weathermetrics` to convert windspeed into knots.

The `stormwindmodel` package also includes a dataset with the location of the population mean center of each U.S. county (`county_points`). This dataset can be used as the grid point inputs if you want to model storm-related winds for counties. These counties are listed FIPS number. The population for \[year\] is also included. This dataset comes from the US Census \[website\].

``` r
data(county_points)
head(county_points)
#>   gridid     glat      glon   gpop
#> 1  01001 32.50039 -86.49416  54571
#> 2  01003 30.54892 -87.76238 182265
#> 3  01005 31.84404 -85.31004  27457
#> 4  01007 33.03092 -87.12766  22915
#> 5  01009 33.95524 -86.59149  57322
#> 6  01011 32.11633 -85.70119  10914
```

This package uses the wind model developed by Willoughby \[reference\] for modeling wind speed at each grid location. Full details on how this model is fit are provided in the "Details" vignetted of the `stormwindmodel` package.

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
#> 1  01001  2.079997 1.3959708        0        0
#> 2  01003  1.262794 0.8475128        0        0
#> 3  01005  3.457852 2.3207060        0        0
#> 4  01007  1.630225 1.0941110        0        0
#> 5  01009  1.922359 1.2901737        0        0
#> 6  01011  2.903270 1.9485032        0        0
```

If you model winds for county centers, so the `gridid` is a county FIPS, the `stormwindmodel` package has a function called `map_wind` for mapping the estimated winds. By default, it maps the maximum sustained wind in each county during the storm in meters per second.

``` r
map_wind(floyd_winds)
```

![](README-unnamed-chunk-7-1.png)

Further functionality
---------------------

There are a number of options when mapping wind speeds using `map_wind`.

First, you can use the `map_tracks` function from `hurricaneexposure` package (also in development on GitHub) to add the storm track to the map. To do that, you save the wind map as an R object and input that object to `map_tracks` as the `plot_object`. This function can only be used for Atlantic basin storms between 1988 and 2015.

``` r
install_github("geanders/hurricaneexposure")
```

``` r
library(hurricaneexposure)
floyd_map <- map_wind(floyd_winds)
map_tracks("Floyd-1999", plot_object = floyd_map)
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

You can get an R version of this data for Atlantic basin storms from 1988 to 2015 through the `hurricaneexposuredata` package (also in development on GitHub):

``` r
devtools::install_github("geanders/hurricaneexposuredata")
```

``` r
library(hurricaneexposuredata)
data("hurr_tracks")
hurr_tracks %>% 
  tidyr::separate(storm_id, c("storm", "year")) %>%
  dplyr::select(storm, year) %>%
  dplyr::distinct() %>%
  knitr::kable()
```

| storm     | year |
|:----------|:-----|
| Alberto   | 1988 |
| Beryl     | 1988 |
| Chris     | 1988 |
| Florence  | 1988 |
| Gilbert   | 1988 |
| Keith     | 1988 |
| Allison   | 1989 |
| Chantal   | 1989 |
| Hugo      | 1989 |
| Jerry     | 1989 |
| Bertha    | 1990 |
| Marco     | 1990 |
| Ana       | 1991 |
| Bob       | 1991 |
| Fabian    | 1991 |
| Notnamed  | 1991 |
| Andrew    | 1992 |
| Danielle  | 1992 |
| Earl      | 1992 |
| Arlene    | 1993 |
| Emily     | 1993 |
| Alberto   | 1994 |
| Beryl     | 1994 |
| Gordon    | 1994 |
| Allison   | 1995 |
| Dean      | 1995 |
| Erin      | 1995 |
| Gabrielle | 1995 |
| Jerry     | 1995 |
| Opal      | 1995 |
| Arthur    | 1996 |
| Bertha    | 1996 |
| Edouard   | 1996 |
| Fran      | 1996 |
| Josephine | 1996 |
| Subtrop   | 1997 |
| Ana       | 1997 |
| Danny     | 1997 |
| Bonnie    | 1998 |
| Charley   | 1998 |
| Earl      | 1998 |
| Frances   | 1998 |
| Georges   | 1998 |
| Hermine   | 1998 |
| Mitch     | 1998 |
| Bret      | 1999 |
| Dennis    | 1999 |
| Floyd     | 1999 |
| Harvey    | 1999 |
| Irene     | 1999 |
| Beryl     | 2000 |
| Gordon    | 2000 |
| Helene    | 2000 |
| Leslie    | 2000 |
| Allison   | 2001 |
| Barry     | 2001 |
| Gabrielle | 2001 |
| Karen     | 2001 |
| Michelle  | 2001 |
| Arthur    | 2002 |
| Bertha    | 2002 |
| Cristobal | 2002 |
| Edouard   | 2002 |
| Fay       | 2002 |
| Gustav    | 2002 |
| Hanna     | 2002 |
| Isidore   | 2002 |
| Kyle      | 2002 |
| Lili      | 2002 |
| Bill      | 2003 |
| Claudette | 2003 |
| Erika     | 2003 |
| Grace     | 2003 |
| Henri     | 2003 |
| Isabel    | 2003 |
| Alex      | 2004 |
| Bonnie    | 2004 |
| Charley   | 2004 |
| Frances   | 2004 |
| Gaston    | 2004 |
| Hermine   | 2004 |
| Ivan      | 2004 |
| Jeanne    | 2004 |
| Matthew   | 2004 |
| Arlene    | 2005 |
| Cindy     | 2005 |
| Dennis    | 2005 |
| Emily     | 2005 |
| Katrina   | 2005 |
| Ophelia   | 2005 |
| Rita      | 2005 |
| Tammy     | 2005 |
| Wilma     | 2005 |
| Alberto   | 2006 |
| Beryl     | 2006 |
| Chris     | 2006 |
| Ernesto   | 2006 |
| Andrea    | 2007 |
| Barry     | 2007 |
| Erin      | 2007 |
| Gabrielle | 2007 |
| Humberto  | 2007 |
| Noel      | 2007 |
| Cristobal | 2008 |
| Dolly     | 2008 |
| Edouard   | 2008 |
| Fay       | 2008 |
| Gustav    | 2008 |
| Hanna     | 2008 |
| Ike       | 2008 |
| Kyle      | 2008 |
| Paloma    | 2008 |
| Claudette | 2009 |
| Ida       | 2009 |
| Alex      | 2010 |
| Bonnie    | 2010 |
| Earl      | 2010 |
| Hermine   | 2010 |
| Nicole    | 2010 |
| Paula     | 2010 |
| Bret      | 2011 |
| Don       | 2011 |
| Emily     | 2011 |
| Irene     | 2011 |
| Lee       | 2011 |
| Alberto   | 2012 |
| Beryl     | 2012 |
| Debby     | 2012 |
| Isaac     | 2012 |
| Sandy     | 2012 |
| Andrea    | 2013 |
| Dorian    | 2013 |
| Karen     | 2013 |
| Arthur    | 2014 |
| Ana       | 2015 |
| Bill      | 2015 |
| Claudette | 2015 |
