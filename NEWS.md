# stormwindmodel 0.1.3

* Resolve a warning on CRAN when rebuilding vignettes in Windows that 
seems to have resulted from the use of caching in one vignette.
* Remove dplyr functions that are being deprecated ('select_', 'mutate_', 
'filter_')

# stormwindmodel 0.1.2

* Resolve a warning on CRAN when rebuilding vignettes in Windows
* Improve method of interpolating storm tracks between synoptic times.
Now uses natural cubic spline for location (latitude and longitude) and
linear interpolation for maximum wind speed (in line with interpolation
by ibtracs).

# stormwindmodel 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Removed use of `ggmap` within vignette and README examples.



