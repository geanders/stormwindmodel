# stormwindmodel 0.1.4

* Resolve a warning on CRAN when building vignettes. Due to a change in 
one of the packages used in one vignette, that vignette was building with 
an error. The `tigris` package changed to import Census spatial files as
`sf` objects by default, rather than `sp` objects. This caused an error
in later code that worked with the objected being imported in the vignette. 
We have resolved this by explicitly requesting the object from `tigris` 
be a `sp` class in the vignette code. 

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



