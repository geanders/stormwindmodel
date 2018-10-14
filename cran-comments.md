## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This release removes all use of `ggmap`. In the previous version of the 
package, use of the Google Maps API was causing errors for CRAN checks.
The package has been changed to completely avoid use of the Google Maps
API to address this problem.

## Reverse dependencies

There are no reverse dependencies.

