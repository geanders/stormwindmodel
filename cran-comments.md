## Test environments
* local OS X install, R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* In my first submission of this version, I got an email that it did not pass
incoming checks automatically. The problem was with one of the vignette rebuilds
on Windows, for which there was an error regarding the namespace load for a
suggested package used in that vignette, `tigris`, with the note that there was
no package called `rgdal`. Before submitting, I had checked on winbuilder (with
release and development versions), and all checks passed. The section of the
vignette that is related to this warning is a minor part of the vignette.
Therefore, to prevent the error in CRAN's automated incoming checks, I have
changed a small amount of code in that part of the vignette to not evaluate.
With this change, the package can remove from "Suggests" several spatial R
packages, including some that have caused CRAN error or warning issues over the
past year for this package. Since this section of the vignette is tangential to
the main functionality of this package, it was not critical to retain, and I
hope that this change will allow this package to pass the CRAN automated checks
and also help the package remain more stable---in terms of continuing to pass
CRAN checks---by removing several spatial package dependencies that it seems are
evolving somewhat quickly and so were requiring frequent revisions of this
package to keep up based solely on a small example in one vignette. This 
change to the vignette fixes the CRAN warning for the previous version, which 
was based on an error in rebuilding this vignette. I have tested this version
of the package on winbuilder (release and devel) and on rhub's windows-x86_64-devel 
platform, as well as on a local MacOS installation of R 4.0.2, and all passed
without errors, warnings, or notes. 

## Reverse dependencies

There are no reverse dependencies.

