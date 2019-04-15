## Test environments
* local Windows 10 install, R 3.5.2
* Windows with devtools::check_win_devel()
* ubuntu 12.04 (on travis-ci), R 3.2.0 and devel
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

(Certain R-hub platforms delivered trivial notes, e.g. bemoaning the
absence of pandoc or qpdf.)

## Downstream dependencies

There is one downstream dependency:
* cocktailApp

As `revdepcheck` is not presently available for Windows, I have not been able to
test this using R CMD check.  I have manually inspected the package to confirm 
that the new parameters added will not impact the existing usage there.