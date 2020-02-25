## Test environments
* local Windows 10 install, R 3.6.1
* Windows with devtools::check_win_devel()
* Ubuntu 16.04.6 LTS, R 3.4.0, release and devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* Mac OS X 10.13.6, R release, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* R-hub, with `check_for_cran()`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

`revdepcheck::revdep_check()` failed to complete, but tests indicate that 
behaviour used by reverse dependencies has not changed.
