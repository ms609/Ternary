## Test environments
* local Windows 10 install, R 3.6.1
* Windows with devtools::check_win_devel()
* Ubuntu 16.04.6 LTS, R 3.4.0, release and devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* Mac OS X 10.13.3, R devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

This release fixes the `class` errors encountered in the new r-devel environment
(per Kurt Hornik's e-mail of 2019-12-04).

## Downstream dependencies

`revdepcheck::revdep_check()` failed to complete, but tests indicate that 
behaviour used by reverse dependencies has not changed.
