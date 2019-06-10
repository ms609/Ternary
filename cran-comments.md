## Test environments
* local Windows 10 install, R 3.6.0
* Windows with devtools::check_win_devel()
* Ubuntu 14.04.5 LTS, R 3.2.0 and devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

(Certain R-hub platforms delivered trivial notes relating to pandoc.)

## Downstream dependencies

All downstream dependencies passed `revdepcheck::revdep_check()`:
* cocktailApp
* CongreveLamsdell2016
* plot3logit
* Quartet
