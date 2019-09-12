## Test environments
* local Windows 10 install, R 3.6.0
* Windows with devtools::check_win_devel()
* Ubuntu 16.04.6 LTS, R 3.4.0, release and devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* Mac OS X 10.13.3, R devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

(Certain R-hub platforms delivered trivial notes relating to pandoc.)

I have updated the link to {arrows} in man/TernaryPlot.Rd.

## Downstream dependencies

`revdepcheck::revdep_check()` failed to run, but tests indicate that 
behaviour used by reverse dependencies has not changed.
