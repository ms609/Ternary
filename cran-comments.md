## Test environments
* local Windows 10 install, R 4.0.2
* Windows with devtools::check_win_devel()
* Ubuntu 16.04.6 LTS, R 3.4.0, release and devel, via [Travis CI](https://travis-ci.org/ms609/Ternary)
* Mac OS X 10.13.6, R release, via [Travis CI](https://travis-ci.org/ms609/Ternary/)
* R-hub, with `check_for_cran()`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

`revdepcheck::revdep_check()` found no new errors, warnings or notes in reverse
dependencies:

√ CongreveLamsdell2016 1.0.2
√ plot3logit 1.0.2

revdepcheck was unable to check the package 'Quartet' (which I maintain); I have
tested 'Quartet' locally with Ternary 1.1.4 and no errors, warnings or notes are
reported.
