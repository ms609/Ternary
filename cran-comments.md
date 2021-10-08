## Test environments
* local Windows 10 install, R 4.2.0-prerelease r81008
* Windows with devtools::check_win_devel()
* Ubuntu 20.04 LTS, R 3.4.0, release and devel, via 
  [GitHub actions](https://github.com/ms609/Ternary/actions)
* Mac OS X 10.13.6, R release, via GitHub actions
* Windows Server 2019 10.0.17763, R release, via GitHub actions
* R-hub, with `check_for_cran()`

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

Downstream dependencies were checked using `revdepcheck::revdep_check()` via
GitHub actions.  No new errors, warnings or notes are reported.
