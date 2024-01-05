## Test environments
* R-hub, with `rhub::check_for_cran()`

* Ubuntu, R 3.6.0, release and devel, via 
  [GitHub actions](https://github.com/ms609/Ternary/actions)
  
* local Windows 10 install, R 4.4.0 devel
* Windows via `devtools::check_win_devel()`
* Windows Server, R release, via GitHub actions
  
* Mac OS, R release, via GitHub actions


## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

Downstream dependencies were checked using `revdepcheck::revdep_check()` via
GitHub actions.  No new errors, warnings or notes are reported.
