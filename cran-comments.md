## Test environments
* linux,windows,macos via
  [R-hub](https://github.com/ms609/Ternary/actions/workflows/rhub.yaml)

* Ubuntu, R 4.0, release and devel, via 
  [GitHub actions](https://github.com/ms609/Ternary/actions)

* local Windows 10 install, R 4.6.0
* Windows via `devtools::check_win_devel()`
* Windows Server, R release, via GitHub actions

* Mac OS, R release, via GitHub actions


## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE about invalid URLs:
  URL: https://www.shadedrelief.com/hypso/hypso.html
  
  The URL resolves; this is a false positive.


## Downstream dependencies

Downstream dependencies were checked using `revdepcheck::revdep_check()` via
GitHub actions.  No new errors, warnings or notes are reported.
