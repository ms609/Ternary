## Test environments
* local Windows 10 install, R 3.5.1
* Windows with devtools::check_win_devel()
* ubuntu 12.04 (on travis-ci), R 3.2.0 and devel
* Linux with check_rhub()

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

  New maintainer:
  Maintainer: 'Martin R. Smith <martin.smith@durham.ac.uk>'
    Martin R. Smith <martin.smith@durham.ac.uk>
  Old maintainer(s):
    Martin R. Smith <martins@gmail.com>
    
I have replaced my personal e-mail address with my professional one.

## Downstream dependencies

There is one downstream dependency:
* cocktailApp

As `revdepcheck` is not presently available for Windows, I have not been able to
test this using R CMD check.  I have manually inspected the package to confirm 
that the new parameters added will not impact the existing usage there.