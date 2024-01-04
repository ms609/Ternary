# Ternary

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#project-statuses)
[![codecov](https://codecov.io/gh/ms609/Ternary/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ms609/Ternary)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/Ternary)](https://cran.r-project.org/package=Ternary)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/Ternary)](https://cran.r-project.org/package=Ternary)
[![DOI](https://zenodo.org/badge/111806977.svg)](https://zenodo.org/badge/latestdoi/111806977)


'Ternary' is an R package that allows the creation of ternary plots 
(a.k.a. ternary graphs / simplex plots / Gibbs triangles / de Finetti diagrams) 
and Holdridge life zone diagrams
using the familiar functions of the default 'graphics' package.

![Example ternary plot](https://user-images.githubusercontent.com/1695515/233467338-88a3b3a8-5580-4924-a11e-06dc8a38b3e9.png)


For simple use cases, generate ternary plots using the point-and-click
Shiny app:

```r
install.packages("Ternary")
Ternary::TernaryApp()
```

For greater control over your plots, use the R command line;
usage instructions are available for
[Ternary plots](https://ms609.github.io/Ternary/articles/Ternary.html)
(with an [introductory vignette for R beginners](https://ms609.github.io/Ternary/articles/new-users.html)), and for
[Holdridge plots](https://ms609.github.io/Ternary/articles/Holdridge.html). 

Install the development version from GitHub with
```r
devtools::install_github("ms609/Ternary", args = "--recursive")
```
This requires [git](https://git-scm.com/) to be installed and added to
your PATH system environment variable.
You may also require the '[curl](https://CRAN.R-project.org/package=curl)'
R package.

Please let me know of any feature requests or bugs by [opening an 
issue on GitHub](https://github.com/ms609/Ternary/issues/).

## Citation

You can cite this package as:

Smith, Martin R. (2017). _Ternary: An R Package for Creating Ternary Plots._
Comprehensive R Archive Network, 
[doi:10.5281/zenodo.1068996](https://dx.doi.org/10.5281/zenodo.1068996).

## See also

The R package '[ggtern](https://CRAN.R-project.org/package=ggtern)'
implements ternary plots within the 'ggplot2' framework.

Please note that the 'Ternary' project is released with a
[Contributor Code of Conduct](https://ms609.github.io/packages/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
