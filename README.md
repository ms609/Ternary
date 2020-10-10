# Ternary

[![Project Status: Inactive.](https://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#project-statuses)
[![Build Status](https://travis-ci.org/ms609/Ternary.svg?branch=master)](https://travis-ci.org/ms609/Ternary)
[![codecov](https://codecov.io/gh/ms609/Ternary/branch/master/graph/badge.svg)](https://codecov.io/gh/ms609/Ternary)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/Ternary)](https://cran.r-project.org/package=Ternary)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/Ternary)](https://cran.r-project.org/package=Ternary)
[![DOI](https://zenodo.org/badge/111806977.svg)](https://zenodo.org/badge/latestdoi/111806977) [![Join the chat at https://gitter.im/TernaryPackage/community](https://badges.gitter.im/TernaryPackage/community.svg)](https://gitter.im/TernaryPackage/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


'Ternary' is an R package that allows the creation of ternary plots 
(a.k.a. ternary graphs / simplex plots / Gibbs triangles / de Finetti diagrams) 
using the familiar functions of the default 'graphics' package.

For simple use cases, generate Ternary plots using the point-and-click
Shiny app:

```
install.packages('shiny')
install.packages('Ternary')
shiny::runGitHub('ms609/TernaryApp')
```

For greater control over your plots, use the R command line;
usage instructions can be 
[viewed here](https://ms609.github.io/Ternary/articles/Ternary.html).

Please let me know of any feature requests or bugs by [opening an 
issue on GitHub](https://github.com/ms609/Ternary/issues/).

## Citation

You can cite this package as:

Smith, Martin R. (2017). _Ternary: An R Package for Creating Ternary Plots._ Zenodo, doi: [10.5281/zenodo.1068996](https://dx.doi.org/10.5281/zenodo.1068996).

## See also

The R package '[ggtern](https://CRAN.R-project.org/package=ggtern)' implements ternary plots within the 'ggplot2' framework.

Please note that the 'Ternary' project is released with a
[Contributor Code of Conduct](https://ms609.github.io/Ternary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
