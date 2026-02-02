# Changelog

## Ternary v2.3.6 (2026-02-02)

- Upgrade
  [`Annotate()`](https://ms609.github.io/Ternary/reference/Annotate.md)
  to use LAPJV in place of Hungarian algorithm.

## Ternary v2.3.5 (2025-09-03)

CRAN release: 2025-09-03

- Fix location of
  [`Annotate()`](https://ms609.github.io/Ternary/reference/Annotate.md)
  annotations with `TernaryPlot(region = *)`.
- Deprecate functions that have moved to PlotTools.
- Increase oldest tested version from R3.6 to R4.0.

## Ternary v2.3.4 (2025-05-07)

CRAN release: 2025-05-07

- Tweak documentation to clarify point colouring and contour
  vectorization.
- Move contour documentation from vignette to documentation page.

## Ternary v2.3.3 (2024-08-27)

CRAN release: 2024-08-27

- Support plotting of `c(0, 0, 0)`, at origin.
- Require R 3.6.
- Remove ‘viridis’ dependency.

## Ternary v2.3.2 (2024-07-25)

CRAN release: 2024-07-25

- Document reading data into R in vignette for new users.
- Provide package anchors in documentation pages.

## Ternary v2.3.1 (2024-02-06)

CRAN release: 2024-02-06

- Improve support for `region` parameter.

## Ternary v2.3.0 (2024-01-08)

CRAN release: 2024-01-09

- `region` parameter plots a sub-region of the ternary space.
- Support `...` in `TernaryContour(Func = function(...))`
  ([\#81](https://github.com/ms609/Ternary/issues/81)).

## Ternary v2.2.1 (2023-06-28)

CRAN release: 2023-06-29

- `Polygon-Geometry` now imported from PlotTools. Aliases of these
  functions are provided but will be removed in a later version of this
  package.
- Use characters, not numerics, for version number checks (see
  <https://bugs.r-project.org/show_bug.cgi?id=18548>).

## Ternary v2.2.0 (2023-05-11)

CRAN release: 2023-05-11

- New function
  [`Annotate()`](https://ms609.github.io/Ternary/reference/Annotate.md)
  annotates points on ternary plot.
- New option `filled` to produce filled contour lines in
  [`TernaryContour()`](https://ms609.github.io/Ternary/reference/TernaryContour.md)
  ([\#69](https://github.com/ms609/Ternary/issues/69)).
- Improve clarity of point size plotting example.
- Native colour bar support in
  [`ColourTernary()`](https://ms609.github.io/Ternary/reference/ColourTernary.md)
  ([\#66](https://github.com/ms609/Ternary/issues/66)).
- Require R 3.5 (due to dependency on “rlang”).

## Ternary v2.1.3 (2023-02-20)

CRAN release: 2023-02-20

- New
  [vignette](https://ms609.github.io/Ternary/articles/annotation.html)
  with annotation example
  ([\#64](https://github.com/ms609/Ternary/issues/64)).

## Ternary v2.1.2 (2022-11-07)

CRAN release: 2022-11-07

- Support independent labelling of axes.

## Ternary v2.1.1 (2022-10-04)

CRAN release: 2022-10-04

- Fix regression that caused `col` parameter to be ignored.
- Fix failure to plot grid when `grid.lines` is integer.
- Mention `srt` graphical parameter in
  [`TernaryText()`](https://ms609.github.io/Ternary/reference/AddToTernary.md)
  documentation.

## Ternary v2.1.0 (2022-05-09)

CRAN release: 2022-05-09

- Colour sub-region of ternary plot with
  [`ColourTernary()`](https://ms609.github.io/Ternary/reference/ColourTernary.md):
  - Add `Polygon-Geometry` functions for polygon manipulation.
  - Support `NA` values in
    [`ColourTernary()`](https://ms609.github.io/Ternary/reference/ColourTernary.md).
- Tidy code formatting and catch typos.

## Ternary v2.0.0 (2022-01-05)

CRAN release: 2022-01-04

- Create Holdridge plots using
  [`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md)
  and related functions
  ([\#48](https://github.com/ms609/Ternary/issues/48)).
- Add `panel.first` and `panel.last` parameters to
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md)
  (fixing [\#54](https://github.com/ms609/Ternary/issues/54)).
- Support graphical parameters (e.g. `xpd`) in
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).
- Add introductory vignette for new R users.
- Help diagnose non-installation of app.

## Ternary v1.2.4 (2021-12-03)

CRAN release: 2021-12-03

- Default to upward direction if
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md)
  not yet called.
- Improved input checking in
  [`TernaryPointValues()`](https://ms609.github.io/Ternary/reference/TernaryPointValues.md)
  and
  [`TernaryContour()`](https://ms609.github.io/Ternary/reference/TernaryContour.md),
  ([\#53](https://github.com/ms609/Ternary/issues/53)), and avoid
  unnecessary calculations.

## Ternary v1.2.3 (2021-10-08)

CRAN release: 2021-10-08

- Fix bug in
  [`TernaryApp()`](https://ms609.github.io/Ternary/reference/TernaryApp.md)
  when ‘colourpicker’ not previously installed.
- Update to ‘testthat’ edition 3.

## Ternary v1.2.2 (2021-05-12)

CRAN release: 2021-05-12

- New
  [vignette](https://ms609.github.io/Ternary/articles/interpolation.html)
  with interpolation example
  ([\#46](https://github.com/ms609/Ternary/issues/46)).
- Document point styling in vignette.
- [`TernaryCoords()`](https://ms609.github.io/Ternary/reference/TernaryCoords.md),
  alias
  [`TernaryToXY()`](https://ms609.github.io/Ternary/reference/TernaryCoords.md),
  accepts matrices.

## Ternary v1.2.1 (2020-12-09)

CRAN release: 2020-12-09

- Use package ‘vdiffr’ conditionally in tests.
- Trivial documentation improvements.

## Ternary v1.2.0 (2020-10-17)

CRAN release: 2020-10-16

- Add Shiny user interface; launch with
  [`TernaryApp()`](https://ms609.github.io/Ternary/reference/TernaryApp.md).
- Optional parameter `axis.rotate` to allow axis labels to be plotted
  ‘unrotated’ (<https://github.com/ms609/Ternary/issues/38>).
- Fix error in `xtip.rotate` (thanks
  [@LSanselme](https://github.com/LSanselme)).
- Add examples to documentation.

## Ternary v1.1.4 (2020-02-27)

CRAN release: 2020-02-27

- Support user-specified colours in
  [`ColourTernary()`](https://ms609.github.io/Ternary/reference/ColourTernary.md).
- More options for controlling axis colours in
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).
- Add examples to documentation.
- axis.labels example in vignette.

## Ternary v1.1.3 (2019-12-04)

CRAN release: 2019-12-04

- Use `inherits`, not `class`
  \[<https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again>\].
- Obey user-specified `direction` in
  [`TernaryContour()`](https://ms609.github.io/Ternary/reference/TernaryContour.md)
  and
  [`TernaryPointValues()`](https://ms609.github.io/Ternary/reference/TernaryPointValues.md)
  (thanks [@pkR-pjR](https://github.com/pkR-pjR)).

## Ternary v1.1.2 (2019-09-16)

CRAN release: 2019-09-16

- `clockwise` parameter allows plots to be configured in ‘flipped’
  orientation (<https://github.com/ms609/Ternary/issues/22>)

## Ternary v1.1.1 (2019-06-17)

CRAN release: 2019-06-14

- `TernaryArrows` adds arrows to a ternary plot.

## Ternary v1.1.0 (2019-04-16)

CRAN release: 2019-04-15

### New features

- Add support for contour plots and density shading.

### Bug fixes

- Maintain isometry when one of `xlim` or `ylim` is specified.
- Fix issues with
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md)
  documentation.
- Remove names in
  [`TernaryCoords()`](https://ms609.github.io/Ternary/reference/TernaryCoords.md).

## Ternary v1.0.2 (2018-10-31)

CRAN release: 2018-10-30

- Improve configuration options by allowing:
  - Custom colouration of axis text;
  - Modification of axis tick length.

## Ternary v1.0.1 (2018-07-05)

CRAN release: 2018-07-05

- Plot minor lines between grid lines, with options to customise
  appearance.
- Document magnification options.

## Ternary v1.0.0 (2017-12-21)

CRAN release: 2017-12-21

- Add function
  [`JoinTheDots()`](https://ms609.github.io/Ternary/reference/AddToTernary.md)
  to plot lines and points simultaneously.
- Support `xlim` & `ylim` for
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).
- Choose direction of plot using new `point` parameter.
- Allow labelling of tips and of sides (`alab` becomes `atip`).
- Improved control over labelling of plot (`clab.rotate` parameter).
- Improve documentation.

## Ternary v0.1.1 (2017-11-23)

CRAN release: 2017-11-23

- Added colour-blind compatible colour palettes `cbPalette8` and
  `cbPalette15`.
- Support `lab.font` in
  [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).

## Ternary v0.1.0 (2017-11-23)

- Generates ternary plots with vertical left axis.
