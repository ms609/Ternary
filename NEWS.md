# Ternary v2.3.2.9000 (development)
- Support plotting of `c(0, 0, 0)`, at origin.

# Ternary v2.3.2 (2024-07-25)
- Document reading data into R in vignette for new users.
- Provide package anchors in documentation pages.

# Ternary v2.3.1 (2024-02-06)
- Improve support for `region` parameter.

# Ternary v2.3.0 (2024-01-08)
- `region` parameter plots a sub-region of the ternary space.
- Support `...`  in `TernaryContour(Func = function(...))`
   ([#81](https://github.com/ms609/Ternary/issues/81)).

# Ternary v2.2.1 (2023-06-28)
- `Polygon-Geometry` now imported from PlotTools. Aliases of these functions
  are provided but will be removed in a later version of this package.
- Use characters, not numerics, for version number checks
  (see <https://bugs.r-project.org/show_bug.cgi?id=18548>).

# Ternary v2.2.0 (2023-05-11)
- New function `Annotate()` annotates points on ternary plot.
- New option `filled` to produce filled contour lines in `TernaryContour()`
  ([#69](https://github.com/ms609/Ternary/issues/69)).
- Improve clarity of point size plotting example.
- Native colour bar support in `ColourTernary()`
   ([#66](https://github.com/ms609/Ternary/issues/66)).
- Require R 3.5 (due to dependency on "rlang").

# Ternary v2.1.3 (2023-02-20)
- New [vignette](https://ms609.github.io/Ternary/articles/annotation.html)
  with annotation example ([#64](https://github.com/ms609/Ternary/issues/64)).

# Ternary v2.1.2 (2022-11-07)
- Support independent labelling of axes.

# Ternary v2.1.1 (2022-10-04)
- Fix regression that caused `col` parameter to be ignored.
- Fix failure to plot grid when `grid.lines` is integer.
- Mention `srt` graphical parameter in `TernaryText()` documentation.

# Ternary v2.1.0 (2022-05-09)
- Colour sub-region of ternary plot with `ColourTernary()`:
  - Add `Polygon-Geometry` functions for polygon manipulation.
  - Support `NA` values in `ColourTernary()`.
- Tidy code formatting and catch typos.

# Ternary v2.0.0 (2022-01-05)
- Create Holdridge plots using `HoldridgePlot()` and related functions
  ([#48](https://github.com/ms609/Ternary/issues/48)).
- Add `panel.first` and `panel.last` parameters to `TernaryPlot()`
  (fixing [#54](https://github.com/ms609/Ternary/issues/54)).
- Support graphical parameters (e.g. `xpd`) in `TernaryPlot()`.
- Add introductory vignette for new R users.
- Help diagnose non-installation of app.

# Ternary v1.2.4 (2021-12-03)
- Default to upward direction if `TernaryPlot()` not yet called.
- Improved input checking in `TernaryPointValues()` and `TernaryContour()`,
  ([#53](https://github.com/ms609/Ternary/issues/53)),
  and avoid unnecessary calculations.

# Ternary v1.2.3 (2021-10-08)
- Fix bug in `TernaryApp()` when 'colourpicker' not previously installed.
- Update to 'testthat' edition 3.

# Ternary v1.2.2 (2021-05-12)
- New [vignette](https://ms609.github.io/Ternary/articles/interpolation.html)
  with interpolation example ([#46](https://github.com/ms609/Ternary/issues/46)).
- Document point styling in vignette.
- `TernaryCoords()`, alias `TernaryToXY()`, accepts matrices.

# Ternary v1.2.1 (2020-12-09)
- Use package 'vdiffr' conditionally in tests.
- Trivial documentation improvements.

# Ternary v1.2.0 (2020-10-17)
- Add Shiny user interface; launch with `TernaryApp()`.
- Optional parameter `axis.rotate` to allow axis labels to be plotted 'unrotated' 
   (https://github.com/ms609/Ternary/issues/38).
- Fix error in `xtip.rotate` (thanks @LSanselme).
- Add examples to documentation.

# Ternary v1.1.4 (2020-02-27)
- Support user-specified colours in `ColourTernary()`.
- More options for controlling axis colours in `TernaryPlot()`.
- Add examples to documentation.
- axis.labels example in vignette.

# Ternary v1.1.3 (2019-12-04)
- Use `inherits`, not `class` [https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again].
- Obey user-specified `direction` in `TernaryContour()` and 
  `TernaryPointValues()` (thanks @pkR-pjR).

# Ternary v1.1.2 (2019-09-16)
 - `clockwise` parameter allows plots to be configured in 'flipped' orientation
   (https://github.com/ms609/Ternary/issues/22)

# Ternary v1.1.1 (2019-06-17)
 - `TernaryArrows` adds arrows to a ternary plot.

# Ternary v1.1.0 (2019-04-16)
## New features
 - Add support for contour plots and density shading.
 
## Bug fixes 
 - Maintain isometry when one of `xlim` or `ylim` is specified.
 - Fix issues with `TernaryPlot()` documentation.
 - Remove names in `TernaryCoords()`.

# Ternary v1.0.2 (2018-10-31)
 - Improve configuration options by allowing:
   - Custom colouration of axis text;
   - Modification of axis tick length.

# Ternary v1.0.1 (2018-07-05)
 - Plot minor lines between grid lines, with options to customise appearance.
 - Document magnification options.

# Ternary v1.0.0 (2017-12-21)
 - Add function `JoinTheDots()` to plot lines and points simultaneously.
 - Support `xlim` & `ylim` for `TernaryPlot()`.
 - Choose direction of plot using new `point` parameter.
 - Allow labelling of tips and of sides (`alab` becomes `atip`).
 - Improved control over labelling of plot (`clab.rotate` parameter).
 - Improve documentation.

# Ternary v0.1.1 (2017-11-23)
 - Added colour-blind compatible colour palettes `cbPalette8` and `cbPalette15`.
 - Support `lab.font` in `TernaryPlot()`.

# Ternary v0.1.0 (2017-11-23)
 - Generates ternary plots with vertical left axis.
