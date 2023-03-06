# Ternary v2.1.3.9000
- Colour bar legend example for `ColourTernary()` in vignette
   ([#66](https://github.com/ms609/Ternary/issues/66)).

# Ternary v2.1.3
- New [vignette](https://ms609.github.io/Ternary/articles/annotation.html)
  with annotation example ([#64](https://github.com/ms609/Ternary/issues/64)).

# Ternary v2.1.2
- Support independent labelling of axes.

# Ternary v2.1.1
- Fix regression that caused `col` parameter to be ignored.
- Fix failure to plot grid when `grid.lines` is integer.
- Mention `srt` graphical parameter in `TernaryText()` documentation.

# Ternary v2.1.0
- Colour sub-region of ternary plot with `ColourTernary()`:
  - Add `Polygon-Geometry` functions for polygon manipulation.
  - Support `NA` values in `ColourTernary()`.
- Tidy code formatting and catch typos.

# Ternary v2.0.0
- Create Holdridge plots using `HoldridgePlot()` and related functions
  ([#48](https://github.com/ms609/Ternary/issues/48)).
- Add `panel.first` and `panel.last` parameters to `TernaryPlot()`
  (fixing [#54](https://github.com/ms609/Ternary/issues/54)).
- Support graphical parameters (e.g. `xpd`) in `TernaryPlot()`.
- Add introductory vignette for new R users.
- Help diagnose non-installation of app.

# Ternary v1.2.4
- Default to upward direction if `TernaryPlot()` not yet called.
- Improved input checking in `TernaryPointValues()` and `TernaryContour()`,
  ([#53](https://github.com/ms609/Ternary/issues/53)),
  and avoid unnecessary calculations.

# Ternary v1.2.3
- Fix bug in `TernaryApp()` when 'colourpicker' not previously installed.
- Update to 'testthat' edition 3.

# Ternary v1.2.2
- New [vignette](https://ms609.github.io/Ternary/articles/interpolation.html)
  with interpolation example ([#46](https://github.com/ms609/Ternary/issues/46)).
- Document point styling in vignette.
- `TernaryCoords()`, alias `TernaryToXY()`, accepts matrices.

# Ternary v1.2.1
- Use package 'vdiffr' conditionally in tests.
- Trivial documentation improvements.

# Ternary v1.2.0
- Add Shiny user interface; launch with `TernaryApp()`.
- Optional parameter `axis.rotate` to allow axis labels to be plotted 'unrotated' 
   (https://github.com/ms609/Ternary/issues/38).
- Fix error in `xtip.rotate` (thanks @LSanselme).
- Add examples to documentation.

# Ternary v1.1.4
- Support user-specified colours in `ColourTernary()`.
- More options for controlling axis colours in `TernaryPlot()`.
- Add examples to documentation.
- axis.labels example in vignette.

# Ternary v1.1.3
- Use `inherits`, not `class` [https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again].
- Obey user-specified `direction` in `TernaryContour()` and 
  `TernaryPointValues()` (thanks @pkR-pjR).

# Ternary v1.1.2
 - `clockwise` parameter allows plots to be configured in 'flipped' orientation
   (https://github.com/ms609/Ternary/issues/22)

# Ternary v1.1.1
 - `TernaryArrows` adds arrows to a ternary plot.

# Ternary v1.1.0
## New features
 - Add support for contour plots and density shading.
 
## Bug fixes 
 - Maintain isometry when one of `xlim` or `ylim` is specified.
 - Fix issues with `TernaryPlot()` documentation.
 - Remove names in `TernaryCoords()`.

# Ternary v1.0.2
 - Improve configuration options by allowing:
   - Custom colouration of axis text;
   - Modification of axis tick length.

# Ternary v1.0.1
 - Plot minor lines between grid lines, with options to customise appearance.
 - Document magnification options.

# Ternary v1.0.0
 - Add function `JoinTheDots()` to plot lines and points simultaneously.
 - Support `xlim` & `ylim` for `TernaryPlot()`.
 - Choose direction of plot using new `point` parameter.
 - Allow labelling of tips and of sides (`alab` becomes `atip`).
 - Improved control over labelling of plot (`clab.rotate` parameter).
 - Improve documentation.

# Ternary v0.1.1
 - Added colour-blind compatible colour palettes `cbPalette8` and `cbPalette15`.
 - Support `lab.font` in `TernaryPlot()`.

# Ternary v0.1.0
 - Generates ternary plots with vertical left axis.
