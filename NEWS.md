# Ternary v1.2.4.9000

- Add `TernaryContours()`
- Add introductory vignette for new R users.

# Ternary v1.2.4

- Default to upward direction if `TernaryPlot()` not yet called.
- Improved input checking in `TernaryPointValues()` and `TernaryContour()`,
  ([#53](https://github.com/ms609/Ternary/issues/53)),
  and avoid unnecessary calculations.

# Ternary v1.2.3

- Fix bug in `TernaryApp()` when 'colourpicker' not previously installed.
- Update to 'testthat' edition 3.

# Ternary v1.2.2

- `TernaryCoords()`, alias `TernaryToXY()`, accepts matrices.
- New vignette with interpolation example ([#46](https://github.com/ms609/Ternary/issues/46)).
- Document point styling in vignette.

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
