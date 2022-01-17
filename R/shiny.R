#' Graphical user interface for creating ternary plots
#'
#' `TernaryApp()` launches a 'Shiny' application for the construction of
#' ternary plots.  The 'app' allows data to be loaded and plotted, and provides
#' code to reproduce the plot in R should more sophisticated plotting functions
#' be desired.
#'
#' ## Load data
#'
#' The 'Load data' input tab allows for the upload of datasets.
#' Data can be read from csv files, `.txt` files created with `write.table()`,
#' or (if the 'readxl' package is installed) Excel spreadsheets.
#'
#' Data should be provided as three columns, corresponding to the three axes
#' of the ternary plot.  Colours or point styles may be specified in columns
#' four to six to allow different categories of point to be plotted distinctly.
#' Example datasets are installed at
#' `system.file("TernaryApp", package = "Ternary")`.
#'
#' Axes are automatically labelled using column names, if present; these can be
#' edited manually on this tab.
#'
#' ## Plot display
#'
#' Allows the orientation, colour and configuration of the plot and its axes
#' to be adjusted,
#'
#' ## Grids
#'
#' Adjust the number, spacing and styling of major and minor grid lines.
#'
#' ## Labels
#'
#' Configure the colour, position and size of tip and axis labels.
#'
#' ## Points
#'
#' Choose whether to plot points, lines, connected points, or text.
#' Set the style of points and lines.
#'
#'
#' # Exporting plots
#'
#' A plot can be saved to PDF or as a PNG bitmap at a specified size.
#' Alternatively, R script that will generate the displayed plot can be viewed
#' (using the 'R code' output tab) or downloaded to file.
#'
#'
#' @seealso
#' Full detail of plotting with 'Ternary', including features not (yet)
#' implemented in the application, is provided in the accompanying
#' [vignette](https://ms609.github.io/Ternary/articles/Ternary.html).
#'
#' @references
#' If you use figures produced with this package in a publication, please cite
#'
#' Smith, Martin R. (2017). _Ternary: An R Package for Creating Ternary Plots._
#' Zenodo, doi: \doi{10.5281/zenodo.1068996}.
#'
#' @importFrom shiny runApp runGitHub
#' @template MRS
#' @export
TernaryApp <- function() {
  appDir <- system.file("TernaryApp", package = "Ternary")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing 'Ternary'.",
      call. = FALSE
    )
  }
  if (system.file("TernaryApp", "app.R", package = "Ternary") == "") {
    warning(
      "App not installed. To install locally:\r\n - Download ",
      "https://github.com/ms609/TernaryApp/archive/refs/heads/master.zip\r\n - ",
      "Unzip contents to ", appDir
    )
    shiny::runGitHub("ms609/TernaryApp")
  } else {
    shiny::runApp(appDir, display.mode = "normal")
  }
}
