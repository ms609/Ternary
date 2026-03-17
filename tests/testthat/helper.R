expect_doppelganger <- function(title, fig, ...) {
  skip_if_not_installed("vdiffr")
  # SVG output differs on ARM due to floating-point rounding; skip there
  skip_if(Sys.info()[["machine"]] %in% c("arm64", "aarch64"))
  vdiffr::expect_doppelganger(title, fig, ...)
}
