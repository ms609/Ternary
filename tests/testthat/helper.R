expect_doppelganger <- function(title, fig, ...) {
  skip_if_not_installed("vdiffr")
  # Only compare SVG snapshots on the reference platform to avoid

  # cross-architecture floating-point rounding differences
  skip_on_os(c("windows", "linux"))
  vdiffr::expect_doppelganger(title, fig, ...)
}
