test_that("Submodule checked out", {
  expect_false(system.file("TernaryApp", "app.R", package = "Ternary") == "")
})
