context("deps")

test_that("parsing", {
  expect_warning(A <- parse_script("simple/A.R"), NA)
  expect_identical(A$path, normalizePath("simple/A.R"))
  expect_identical(A$done, list(
    names = "fortytwo"
  ))
})
