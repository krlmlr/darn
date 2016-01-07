context("deps")

test_that("parsing", {
  expect_warning(A <- parse_script("simple/A.R"), NA)
  expect_identical(A$path, normalizePath("simple/A.R"))
  expect_identical(A$init, list(deps = NULL))
  expect_identical(A$done, list(
    names = "fortytwo"
  ))

  expect_warning(B <- parse_script("simple/B.R"), NA)
  expect_identical(B$path, normalizePath("simple/B.R"))
  expect_identical(B$init, list(
    deps = list(
      A = NULL
    )
  ))
  expect_identical(B$done, list(
    names = "twentyone"
  ))
})
