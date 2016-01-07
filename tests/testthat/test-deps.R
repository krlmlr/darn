context("deps")

test_that("parsing", {
  expect_warning(parsed <- parse_script(dir("simple", full.names = TRUE)), NA)

  A <- parsed[["simple/A.R"]]
  expect_identical(A$path, normalizePath("simple/A.R"))
  expect_identical(A$init, list(deps = NULL))
  expect_identical(A$done, list(
    names = "fortytwo"
  ))

  B <- parsed[["simple/B.R"]]
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
