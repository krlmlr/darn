context("run")

test_that("multiplication works", {
  expect_error(source("simple/A.R", local = TRUE), NA)
})
