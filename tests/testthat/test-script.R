context("script")

test_that("can create script", {
  f <- setup_scenario("simple")

  create_script(f(), f("darn.R"))
  expect_identical(readLines(f("darn.R")), run_call(c("A.R", "B.R")))
})
