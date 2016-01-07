context("run")

test_that("multiplication works", {
  f <- setup_scenario("simple")

  expect_error(source(f("A.R"), local = TRUE), NA)
  expect_true(file.exists(f("A.rdb")))
  expect_true(file.exists(f("A.rdx")))

  env <- new.env()
  lazyLoad(f("A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 42))
})
