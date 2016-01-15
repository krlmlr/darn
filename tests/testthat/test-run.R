context("run")

test_that("can run single script", {
  f <- setup_scenario("simple")

  expect_error(source(f("A.R"), local = TRUE), NA)
  expect_true(file.exists(f("A.rdb")))
  expect_true(file.exists(f("A.rdx")))

  env <- new.env()
  lazyLoad(f("A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 42))
})

test_that("can run script with dependency", {
  f <- setup_scenario("simple")

  expect_error(source(f("B.R"), local = TRUE), NA)
  expect_true(file.exists(f("A.rdb")))
  expect_true(file.exists(f("A.rdx")))
  expect_true(file.exists(f("B.rdb")))
  expect_true(file.exists(f("B.rdx")))

  env <- new.env()
  lazyLoad(f("B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 21))
})

test_that("can run script in subdir", {
  f <- setup_scenario("subdir")

  expect_error(source(f("dir/B.R"), local = TRUE), NA)
  expect_true(file.exists(f("dir/A.rdb")))
  expect_true(file.exists(f("dir/A.rdx")))
  expect_true(file.exists(f("dir/B.rdb")))
  expect_true(file.exists(f("dir/B.rdx")))

  env <- new.env()
  lazyLoad(f("dir/B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 21))
})
