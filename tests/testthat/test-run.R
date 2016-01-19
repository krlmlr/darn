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

test_that("can run single script with different output directory", {
  f <- setup_scenario("out_dir")

  expect_error(source(f("A.R"), local = TRUE), NA)
  expect_true(file.exists(f("out/A.rdb")))
  expect_true(file.exists(f("out/A.rdx")))

  env <- new.env()
  lazyLoad(f("out/A"), envir = env)
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

test_that("can run script with env vars", {
  f <- setup_scenario("env")

  expect_error(source(f("src/B.R"), local = TRUE), "FORTYTWO")
  withr::with_envvar(
    c(FORTYTWO = "42", TWENTYONE = "21"),
    expect_error(source(f("src/B.R"), local = TRUE), NA)
  )

  expect_true(file.exists(f("out/FORTYTWO=42/TWENTYONE=21/src/A.rdb")))
  expect_true(file.exists(f("out/FORTYTWO=42/TWENTYONE=21/src/A.rdx")))
  expect_true(file.exists(f("out/FORTYTWO=42/TWENTYONE=21/src/B.rdb")))
  expect_true(file.exists(f("out/FORTYTWO=42/TWENTYONE=21/src/B.rdx")))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO=42/TWENTYONE=21/src/A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 42L))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO=42/TWENTYONE=21/src/B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 21L))
})
