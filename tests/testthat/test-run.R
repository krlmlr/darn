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

  source(f("B.R"), local = TRUE)
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

test_that("can run script with default env vars", {
  f <- setup_scenario("env")

  expect_error(source(f("src/B.R"), local = TRUE), NA)

  expect_true(file.exists(f("out/FORTYTWO-42/TWENTYONE-21/src/A.rdb")))
  expect_true(file.exists(f("out/FORTYTWO-42/TWENTYONE-21/src/A.rdx")))
  expect_true(file.exists(f("out/FORTYTWO-42/TWENTYONE-21/src/B.rdb")))
  expect_true(file.exists(f("out/FORTYTWO-42/TWENTYONE-21/src/B.rdx")))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO-42/TWENTYONE-21/src/A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 42L))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO-42/TWENTYONE-21/src/B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 21L))
})

test_that("can run script with overwritten env vars", {
  f <- setup_scenario("env", unlink_darnfile = TRUE)

  create_makefile(f(), src_dir = "src", env_vars = list(FORTYTWO = 44, TWENTYONE = 23))

  expect_error(source(f("src/B.R"), local = TRUE), NA)

  expect_true(file.exists(f("FORTYTWO-44/TWENTYONE-23/src/A.rdb")))
  expect_true(file.exists(f("FORTYTWO-44/TWENTYONE-23/src/A.rdx")))
  expect_true(file.exists(f("FORTYTWO-44/TWENTYONE-23/src/B.rdb")))
  expect_true(file.exists(f("FORTYTWO-44/TWENTYONE-23/src/B.rdx")))

  env <- new.env()
  lazyLoad(f("FORTYTWO-44/TWENTYONE-23/src/A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 44L))

  env <- new.env()
  lazyLoad(f("FORTYTWO-44/TWENTYONE-23/src/B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 23L))
})

test_that("can run script with custom env vars", {
  f <- setup_scenario("env")

  withr::with_envvar(
    c(FORTYTWO = "43", TWENTYONE = "22"),
    expect_error(source(f("src/B.R"), local = TRUE), NA)
  )

  expect_true(file.exists(f("out/FORTYTWO-43/TWENTYONE-22/src/A.rdb")))
  expect_true(file.exists(f("out/FORTYTWO-43/TWENTYONE-22/src/A.rdx")))
  expect_true(file.exists(f("out/FORTYTWO-43/TWENTYONE-22/src/B.rdb")))
  expect_true(file.exists(f("out/FORTYTWO-43/TWENTYONE-22/src/B.rdx")))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO-43/TWENTYONE-22/src/A"), envir = env)
  expect_identical(as.list(env), list(fortytwo = 43L))

  env <- new.env()
  lazyLoad(f("out/FORTYTWO-43/TWENTYONE-22/src/B"), envir = env)
  expect_identical(as.list(env), list(twentyone = 22L))
})

test_that("can run script with custom env vars twice in same environment", {
  f <- setup_scenario("env")

  target_env <- new.env()

  withr::with_envvar(
    c(FORTYTWO = "43", TWENTYONE = "22"),
    expect_error(local(source(f("src/B.R")), envir = target_env), NA)
  )

  withr::with_envvar(
    c(FORTYTWO = "43", TWENTYONE = "22"),
    expect_error(local(source(f("src/B.R")), envir = target_env), NA)
  )

  withr::with_envvar(
    c(FORTYTWO = "44", TWENTYONE = "22"),
    expect_error(local(source(f("src/B.R")), envir = target_env), "Value clash.*FORTYTWO")
  )
})

test_that("running bad script gives error", {
  f <- setup_scenario("error")

  create_makefile(f())

  expect_error(source(f("B.R"), local = TRUE))
  expect_error(source(f("A.R"), local = TRUE))

  patch_oops(f("A.R"))

  expect_error(source(f("B.R"), local = TRUE), NA)
  expect_true(file.exists(f("A.rdb")))
  expect_true(file.exists(f("A.rdx")))
})
