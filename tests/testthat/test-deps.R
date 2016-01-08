context("deps")

test_that("parsing", {
  withr::with_dir("simple", {
    expect_warning(parsed <- parse_script(dir()), NA)

    A <- parsed[["A.R"]]
    expect_identical(A$path, normalizePath("A.R"))
    expect_identical(A$init, list(deps = NULL))
    expect_identical(A$done, list(
      names = "fortytwo"
    ))

    B <- parsed[["B.R"]]
    expect_identical(B$path, normalizePath("B.R"))
    expect_identical(B$init, list(
      deps = list(
        A.R = NULL
      )
    ))
    expect_identical(B$done, list(
      names = "twentyone"
    ))
  })
})

test_that("deps", {
  withr::with_dir("simple", {
    expect_identical(
      get_deps(dir()),
      list("A.R" = NULL, "B.R" = list("A.R" = NULL))
    )
  })
})

test_that("deps in subdir", {
  expect_identical(
    get_deps(dir("simple", full.names = TRUE)),
    list("simple/A.R" = NULL, "simple/B.R" = list("simple/A.R" = NULL))
  )
})

test_that("dep rules", {
  rules <- create_deps_rules("simple")
  expect_true(any(grepl("simple/B[.]R: simple/A[.]R", format(rules))))
})

test_that("dep file, by default into file named Dependencies", {
  f <- setup_scenario("simple")
  create_dep_file(f())
  dep_contents <- readLines(f("Dependencies"))
  expect_true(any(grepl("B[.]R: A[.]R", dep_contents)))
})
