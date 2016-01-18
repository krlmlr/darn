context("deps")

test_that("parsing", {
  withr::with_dir("simple", {
    expect_warning(parsed <- parse_script(dir(pattern = "[.][rR]$")), NA)

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
    web <- parse_script(dir(pattern = "[.][rR]$"))
    expect_identical(
      get_deps(web),
      list("A.R" = NULL, "B.R" = list("A.R" = NULL))
    )
  })
})

test_that("deps in subdir", {
  web <- parse_script(dir("simple", pattern = "[.][rR]$", full.names = TRUE))
  expect_identical(
    get_deps(web),
    list("simple/A.R" = NULL, "simple/B.R" = list("simple/A.R" = NULL))
  )
})

test_that("dep rules in subdir", {
  rules <- create_deps_rules("simple", ".")
  expect_true(
    "all: simple/A.rdx simple/B.rdx" %in% format(rules))
  expect_true(
    "simple/B.rdx: simple/A.rdx" %in% format(rules))
})

test_that("dep file, by default into file named Dependencies", {
  f <- setup_scenario("simple")
  create_dep_file(f())
  dep_contents <- readLines(f("Dependencies"))
  expect_true("all: A.rdx B.rdx" %in% dep_contents)
  expect_true("B.rdx: A.rdx" %in% dep_contents)
})
