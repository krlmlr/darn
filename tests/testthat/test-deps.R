context("deps")

test_that("parsing", {
  withr::with_dir("simple", {
    expect_warning(parsed <- parse_script(dir(pattern = "[.][rR]$"), "."), NA)

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
    web <- parse_script(dir(pattern = "[.][rR]$"), ".")
    expect_identical(
      get_deps(web),
      list("A.R" = NULL, "B.R" = list("A.R" = NULL))
    )
  })
})


test_that("scripts without done() are not listed in dependencies", {
  f <- setup_scenario("simple")
  withr::with_dir(f(), {
    web1 <- parse_script(dir(pattern = "[.][rR]$"), ".")
    writeLines("# empty script", f("C.R"))
    web2 <- parse_script(dir(pattern = "[.][rR]$"), ".")
    expect_identical(
      get_deps(web1),
      get_deps(web2)
    )
  })
})

test_that("deps in subdir", {
  web <- parse_script(
    dir("subdir", pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE),
    "subdir")

  expect_identical(
    get_deps(web),
    list("dir/A.R" = NULL, "dir/B.R" = list("dir/A.R" = NULL))
  )
})

test_that("dep rules in subdir", {
  rules <- create_deps_rules("subdir", "subdir/dir")
  expect_true(
    "all: A" %in% format(rules))
  expect_true(
    "all: B" %in% format(rules))
  expect_true(
    "dir/B.rdx: A" %in% format(rules))
})

test_that("dep file, by default into file named Dependencies", {
  f <- setup_scenario("simple")
  create_dep_file(f())
  dep_contents <- readLines(f("Dependencies"))
  expect_true("all: A" %in% dep_contents)
  expect_true("all: B" %in% dep_contents)
  expect_true("B.rdx: A" %in% dep_contents)
})
