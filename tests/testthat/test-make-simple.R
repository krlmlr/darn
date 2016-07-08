context("make-simple")

withr::with_libpaths(future::value(temp_lib), action = "prefix", {
  test_that("can make simple project", {
    scenario_name <- "simple"
    src_dir <- "."
    out_dir <- "."
    unlink_darnfile <- FALSE
    makefile_warning <- "Not overwriting"
    test_scenario(scenario_name, src_dir, out_dir, unlink_darnfile, makefile_warning)
  })
})
