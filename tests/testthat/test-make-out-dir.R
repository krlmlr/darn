context("make-out-dir")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make out_dir project", {
    scenario_name <- "out_dir"
    src_dir <- "."
    out_dir <- "out"
    unlink_darnfile <- FALSE
    makefile_warning <- "Not overwriting"
    test_scenario(scenario_name, src_dir, out_dir, unlink_darnfile, makefile_warning)
  })
})
