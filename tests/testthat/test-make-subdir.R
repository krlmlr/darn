context("make-subdir")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make subdir project", {
    scenario_name <- "subdir"
    src_dir <- "dir"
    out_dir <- "."
    unlink_darnfile <- TRUE
    makefile_warning <- NA
    test_scenario(scenario_name, src_dir, out_dir, unlink_darnfile, makefile_warning)
  })
})
