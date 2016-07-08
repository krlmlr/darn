context("make-out-dir-subdir")

withr::with_libpaths(future::value(temp_lib), action = "prefix", {
  test_that("can make subdir project with out_dir", {
    scenario_name <- "subdir"
    src_dir <- "dir"
    out_dir <- "out"
    unlink_darnfile <- TRUE
    makefile_warning <- NA
    test_scenario(scenario_name, src_dir, out_dir, unlink_darnfile, makefile_warning)
  })
})
