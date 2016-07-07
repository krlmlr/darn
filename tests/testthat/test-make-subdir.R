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
    f <- setup_scenario(scenario_name, unlink_darnfile)

    target_dir <- file.path(out_dir, src_dir)

    expect_warning(create_makefile(f(), src_dir = src_dir, out_dir = out_dir), makefile_warning)

    expect_false(file.exists(f("Dependencies")))

    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_true(file.exists(f("Dependencies")))
    expect_true(file.exists(f(target_dir, "B.rdx")))

    writeLines("darn::done()", f(src_dir, "C.R"))
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_true(file.exists(f(target_dir, "C.rdx")))

    unlink(f("B.R"))
    unlink(f(target_dir, c("B.rdb", "B.rdx")))
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_false(all(grepl("B[.]R", readLines(f("Dependencies")))))

    expect_lt_time(file.info(f(src_dir, "A.R"))$mtime, file.info(f("Dependencies"))$mtime)
    writeLines(readLines(f(src_dir, "A.R")), f(src_dir, "A.R"))
    expect_gt_time(file.info(f(src_dir, "A.R"))$mtime, file.info(f("Dependencies"))$mtime)
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f(), "--dry-run"), 0L)
      }
    )
    expect_lt_time(file.info(f(src_dir, "A.R"))$mtime, file.info(f("Dependencies"))$mtime)
  })
})
