context("make-simple")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make simple project", {
    scenario_name <- "simple"
    out_dir <- "."
    f <- setup_scenario(scenario_name)

    expect_warning(create_makefile(f()), "Not overwriting")

    expect_false(file.exists(f("Dependencies")))

    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_true(file.exists(f("Dependencies")))
    expect_true(file.exists(f(out_dir, "B.rdx")))

    writeLines("darn::done()", f("C.R"))
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_true(file.exists(f(out_dir, "C.rdx")))

    unlink(f("B.R"))
    unlink(f(out_dir, c("B.rdb", "B.rdx")))
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
    expect_false(all(grepl("B[.]R", readLines(f("Dependencies")))))

    expect_lt_time(file.info(f("A.R"))$mtime, file.info(f("Dependencies"))$mtime)
    writeLines(readLines(f("A.R")), f("A.R"))
    expect_gt_time(file.info(f("A.R"))$mtime, file.info(f("Dependencies"))$mtime)
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f(), "--dry-run"), 0L)
      }
    )
    expect_lt_time(file.info(f("A.R"))$mtime, file.info(f("Dependencies"))$mtime)
  })
})
