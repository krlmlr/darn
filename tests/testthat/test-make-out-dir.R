context("make-out-dir")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make out_dir project", {
    f <- setup_scenario("out_dir")

    expect_warning(create_makefile(f()), "Not overwriting")
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f(), "out/B.rdx"), 0L)
      }
    )
    expect_true(file.exists(f("out/B.rdx")))
  })
})
