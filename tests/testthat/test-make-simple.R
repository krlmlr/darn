context("make-simple")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make simple project", {
    f <- setup_scenario("simple")

    expect_warning(create_makefile(f()), "Not overwriting")

    expect_false(file.exists(f("Dependencies")))

    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f(), "B.rdx"), 0L)
      }
    )
    expect_true(file.exists(f("Dependencies")))
    expect_true(file.exists(f("B.rdx")))
  })
})
