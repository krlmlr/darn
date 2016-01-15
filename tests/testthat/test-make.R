context("make")

withr::with_temp_libpaths({
  devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE)
  expect_null(NULL)

  test_that("can make simple project", {
    f <- setup_scenario("simple")

    create_makefile(f())
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        expect_equal(run_make("-C", f(), "B.rdx"), 0L)
      }
    )
    expect_true(file.exists(f("B.rdx")))
  })
})
