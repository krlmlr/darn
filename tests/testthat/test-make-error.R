context("make-env")

withr::with_libpaths(future::value(temp_lib), action = "prefix", {
  test_that("errors in source file are propagated when using make", {
    f <- setup_scenario("error")

    create_makefile(f())

    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f(), "Dependencies"), 0L)
      }
    )

    # First invocation raises error
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 2L)
      }
    )

    patch_oops(f("A.R"))

    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        #withr::with_dir(f(), system("xterm"))
        expect_equal(run_make("-C", f()), 0L)
      }
    )
  })
})
