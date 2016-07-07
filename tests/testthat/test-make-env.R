context("make-env")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
  })

  test_that("can make env project", {
    f <- setup_scenario("env")

    expect_warning(create_makefile(f()), "Not overwriting")
    withr::with_envvar(
      c(R_LIBS=paste(.libPaths(), collapse = ":")),
      {
        expect_equal(run_make("-C", f(), "Dependencies"), 0L)
        expect_true(file.exists(f("Dependencies")))
        #withr::with_dir(f(), system("xterm"))
        dep_file <- readLines(f("Dependencies"))

        expect_equal(run_make("-C", f()), 0L)

        withr::with_envvar(
          c(FORTYTWO="44", TWENTYONE="23"),
          {
            #withr::with_dir(f(), system("xterm"))
            expect_equal(run_make("-C", f()), 0L)
          }
        )
        expect_identical(dep_file, readLines(f("Dependencies")))

        withr::with_envvar(
          c(FORTYTWO="43", TWENTYONE="22"),
          {
            #withr::with_dir(f(), system("xterm"))
            expect_equal(run_make("-C", f()), 0L)
          }
        )
        expect_identical(dep_file, readLines(f("Dependencies")))
      }
    )

    expect_true(file.exists(f("out/FORTYTWO-42/TWENTYONE-21/src/B.rdx")))
    expect_true(file.exists(f("out/FORTYTWO-43/TWENTYONE-22/src/B.rdx")))
    expect_true(file.exists(f("out/FORTYTWO-44/TWENTYONE-23/src/B.rdx")))
  })
})
