context("make")

if (TEST_MAKE) {

  withr::with_temp_libpaths({
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE)
    expect_null(NULL)

    test_that("can make simple project", {
      f <- setup_scenario("simple")

      create_makefile(f())
      expect_false(file.exists(f("Dependencies")))

      withr::with_envvar(
        c(R_LIBS=paste(.libPaths(), collapse = ":")),
        {
          expect_equal(run_make("-C", f(), "B.rdx"), 0L)
        }
      )
      expect_true(file.exists(f("Dependencies")))
      expect_true(file.exists(f("B.rdx")))
    })

    test_that("can make out_dir project", {
      f <- setup_scenario("out_dir")

      create_makefile(f())
      withr::with_envvar(
        c(R_LIBS=paste(.libPaths(), collapse = ":")),
        {
          #withr::with_dir(f(), system("xterm"))
          expect_equal(run_make("-C", f(), "out/B.rdx"), 0L)
        }
      )
      expect_true(file.exists(f("out/B.rdx")))
    })

    test_that("can make subdir project", {
      f <- setup_scenario("subdir", unlink_darnfile = TRUE)

      create_makefile(f(), src_dir = "dir")
      withr::with_envvar(
        c(R_LIBS=paste(.libPaths(), collapse = ":")),
        {
          #withr::with_dir(f(), system("xterm"))
          expect_equal(run_make("-C", f(), "dir/B.rdx"), 0L)
        }
      )
      expect_true(file.exists(f("dir/B.rdx")))
    })

    test_that("can make subdir project with out_dir", {
      f <- setup_scenario("subdir", unlink_darnfile = TRUE)

      create_makefile(f(), src_dir = "dir", out_dir = "out")
      withr::with_envvar(
        c(R_LIBS=paste(.libPaths(), collapse = ":")),
        {
          #withr::with_dir(f(), system("xterm"))
          expect_equal(run_make("-C", f(), "out/dir/B.rdx"), 0L)
        }
      )
      expect_true(file.exists(f("out/dir/B.rdx")))
    })

    test_that("can make env project", {
      f <- setup_scenario("env")

      create_makefile(f())
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

} # if (TEST_MAKE) {
