context("make-out-dir-subdir")

withr::with_temp_libpaths({
  test_that("prepare: install", {
    devtools::install(dependencies = FALSE, upgrade_dependencies = FALSE, quiet = TRUE, quick = TRUE)
    expect_null(NULL)
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
})
