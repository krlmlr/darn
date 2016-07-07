TEST_MAKE_VERBOSE <- TRUE

TEST_MAKE_VERBOSE <- FALSE

test_scenario <- function(scenario_name, src_dir, out_dir, unlink_darnfile, makefile_warning) {
  f <- setup_scenario(scenario_name, unlink_darnfile)

  expect_warning(create_makefile(f(), src_dir = src_dir, out_dir = out_dir), makefile_warning)

  target_dir <- file.path(out_dir, src_dir)

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
}

setup_scenario <- function(path, unlink_darnfile = FALSE) {
  target <- file.path(tempfile("darn"))
  dir.create(target)
  file.copy(path, target, recursive = TRUE)
  if (unlink_darnfile) {
    file.remove(file.path(target, path, CONFIG_FILE_NAME))
  }
  path_base <- basename(path)
  eval(bquote(function(...) file.path(.(target), .(path_base), ...)))
}

run_make <- function(...) {
  if (TEST_MAKE_VERBOSE) {
    system2("make", args = c(...))
  } else {
    system2("make", args = c(...), stdout = NULL, stderr = NULL)
  }
}
