TEST_MAKE_VERBOSE <- TRUE

TEST_MAKE_VERBOSE <- FALSE

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
