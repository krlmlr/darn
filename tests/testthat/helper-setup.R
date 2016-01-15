setup_scenario <- function(path) {
  target <- file.path(tempfile("darn"))
  dir.create(target)
  file.copy(path, target, recursive = TRUE)
  path_base <- basename(path)
  eval(bquote(function(...) file.path(.(target), .(path_base), ...)))
}

run_make <- function(...) {
  system2("make", args = c(...), stdout = NULL, stderr = NULL)
}
