setup_scenario <- function(path) {
  target <- file.path(tempfile("darg"))
  dir.create(target)
  file.copy(path, target, recursive = TRUE)
  path_base <- basename(path)
  eval(bquote(function(...) file.path(.(target), .(path_base), ...)))
}
