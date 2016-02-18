.darn_env <- new.env(parent = emptyenv())

#' @export
run <- function(file, fun, ...) {
  .darn_env$current_file <- normalizePath(file)
  on.exit(rm(current_file, .darn_env))
  fun(file, ...)
}
