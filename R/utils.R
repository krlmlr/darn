strip_extension <- function(path) {
  gsub("[.][^.]*$", "", path)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
