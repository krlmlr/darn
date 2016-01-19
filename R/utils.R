strip_extension <- function(path) {
  gsub("[.][^.]*$", "", path)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

relative_to <- function(path, root) {
  if (length(path) == 0L) {
    return ()
  }

  R.utils::getRelativePath(path, root)
}
