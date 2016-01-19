`%||%` <- function(a, b) if (is.null(a)) b else a

`%nin%` <- function(a, b) !(a %in% b)

strip_extension <- function(path) {
  gsub("[.][^.]*$", "", path)
}

relative_to <- function(path, root) {
  if (length(path) == 0L) {
    return ()
  }

  R.utils::getRelativePath(path, root)
}
