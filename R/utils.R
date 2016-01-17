strip_extension <- function(path) {
  gsub("[.][^.]*$", "", path)
}
