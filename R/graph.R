#' Dependency graph
#'
#' TBD.
#'
#' @inheritParams create_dep_file
#' @export
dep_graph <- function(root_dir) {
  require_suggested("graph")

  config <- read_config(root_dir)
  web <- parse_script(dir(file.path(root_dir, config[["src_dir"]] %||% "."),
                          pattern = R_FILE_PATTERN, full.names = TRUE),
                      base_dir = root_dir)
  deps <- get_deps(web)
  V <- names(deps)
  E <- lapply(deps, names)
  EL <- lapply(E, `%||%`, character())
  graph::graphNEL(V, EL, edgemode = "directed")
}