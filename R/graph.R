#' Dependency graph
#'
#' TBD.
#'
#' @inheritParams create_dep_file
#' @export
dep_graph <- function(root_dir) {
  require_suggested("graph")

  config <- read_config(root_dir)
  src_dir <- file_path(root_dir, config[["src_dir"]])
  web <- get_web_from_src(root_dir, src_dir)

  deps <- get_deps(web)
  V <- names(deps)
  E <- lapply(deps, names)
  EL <- lapply(E, `%||%`, character())
  g <- graph::graphNEL(V, EL, edgemode = "directed")
  graph::reverseEdgeDirections(g)
}

get_order <- function(root_dir) {
  require_suggested("igraph")

  g <- dep_graph(root_dir)
  ig <- igraph::graph_from_graphnel(g)

  if (!igraph::is_dag(ig)) {
    stop("Dependency cycle detected", call. = FALSE)
  }

  names(igraph::topo_sort(ig))
}

#' Creates a script that will run all scripts in the network
#'
#' The scripts will be run in an order that guarantees that all dependencies
#' are available when needed.
#'
#' @inheritParams create_dep_file
#' @param filename \code{[character(1)]}\cr
#'   The file name of the script to be created
#'
#' @export
create_script <- function(root_dir, filename = "darn.R") {
  order <- get_order(root_dir)
  writeLines(run_call(order), filename)
}
