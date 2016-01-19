#' Init a stage
#'
#' Add a call to this functions as first statement to each of your scripts,
#' using double-colon notation (\code{darn::init()}).  A script without such a
#' call is considered to have no dependencies.
#'
#' @param ... Dependencies
#' @name init
#' @inheritParams init_
NULL

#' @importFrom tibble lst
get_path_info <- function(path = NULL) {
  if (is.null(path)) {
    path <- kimisc::thisfile()
    path <- gsub("(.*)[.]spin[.]Rmd$", "\\1.R", path) ## HACK HACK HACK
  }

  source_dir <- dirname(path)

  root <- root_file_path(path = source_dir)
  config <- read_config(root)
  out_dir <- config[["out_dir"]] %||% "."

  env_vals <- get_env_vals(config)
  env_dir <- get_env_dir(env_vals)

  relative_source_dir <- relative_to(source_dir, root)

  target_dir <- file.path(root, out_dir, env_dir, relative_source_dir)

  target_base <- file.path(target_dir, strip_extension(basename(path)))

  lst(path, source_dir, root, target_dir, target_base, env_vals)
}

get_env_vals <- function(config) {
  env_vars <- strsplit(config[["env_vars"]] %||% "", "\\s+")[[1L]]
  env_vars <- env_vars[env_vars != ""]

  if (length(env_vars) == 0L) {
    return()
  }

  env_vals <- Sys.getenv(env_vars, names = TRUE)
  empty_env_vals <- env_vals[env_vals == ""]
  if (length(empty_env_vals) > 0L) {
    stop("Undefined or empty environment variables: ",
         paste(names(empty_env_vals), collapse = ", "), call. = FALSE)
  }

  lapply(env_vals, type.convert, as.is = TRUE, numerals = "allow.loss")
}

get_env_dir <- function(env_vals) {
  # Special case to work around file.path behavior for zero-length input
  if (length(env_vals) == 0L) {
    return("")
  }

  env_dir_parts <- mapply(function(name, value) paste0(name, "=", "value"),
                          names(env_vals), env_vals)
  env_dir <- do.call(file.path, as.list(env_dir_parts))
}

#' @export
#' @param .dots \code{[list]}\cr
#'   Additional dependencies as list
#' @param env_vars \code{[character]}\cr
#'   Environment variables that define the configuration of the script
#' @rdname init
init_ <- function(..., .dots = NULL, env_vars = NULL,
                  envir = parent.frame()) {
  path_info <- get_path_info()

  check_env_vals(env_vars, path_info)

  deps_list <- get_init_deps_list(.dots, ...)

  mapply(init_one, names(deps_list), deps_list, MoreArgs = list(
    path_info = path_info, envir = envir))

  assign_env_vals(path_info$env_vals, envir)
}

check_env_vals <- function(env_vars, path_info) {
  extra_env_vars <- setdiff(env_vars, names(path_info$env_vals))
  if (length(extra_env_vars) > 0L) {
    stop("Define all environment variables in the ", CONFIG_FILE_NAME,
         ", missing: ", paste(extra_env_vars, collapse =", "), ".",
         call. = FALSE)
  }
}

assign_env_vals <- function(env_vals, envir) {
  defined_env_vars <- intersect(names(env_vals), ls(envir))
  if (length(defined_env_vars) > 0L) {
    stop("Name clash for environment variables: ",
         paste0(defined_env_vars, collapse = ", "), call. = FALSE)
  }

  mapply(assign, names(env_vals), env_vals, MoreArgs = list(envir = envir))
}

get_init_deps_list <- function(.dots, ...) {
  dots <- lazyeval::all_dots(.dots, ...)
  vals <- lazyeval::lazy_eval(dots)

  if (is.null(names(vals))) {
    names(vals) <- rep("", length(vals))
  }

  if (any(names(vals) != "")) {
    stop("Naming not yet implemented.")
  }

  deps <- unlist(vals)
  deps_list <- vector("list", length(deps))
  names(deps_list) <- paste0(deps, ".R")

  deps_list
}

init_one <- function(r_file_name, deps, path_info, envir) {
  r_file <- file.path(path_info$source_dir, r_file_name)
  file_base <- strip_extension(r_file_name)

  rdx_base <- file.path(path_info$target_dir, file_base)
  rdx_file <- sprintf("%s.rdx", rdx_base)

  info <- file.info(r_file, rdx_file)

  if (is.na(info$mtime[[2]]) || diff(info$mtime) < 0) {
    if (Sys.getenv("MAKEFLAGS") == "") {
      source(r_file, local = TRUE)
    } else {
      stop("Not running ", r_file, " from within make. Check dependencies")
    }
  }

  stopifnot(file.exists(rdx_file))

  lazyLoad(rdx_base, envir = envir)
}

#' @export
#' @inheritParams init_
init <- lazyforward::lazyforward("init_")

#' Write the results of a stage
#'
#' Add a call to this functions as last statement to each of your scripts,
#' using double-colon notation (\code{darn::done()}).  A script without such a
#' call is considered to have no output and cannot be a dependency of another
#' script.
#'
#' @param ... Objects to save
#' @name done
NULL

#' @export
#' @param .dots \code{list}\cr
#'   Additional objects as list
#' @param .compress \code{logical(1)}\cr
#'   Compress output (default: \code{FALSE})
#' @rdname done
done_ <- function(..., .dots = NULL, .compress = FALSE) {
  path_info <- get_path_info()

  dots <- get_done_dots(.dots, ...)
  vals <- lazyeval::lazy_eval(dots)

  dir.create(path_info$target_dir, showWarnings = FALSE, recursive = TRUE)
  tools:::makeLazyLoadDB(vals, path_info$target_base, compress = .compress)
}

get_done_dots <- function(.dots, ...) {
  lazyeval::all_dots(.dots, ..., all_named = TRUE)
}

#' @export
#' @inheritParams done_
done <- lazyforward::lazyforward("done_")
