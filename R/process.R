init_ <- function(..., .dots = NULL, env_vars = NULL,
                  envir = parent.frame(), path = NULL) {
  path_info <- get_path_info(path)

  check_env_vals(env_vars, path_info)

  deps_list <- get_init_deps_list(.dots, ...)

  mapply(init_one, names(deps_list), deps_list, MoreArgs = list(
    path_info = path_info, envir = envir))

  assign_env_vals(path_info$env_vals, envir)
  invisible(path_info)
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
  if (length(env_vals) == 0L)
    return()

  defined_env_vars <- mget(names(env_vals), envir, ifnotfound = list(NULL))
  defined_env_vars <- defined_env_vars[!vapply(defined_env_vars, is.null, logical(1L))]

  if (length(defined_env_vars) > 0L) {
    if (!isTRUE(all.equal(env_vals[names(defined_env_vars)], defined_env_vars))) {
      stop("Value clash for environment variables: ",
           paste0(names(defined_env_vars), " = ", unlist(defined_env_vars), collapse = ", "),
           call. = FALSE)
    }
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
  if (length(deps) == 0L) {
    return(NULL)
  }

  deps_list <- vector("list", length(deps))
  names(deps_list) <- paste0(deps, ".R")

  deps_list
}

init_one <- function(r_file_name, deps, path_info, envir) {
  r_file <- file_path(path_info$src_dir, r_file_name)
  file_base <- strip_extension(r_file_name)

  rdx_base <- file_path(path_info$target_dir, file_base)
  rdx_file <- sprintf("%s.rdx", rdx_base)

  info <- file.info(r_file, rdx_file)

  if (is.na(info$mtime[[2]]) || diff(info$mtime) < 0) {
    if (Sys.getenv("MAKEFLAGS") == "") {
      old_search <- search()
      source(r_file, local = TRUE)
      if (!identical(search(), old_search)) {
        warning("Script ", r_file, " changed search path.", call. = FALSE)
      }
    } else {
      stop("Not running ", r_file, " from within make. Check dependencies",
           call. = FALSE)
    }
  }

  stopifnot(file.exists(rdx_file))

  lazyLoad(rdx_base, envir = envir)
}

#' Init a stage
#'
#' Add a call to this functions as first statement to each of your scripts,
#' using double-colon notation (\code{darn::init()}).  A script without such a
#' call is considered to have no prerequisites.
#'
#' @param ... [character(1)]\cr
#'   Prerequisites, without path or extension
#' @name init
#' @inheritParams init_
NULL

#' @export
#' @param .dots \code{[list]}\cr
#'   Additional dependencies as list
#' @param env_vars \code{[character]}\cr
#'   Environment variables that define the configuration of the script
#' @param envir \code{[environment]}\cr
#'   Environment to load the data into. Default: parent frame.
#' @param path \code{[character(1)]}\cr
#'   The path of the current script, useful if automatic detection fails.
#' @return A named list that contains path information about the current script.
#' @rdname init
"init_"

#' @export
#' @name init
#' @inheritParams init_
#' @importFrom lazyforward lazyforward
init <- lazyforward("init_")

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
#' @return A named list that contains path information about the current script.
#' @rdname done
done_ <- function(..., .dots = NULL, .compress = FALSE) {
  path_info <- get_path_info()

  dots <- get_done_dots(.dots, ...)
  vals <- lazyeval::lazy_eval(dots)

  dir.create(path_info$target_dir, showWarnings = FALSE, recursive = TRUE)
  getMakeLazyLoadDB()(vals, path_info$target_base, compress = .compress)
  invisible(path_info)
}

get_done_dots <- function(.dots, ...) {
  lazyeval::all_dots(.dots, ..., all_named = TRUE)
}

#' @export
#' @name done
#' @inheritParams done_
#' @importFrom lazyforward lazyforward
done <- lazyforward("done_")

#' Returns target directory
#'
#' Can be run by the scripts to determine the write location for additional
#' files.
#'
#' @export
get_target_dir <- function() {
  path_info <- get_path_info()
  path_info$target_dir
}

#' @inheritParams base::file.path
#' @export
#' @rdname get_target_dir
get_target_path <- function(...) {
  file.path(get_target_dir(), ...)
}
