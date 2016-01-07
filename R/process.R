#' Init a stage
#'
#' Add a call to this functions as first statement to each of your scripts,
#' using double-colon notation (\code{darg::init()}).  A script without such a
#' call is considered to have no dependencies.
#'
#' @param ... Dependencies
#' @name init
NULL

#' @export
#' @param .dots \code{list}\cr
#'   Additional dependencies as list
#' @rdname init
init_ <- function(..., .dots = NULL, envir = parent.frame()) {
  path <- kimisc::thisfile()
  source_dir <- dirname(path)
  target_dir <- dirname(path)
  file_base <- gsub("[.][^.]*", "", basename(path))

  dots <- lazyeval::all_dots(.dots, ...)
  vals <- lazyeval::lazy_eval(dots)

  if (is.null(names(vals))) {
    names(vals) <- rep("", length(vals))
  }

  if (any(names(vals) != "")) {
    stop("Naming not yet implemented.")
  }

  deps <- unlist(vals)
  lapply(vals, init_one, source_dir = source_dir, target_dir = target_dir,
         envir = envir)
}

init_one <- function(file_base, source_dir, target_dir, envir) {
  r_file <- file.path(source_dir, sprintf("%s.R", file_base))

  rdx_base <- file.path(target_dir, file_base)
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
#' using double-colon notation (\code{darg::done()}).  A script without such a
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
  path <- kimisc::thisfile()
  target_dir <- dirname(path)
  file_base <- gsub("[.][^.]*", "", basename(path))

  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  vals <- lazyeval::lazy_eval(dots)

  tools:::makeLazyLoadDB(vals, file.path(target_dir, file_base),
                         compress = .compress)
}

#' @export
#' @inheritParams done_
done <- lazyforward::lazyforward("done_")
