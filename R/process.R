#' Write the results of a stage
#'
#' Add a call to this functions as last statement to each of your scripts, using
#' double-colon notation (\code{darg::done()}).  A script without such a call
#' is considered to have no output and cannot be a dependency of another script.
#'
#' @param ... Objects to save
#' @name done
NULL

#' @export
#' @param .dots \code{list}\cr
#'   Additional objects as list
#' @param .compress \code{logical(1)}\cr
#'   Compress output (default: \code{FALSE})
done_ <- function(..., .dots = NULL, .compress = FALSE) {
  path <- kimisc::thisfile()
  target_dir <- dirname(path)
  file_base <- gsub("[.][^.]*", "", basename(path))

  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  vals <- lazyeval::lazy_eval(dots)

  tools:::makeLazyLoadDB(dots, file.path(target_dir, file_base),
                         compress = .compress)
}

#' @export
#' @inheritParams done_
done <- lazyforward::lazyforward("done_")
