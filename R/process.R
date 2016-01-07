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
done_ <- function(..., .dots = NULL) {
}

#' @export
#' @inheritParams done_
done <- lazyforward::lazyforward("done_")
