parse_script <- function(path) {
  names(path) <- path
  lapply(path, parse_script_one)
}

parse_script_one <- function(path) {
  exprs <- parse(path)
  darg_calls <- vapply(
    exprs,
    function(x) {
      if (is.call(x)) {
        y <- x[[1L]]
        if (is.call(y) && identical(as.character(y[[1L]]), "::") &&
            identical(as.character(y[[2L]]), PACKAGE_NAME)) {
          return(as.character(y[[3L]]))
        }
      }

      NA_character_
    },
    character(1L)
  )

  init_call_idx <- which(darg_calls == "init")
  done_call_idx <- which(darg_calls == "done")

  if (length(done_call_idx) == 0L) {
    warning("No call to done() found, ", path,
            " cannot be used as parent for other scripts.")
  } else if (length(done_call_idx) > 1L) {
    warning("More than one call to done() found in ", path,
            ", using the last.")
    done_call_idx <- done_call_idx[length(done_call_idx)]
  }

  deps <- lapply(
    exprs[init_call_idx],
    function(init_call) {
      init_call[[1]] <- quote(lazyeval::lazy_dots)
      get_init_deps_list(eval(init_call))
    }
  )

  done <- if (length(done_call_idx) > 0L) {
    done_call <- exprs[[done_call_idx]]
    done_call[[1]] <- quote(lazyeval::lazy_dots)
    done_lazy <- get_done_dots(eval(done_call))
    list(names = names(done_lazy))
  }

  list(
    path = normalizePath(path),
    init = list(
      deps = unlist(deps, recursive = FALSE)
    ),
    done = done
  )
}
