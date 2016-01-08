#' Create dependencies file
#'
#' This function collects dependency information for all scripts below the
#' root directory, and writes a \code{make}-compatible dependencies file.
#'
#' @param root_dir \code{character[1]}\cr The root directory
#' @param file_name \code{character[1]}\cr File path (relative to the root
#'   directory), default: \code{Dependencies}
#' @export
create_dep_file <- function(root_dir, file_name = "Dependencies") {
  deps_file <- create_deps_rules(root_dir)
  writeLines(format(deps_file), file.path(root_dir, file_name))
}

create_deps_rules <- function(root_dir, relative_to = root_dir) {
  deps <- get_deps(dir(root_dir, full.names = TRUE), relative_to = relative_to)
  rules <- mapply(
    function(target, dep) {
      if (!is.null(dep))
        MakefileR::make_rule(target, names(dep))
      },
    names(deps), deps
  )

  purrr::reduce(purrr::compact(rules), `+`, .init = MakefileR::makefile())
}

get_deps <- function(path, relative_to) {
  parsed <- parse_script(path)
  names(parsed) <- R.utils::getRelativePath(names(parsed), relative_to)
  lapply(parsed, get_deps_one, relative_to = relative_to)
}

get_deps_one <- function(parsed_one, relative_to) {
  path <- dirname(parsed_one[["path"]])
  deps <- parsed_one[["init"]][["deps"]]
  if (is.null(deps)) {
    return(NULL)
  }
  names(deps) <- R.utils::getRelativePath(file.path(path, names(deps)), relative_to)
  deps
}

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
  deps <- unlist(deps, recursive = FALSE)

  done <- if (length(done_call_idx) > 0L) {
    done_call <- exprs[[done_call_idx]]
    done_call[[1]] <- quote(lazyeval::lazy_dots)
    done_lazy <- get_done_dots(eval(done_call))
    list(names = names(done_lazy))
  }

  list(
    path = normalizePath(path),
    init = list(
      deps = deps
    ),
    done = done
  )
}
