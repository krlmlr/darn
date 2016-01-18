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
  MakefileR::write_makefile(deps_file, file.path(root_dir, file_name))
}

create_deps_rules <- function(root_dir, relative_to = root_dir) {
  web <- parse_script(dir(root_dir, pattern = R_FILE_PATTERN, full.names = TRUE))
  deps <- get_deps(web, relative_to = relative_to)

  dep_rules <- mapply(
    function(target, dep) {
      if (!is.null(dep))
        MakefileR::make_rule(rdx_from_r(web, target), rdx_from_r(web, names(dep)))
      },
    names(deps), deps
  )

  process_rules <- lapply(
    names(deps),
    function(target) {
      MakefileR::make_rule(rdx_from_r(web, target), target, "${script}")
    }
  )

  init <-
    MakefileR::makefile() +
    MakefileR::make_rule("all", rdx_from_r(web, names(deps)))

  purrr::reduce(c(purrr::compact(dep_rules), process_rules),
                `+`, .init = init)
}

rdx_from_r <- function(web, paths) {
  lapply(paths, rdx_from_r_one, web = web)
}

rdx_from_r_one <- function(web, path) {
  path_info <- web[[path]]$path_info
  R.utils::getRelativePath(
    file.path(path_info$target_dir, gsub(R_FILE_PATTERN, ".rdx", path)),
    path_info$root)

  # TODO: Use root from web (not from path_info)
}

get_deps <- function(web, relative_to = ".") {
  names(web) <- R.utils::getRelativePath(names(web), relative_to)
  lapply(web, get_deps_one, relative_to = relative_to)
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

  # TODO: Notion of "root directory" also here (or in read_web())
}

parse_script_one <- function(path) {
  exprs <- parse(path)
  darn_calls <- vapply(
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

  init_call_idx <- which(darn_calls == "init")
  done_call_idx <- which(darn_calls == "done")

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

  path <- normalizePath(path)

  # TODO: Check root dir returned by path info

  list(
    path = path,
    path_info = get_path_info(path),
    init = list(
      deps = deps
    ),
    done = done
  )
}
