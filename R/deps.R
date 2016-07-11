#' Create dependencies file
#'
#' This function collects dependency information for all scripts below the
#' root directory, and writes a \code{make}-compatible dependencies file.
#'
#' @param root_dir \code{character[1]}\cr The root directory
#' @param file_name \code{character[1]}\cr File path (relative to the root
#'   directory), default: \code{Dependencies}
#' @param src_dir \code{character[1]}\cr The source directory, relative to the
#'   root
#' @export
create_dep_file <- function(root_dir, file_name = "Dependencies",
                            src_dir = ".") {
  deps_file <- create_deps_rules(root_dir, file_path(root_dir, src_dir))
  MakefileR::write_makefile(deps_file, file_path(root_dir, file_name))
}

create_deps_rules <- function(root_dir, src_dir = root_dir) {
  web <- get_web_from_src(root_dir, src_dir)
  deps <- get_deps(web)

  init <-
    MakefileR::makefile() +
    MakefileR::make_group(
      .dots = lapply(simple_from_r(web, names(deps)), MakefileR::make_rule, targets = "all"))

  simple_rules <- list(MakefileR::make_group(
    MakefileR::make_comment("Simple rules for building individual targets"),
    .dots = lapply(
      names(deps),
      function(target) {
        MakefileR::make_rule(simple_from_r(web, target), rdx_from_r(web, target))
      }
    )
  ))

  dep_rules_simple <- list(MakefileR::make_group(
    MakefileR::make_comment("Dependencies on simple targets are there for the user's convenience only"),
    .dots = purrr::compact(mapply(
      function(target, dep) {
        if (!is.null(dep))
          MakefileR::make_rule(simple_from_r(web, target), simple_from_r(web, names(dep)))
      },
      names(deps), deps
    ))
  ))

  dep_rules_rdx <- list(MakefileR::make_group(
    MakefileR::make_comment("Dependencies are formulated on .rdx files for safety"),
    .dots = purrr::compact(mapply(
      function(target, dep) {
        if (!is.null(dep))
          MakefileR::make_rule(rdx_from_r(web, target), rdx_from_r(web, names(dep)))
        },
      names(deps), deps
    ))
  ))

  process_rules_comment <-
    list(MakefileR::make_comment("The actual worker rules"))

  # Not using a pattern rule here by design (#4)
  process_rules <- lapply(
    names(deps),
    function(target) {
      MakefileR::make_rule(rdx_from_r(web, target), target, "${script}")
    }
  )

  purrr::reduce(c(simple_rules, dep_rules_simple, dep_rules_rdx, process_rules_comment, process_rules),
                `+`, .init = init)
}

simple_from_r <- function(web, paths) {
  lapply(paths, simple_from_r_one, web = web)
}

simple_from_r_one <- function(web, path) {
  path_info <- web[[path]]$path_info
  basename(path_info$target_base)
}

rdx_from_r <- function(web, paths) {
  lapply(paths, rdx_from_r_one, web = web)
}

rdx_from_r_one <- function(web, path) {
  path_info <- web[[path]]$path_info
  relative_to(paste0(path_info$target_base, ".rdx"), path_info$root)

  # TODO: Use root from web (not from path_info)
}

get_deps <- function(web) {
  lapply(web, get_deps_one, relative_to = relative_to)
}

get_deps_one <- function(parsed_one, relative_to) {
  path <- dirname(parsed_one[["path"]])
  parsed_one[["init"]][["deps"]]
}

get_web_from_src <- function(root_dir, src_dir) {
  withr::with_locale(
    c(LC_COLLATE = "C"),
    files <- dir(src_dir, pattern = R_FILE_PATTERN, full.names = TRUE)
  )
  parse_script(files, root_dir = root_dir)
}

parse_script <- function(path, root_dir) {
  names(path) <- relative_to(path, root_dir)
  parsed <- lapply(path, parse_script_one, root_dir = root_dir)
  parsed <- parsed[!vapply(parsed, is.null, logical(1L))]
  parsed
}

remove_assignments <- function(x) {
  if (is.call(x) && identical(as.character(x[[1L]]), "<-"))
    remove_assignments(x[[3L]])
  else
    x
}

find_darn_call <- function(x) {
  if (is.call(x)) {
    y <- x[[1L]]
    if (is.call(y) && identical(as.character(y[[1L]]), "::") &&
        identical(as.character(y[[2L]]), PACKAGE_NAME)) {
      return(as.character(y[[3L]]))
    }
  }

  NA_character_
}

parse_script_one <- function(path, root_dir) {
  root_dir <- normalizePath(root_dir)
  path <- normalizePath(path)
  path_info <- get_path_info(path, expand_makefile_env_vars)

  exprs <- parse(path)
  exprs <- lapply(exprs, remove_assignments)
  darn_calls <- vapply(exprs, find_darn_call, character(1L))

  init_call_idx <- which(darn_calls == "init")
  done_call_idx <- which(darn_calls == "done")

  if (length(done_call_idx) == 0L) {
    return(NULL)
  } else if (length(done_call_idx) > 1L) {
    warning("More than one call to done() found in ", path,
            ", using the last.", call. = FALSE)
    done_call_idx <- done_call_idx[length(done_call_idx)]
  }

  deps <- lapply(
    exprs[init_call_idx],
    function(init_call) {
      init_call[[1]] <- quote(lazyeval::lazy_dots)
      init_args <- eval(init_call)
      init_args <- init_args[names(init_args) %nin% names(formals(init))]
      get_init_deps_list(init_args)
    }
  )
  deps <- unlist(deps, recursive = FALSE)
  names(deps) <- relative_to(file.path(path_info$src_dir, names(deps)),
                             root_dir)

  done <- if (length(done_call_idx) > 0L) {
    done_call <- exprs[[done_call_idx]]
    done_call[[1]] <- quote(lazyeval::lazy_dots)
    done_lazy <- get_done_dots(eval(done_call))
    list(names = names(done_lazy))
  }

  if (normalizePath(path_info$root) != root_dir) {
    stop("Project root for file ", path, " different from given base directory ",
         root_dir, call. = FALSE)
  }

  list(
    path = path,
    path_info = path_info,
    init = list(
      deps = deps
    ),
    done = done
  )
}

#' @importFrom stats setNames
expand_makefile_env_vars <- function(env_vars, ...) {
  if (length(env_vars) == 0L) {
    return()
  }
  setNames(paste0("${", env_vars, "}"), env_vars)
}
