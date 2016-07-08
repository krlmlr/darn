#' @importFrom tibble lst
get_path_info <- function(path = NULL, .get_env_vals = NULL) {
  path <- (
    path
    %||% .darn_env$current_file
    %||% kimisc::thisfile()
  )

  if (is.null(path)) {
    stop("Could not determine source file path.", call. = FALSE)
  }

  path <- gsub("(.*)[.]spin[.]Rmd$", "\\1.R", path) ## HACK HACK HACK

  if (!file.exists(path)) {
    stop("Location of current file computed as ", path, " but not found.",
         call. = FALSE)
  }

  src_dir <- dirname(path)

  root <- root_file_path(path = src_dir)
  config <- read_config(root)
  out_dir <- config[["out_dir"]] %||% "."

  if (is.null(.get_env_vals)) {
    .get_env_vals = get_env_vals
  }

  env_vars <- get_env_vars(config)
  env_vals <- .get_env_vals(env_vars, config)
  env_dir <- get_env_dir(env_vals)

  relative_src_dir <- relative_to(src_dir, root)

  target_dir <- file_path(root, out_dir, env_dir, relative_src_dir)

  target_base <- file_path(target_dir, strip_extension(basename(path)))

  lst(path, src_dir, root, target_dir, target_base, env_vals)
}

get_env_vars <- function(config) {
  env_vars <- strsplit(config[["env_vars"]] %||% "", "\\s+")[[1L]]
  env_vars <- env_vars[env_vars != ""]

  env_vars
}

get_env_vals <- function(env_vars, config) {
  if (length(env_vars) == 0L) {
    return()
  }

  env_vals <- Sys.getenv(env_vars, names = TRUE)
  empty_env_vals <- names(env_vals)[env_vals == ""]

    missing_env_vals <- setdiff(empty_env_vals, names(config))
  if (length(missing_env_vals) > 0L) {
    stop("Undefined or empty environment variables, define default values in ",
         CONFIG_FILE_NAME, ": ",
         paste(missing_env_vals, collapse = ", "), call. = FALSE)
  }

  env_vals[empty_env_vals] <- unlist(config[empty_env_vals])

  lapply(env_vals, type.convert, as.is = TRUE, numerals = "allow.loss")
}

get_env_dir <- function(env_vals) {
  env_dir_parts <- mapply(function(name, value) paste0(name, "-", value),
                          names(env_vals), env_vals)
  env_dir <- file_path(.dots = as.list(env_dir_parts))
}
