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

  env_dir_parts <- mapply(function(name, value) paste0(name, "-", value),
                          names(env_vals), env_vals)
  env_dir <- do.call(file.path, as.list(env_dir_parts))
}
