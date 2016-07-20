#' Create Makefile
#'
#' This function creates a Makefile that builds all scripts below the root
#' directory.
#'
#' @param root_dir \code{[character(1)]}\cr
#'   The root directory
#' @param file_name \code{[character(1)]}\cr
#'   File path (relative to the root directory), default: \code{"Makefile"}
#' @param dep_file_name \code{[character(1)]}\cr
#'   File path (relative to the root directory), default: \code{"Dependencies"}
#' @param src_dir,out_dir \code{[character(1)]}\cr
#'   Source and output directories (relative to the root directory), default:
#'   \code{"."}
#' @param env_vars \code{[named list]}\cr
#'   Default values for environment variables that define configurations of the
#'   script, default: none
#' @param script \code{[character]}\cr A script that processes the input files,
#'   suitable for inclusion in a \code{Makefile}.  Use \code{$<} as placeholder
#'   for the current input file.
#'   Default: a call to \code{run} (recommmended) that invokes
#'   \code{ezknitr::\link[ezknitr]{ezspin}}.
#' @export
create_makefile <- function(
  root_dir, file_name = "Makefile", dep_file_name = "Dependencies",
  src_dir = ".", out_dir = ".", env_vars = NULL,
  script = NULL) {

  my_formals <- formals()
  my_formals$script <- paste0("R -q -e \"", run_call("$<"), "\"")

  if (is.null(script))
    script <- my_formals$script

  out_dir <- relative_to(file_path(root_dir, out_dir), root_dir)

  config_path <- file_path(root_dir, CONFIG_FILE_NAME)

  if (!file.exists(config_path)) {
    config_file <-
      MakefileR::makefile() +
      MakefileR::make_comment("This file contains the configuration of the R script network.") +
      create_config_group(dep_file_name, src_dir, out_dir, env_vars, script,
                          define_config_vars = TRUE)
    MakefileR::write_makefile(config_file, config_path)
  } else {
    warning("Not overwriting config file ", config_path, call. = FALSE)
  }

  make_file <-
    MakefileR::makefile() +

    MakefileR::make_group(
      MakefileR::make_comment(paste0("Primary target (further defined in ",
                                     dep_file_name , ")")),
      MakefileR::make_rule("all")
    ) +

    MakefileR::make_group(
      MakefileR::make_comment("Configuration, and default values"),
      MakefileR::make_text(paste0("-include ", CONFIG_FILE_NAME)),
      create_config_group(dep_file_name = my_formals$dep_file_name,
                          src_dir = my_formals$src_dir,
                          out_dir = my_formals$out_dir,
                          env_vars = my_formals$env_vars,
                          script =  my_formals$script,
                          define_config_vars = FALSE)
    ) +

    MakefileR::make_comment("This makes sure that the dependencies are created initially, and updated with each invocation") +
    MakefileR::make_rule(
      targets = "${dep_file_name}",
      deps = c("${src_dir}", "$(wildcard ${src_dir}/*.R)", "$(wildcard ${src_dir}/*.r)"),
      script = paste0(
        "R -q -e \"", PACKAGE_NAME, "::",
        as.character(quote(create_dep_file)),
        "('.', '$@', '${src_dir}')\"")) +

    MakefileR::make_comment("This defines the dependencies between the R scripts") +
    MakefileR::make_text("include ${dep_file_name}")

  MakefileR::write_makefile(make_file, file_path(root_dir, file_name))
}

create_config_group <- function(dep_file_name, src_dir, out_dir, env_vars,
                                script, ..., define_config_vars) {
  operator <- "?="

  ret <- MakefileR::make_group(
    MakefileR::make_def("dep_file_name", dep_file_name, operator),
    MakefileR::make_def("src_dir", src_dir, operator),
    MakefileR::make_def("out_dir", out_dir, operator),
    MakefileR::make_def("env_vars", paste(names(env_vars), collapse = " "), operator),
    MakefileR::make_def("script", script, operator)
  )

  if (define_config_vars) {
    ret <- ret + MakefileR::make_group(
      MakefileR::make_comment("Default values for configuration variables"),
      .dots = mapply(MakefileR::make_def, names(env_vars), env_vars,
                     MoreArgs = list(operator = "?="), SIMPLIFY = FALSE)
    )
  }

  ret
}

run_call <- function(filename) {
  paste0("darn::run(file = '", filename, "', ezknitr::ezspin, wd = '.', out_dir = '$(dir $@)', verbose = TRUE, keep_rmd = TRUE, chunk_opts = list(tidy = FALSE, error = FALSE))")
}
