#' Create Makefile
#'
#' This function creates a Makefile that builds all scripts below the root
#' directory.
#'
#' @param root_dir \code{character[1]}\cr The root directory
#' @param file_name \code{character[1]}\cr File path (relative to the root
#'   directory), default: \code{Makefile}
#' @param dep_file_name \code{character[1]}\cr File path (relative to the root
#'   directory), default: \code{Dependencies}
#' @export
create_makefile <- function(root_dir, file_name = "Makefile", dep_file_name = "Dependencies") {
  make_file <-
    MakefileR::makefile() +
    MakefileR::make_comment("This makes sure that the dependencies are created initially, and updated with each invocation") +
    MakefileR::make_rule(R_FILE_TARGETS, dep_file_name) +
    MakefileR::make_rule(dep_file_name, script =
      paste0("Rscript -e \"", PACKAGE_NAME, "::create_dep_file('.', '$@')\"")) +
    MakefileR::make_comment("This defines the dependencies between the R scripts") +
    MakefileR::make_text(paste0("include ", dep_file_name)) +
    MakefileR::make_comment("This defines the actual processing logic") +
    purrr::reduce(
      .init = MakefileR::make_group(),
      .f = `+`,
      lapply(
        R_FILE_TARGETS,
        MakefileR::make_rule,
        targets = "%.rdx",
        script = "Rscript -e \"rmarkdown::render('$<', 'html_document')\""
      )
    )

  MakefileR::write_makefile(make_file, file.path(root_dir, file_name))
}
