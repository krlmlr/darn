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
    MakefileR::make_comment("This defines the dependencies between the R scripts") +
    MakefileR::make_text(paste0("-include ", file.path(root_dir, dep_file_name))) +
    MakefileR::make_rule("%.rdx", "%.R", "Rscript -e \"rmarkdown::render('$<', 'html_document')\"")

  MakefileR::write_makefile(make_file, file.path(root_dir, file_name))

  create_dep_file(root_dir, file_name = dep_file_name)
}
