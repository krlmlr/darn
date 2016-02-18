R_FILE_PATTERN <- "[.][rR]$"
R_FILE_TARGETS <- c("%.R", "%.r")

PACKAGE_NAME <- unname(read.dcf("DESCRIPTION")[1L, "Package"])

CONFIG_FILE_NAME <- "Darnfile"

#' @importFrom rprojroot has_file
root_criterion <- has_file(CONFIG_FILE_NAME)

root_file_path <- root_criterion$find_file

#' @importFrom stats setNames
read_config <- function(root) {
  config <- file_path(root, CONFIG_FILE_NAME)
  config_contents <- readLines(config)

  CONFIG_PATTERN <- "([^=?]+)[?]?=(.*)"

  config_assignments <- grep(CONFIG_PATTERN, config_contents, value = TRUE)

  config_names <- gsub(CONFIG_PATTERN, "\\1", config_assignments)
  config_values <- gsub(CONFIG_PATTERN, "\\2", config_assignments)
  config_values <- gsub('^"(.*)"$', "\\1", config_values)
  config_values <- gsub("^'(.*)'$", "\\1", config_values)

  setNames(as.list(config_values), config_names)
}
