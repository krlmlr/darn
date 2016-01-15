R_FILE_PATTERN <- "[.][rR]$"
R_FILE_TARGETS <- c("%.R", "%.r")

PACKAGE_NAME <- unname(read.dcf("DESCRIPTION")[1L, "Package"])

CONFIG_FILE_NAME <- "Darnfile"

get_config_file <- function() {
  find_root_file(CONFIG_FILE_NAME)
}
