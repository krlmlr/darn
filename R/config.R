R_FILE_PATTERN <- "[.][rR]$"

PACKAGE_NAME <- unname(read.dcf("DESCRIPTION")[1L, "Package"])

CONFIG_FILE_NAME <- paste0(".", PACKAGE_NAME)

get_config_file <- function() {
  find_root_file(CONFIG_FILE_NAME)
}
