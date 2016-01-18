#' @importFrom utils glob2rx
#' @include config.R
is_darn_root <- rprojroot::has_file(glob2rx(CONFIG_FILE_NAME))

find_root_file <- rprojroot::make_find_root_file(is_darn_root)
