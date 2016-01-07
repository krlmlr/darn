CONFIG_FILE_NAME <- ".darg"

#' @importFrom utils glob2rx
is_darg_root <- rprojroot::has_file(glob2rx(CONFIG_FILE_NAME))

find_root_file <- rprojroot::make_find_root_file(is_darg_root)
