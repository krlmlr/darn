if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  message("helper-testthat: Setting options specific to testthat")
  options(mc.cores = parallel::detectCores())
  options(testthat.summary.omit_dots = TRUE)
}
