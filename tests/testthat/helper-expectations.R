expect_lt_time <- function(expected, actual) {
  expect_lt(as.numeric(expected), as.numeric(actual))
}

expect_gt_time <- function(expected, actual) {
  expect_gt(as.numeric(expected), as.numeric(actual))
}
