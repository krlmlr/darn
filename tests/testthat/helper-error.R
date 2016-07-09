patch_oops <- function(filename) {
  text <- readLines(filename)
  oops <- grepl("oops", text)
  expect_true(any(oops))
  text[oops] <- ""
  writeLines(text, filename)
}
