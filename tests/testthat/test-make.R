context("make")

test_that("can make simple project", {
  f <- setup_scenario("simple")

  create_makefile(f())
  expect_equal(run_make("-C", f()), 0L)
  expect_true(file.exists(f("B.rdx")))
})
