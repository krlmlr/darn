context("graph")

test_that("Can create simple graph", {
  g <- dep_graph("simple")
  expect_identical(graph::nodes(g), c("A.R", "B.R"))
  expect_identical(graph::edges(g), list(A.R = "B.R", B.R = character()))
})

test_that("Can create simple graph for subdir", {
  g <- dep_graph("subdir")
  expect_identical(graph::nodes(g), c("dir/A.R", "dir/B.R"))
  expect_identical(graph::edges(g), list(`dir/A.R` = "dir/B.R", `dir/B.R` = character()))
})

test_that("Can get order of graph", {
  expect_identical(get_order("simple"), c("A.R", "B.R"))
  expect_identical(get_order("out_dir"), c("A.R", "B.R"))
  expect_identical(get_order("subdir"), c("dir/A.R", "dir/B.R"))
  expect_error(get_order("loop"), "cycle")
})
