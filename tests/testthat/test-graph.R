context("graph")

test_that("Can create simple graph", {
  g <- dep_graph("simple")
  expect_identical(graph::nodes(g), c("A.R", "B.R"))
  expect_identical(graph::edges(g), list(A.R = character(), B.R = "A.R"))
})
