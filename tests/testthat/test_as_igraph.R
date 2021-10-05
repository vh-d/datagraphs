test_that("Conversion to igraph", {
  dg1 <- datagraph()
  add_vertex(dg1, list(id = "1"))
  add_edge(dg1, data.table(from = "1", to = "2"))
  expect_true(are_adjacent(dg1, "1", "2"))
  ig1 <- as.igraph(dg1, add_missing = TRUE)
  expect_length(V(ig1), 2)
  expect_length(E(ig1), 1)

  ig2 <- as.igraph(dg1, add_missing = FALSE)
  expect_length(V(ig2), 1)
  expect_length(E(ig2), 0)
})
