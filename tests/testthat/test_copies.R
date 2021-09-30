test_that("datagraphs can be copied", {
  dg1 <- datagraph()
  dg2 <- copy_graph(dg1)
  # copy of empty graphs
  expect_true(all.equal(dg1, dg2))

  add_vertices(dg1, data.table(id = c("A", "B")))
  expect_type(all.equal(dg1, dg2), type = "character")

  add_edges(dg1, data.table(from = "A", to = "B"))
  dg3 <- copy_graph(dg1)

  # copy of graphs with vertices and edges
  expect_true(all.equal(dg1, dg3))

  # copies are independent
  dg3[["A"]]$data <- 1
  expect_type(all.equal(dg1, dg3), type = "character")

})
