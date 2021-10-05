test_that("Collapsing vertices", {
  dg1 <- datagraph()
  add_vertices(dg1, data.table(id = LETTERS[1:4]))
  add_edges(dg1, data.table(from = c("A", "A", "B"), to = c("B", "C", "D")))

  expect_length(V(dg1), 4)

  collapse_vertices(dg1, c("A", "B"))
  expect_length(V(dg1), 3)
  expect_true(are_adjacent(dg1, "A", "C"))
  expect_true(are_adjacent(dg1, "A", "D"))
})
