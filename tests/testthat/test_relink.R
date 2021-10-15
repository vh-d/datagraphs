test_that("relinking edges", {
  dg1 <- datagraph()
  add_vertex(graph = dg1, list(id = "A"))
  add_vertex(graph = dg1, list(id = "B"))
  add_vertex(graph = dg1, list(id = "C"))
  add_edge("A", "B", graph = dg1)
  expect_true(are_adjacent(dg1, "A", "B"))

  relink_edge(e = dg1[["A"]][["to"]][["B"]], graph = dg1, to = dg1[["C"]])
  expect_false(are_adjacent(dg1, "A", "B"))
  expect_true(are_adjacent(dg1, "A", "C"))
  expect_true(check(dg1))
})
