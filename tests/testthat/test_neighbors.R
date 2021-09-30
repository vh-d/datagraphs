
test_that("neighbors", {
  dg1 <- datagraph()
  add_vertex(dg1, data.frame(id = "A"))
  add_vertex(dg1, data.frame(id = "B"))
  add_edges(dg1, data.frame(from = "A", to = "B"))

  expect_equal(neighbors(dg1, "A", "in"), character())
  expect_equal(neighbors(dg1, "A", "all"), "B")
  expect_equal(neighbors(dg1, "A", "all"), "B")
})
