
test_that("neighbors", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_edges(dg1,  data.table(from = "A", to = "B"))

  expect_equal(neighbors(dg1, "A", "in"), character())
  expect_equal(neighbors(dg1, "A", "all"), "B")
  expect_equal(neighbors(dg1, "A", "all"), "B")
})


test_that("neighborhood", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_vertex(dg1, data.table(id = "C"))
  add_edges(dg1, data.table(from = "A", to = "B"))
  add_edges(dg1, data.table(from = "B", to = "C"))

  expect_setequal(neighborhood(dg1, vertices = "A", order = 0L,   mode = "in"), c("A"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 100L, mode = "in"), c("A"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 1L,   mode = "all"), c("A", "B"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 100L, mode = "all"), c("A", "B", "C"))

  expect_setequal(neighborhood(dg1, vertices = "B", order = 0L,   mode = "in"),  c("B"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "in"),  c("A", "B"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "out"), c("B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "all"), c("A", "B", "C"))

  expect_setequal(neighborhood(dg1, vertices = "C", order = 0L,   mode = "in"),  c("C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 1L,   mode = "in"),  c("B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "in"),  c("A", "B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "out"), c("C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "all"), c("A", "B", "C"))

})
