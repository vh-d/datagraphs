
test_that("neighbors", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_edges(dg1,  data.table(from = "A", to = "B"))

  expect_equal(neighbors(dg1, "A", "in", names = TRUE), character())
  expect_equal(neighbors(dg1, "A", "all", names = TRUE), "B")
  expect_equal(neighbors(dg1, "A", "all", names = TRUE), "B")
})


test_that("neighborhood", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_vertex(dg1, data.table(id = "C"))
  add_edges(dg1, data.table(from = "A", to = "B"))
  add_edges(dg1, data.table(from = "B", to = "C"))

  expect_setequal(neighborhood(dg1, vertices = "A", order = 0L,   mode = "in", names = TRUE), c("A"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 100L, mode = "in", names = TRUE), c("A"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 1L,   mode = "all", names = TRUE), c("A", "B"))
  expect_setequal(neighborhood(dg1, vertices = "A", order = 100L, mode = "all", names = TRUE), c("A", "B", "C"))

  expect_setequal(neighborhood(dg1, vertices = "B", order = 0L,   mode = "in", names = TRUE),  c("B"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "in", names = TRUE),  c("A", "B"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "out", names = TRUE), c("B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "B", order = 100L, mode = "all", names = TRUE), c("A", "B", "C"))

  expect_setequal(neighborhood(dg1, vertices = "C", order = 0L,   mode = "in", names = TRUE),  c("C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 1L,   mode = "in", names = TRUE),  c("B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "in", names = TRUE),  c("A", "B", "C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "out", names = TRUE), c("C"))
  expect_setequal(neighborhood(dg1, vertices = "C", order = 100L, mode = "all", names = TRUE), c("A", "B", "C"))

})
