test_that("Creating graphs works", {
  dg1 <- datagraph()
  expect_s3_class(dg1, "datagraph")
})

test_that("Adding and removing vertices works", {
  dg1 <- datagraph()

  add_vertex(dg1, data.frame(id = "A"))
  expect_true(contains_vertex(dg1, "A"))
  expect_identical(dg1[["A"]]$id, "A")

  remove_vertex(dg1, "A")
  expect_false(contains_vertex(dg1, "A"))

  add_vertex(dg1, data.frame(id = "A"))
  add_vertex(dg1, data.frame(id = "B"))
  expect_length(dg1, 2L)
  expect_true(contains_vertex(dg1, "A"))
  expect_true(contains_vertex(dg1, "B"))

  remove_vertices(dg1, c("A", "B"))
  expect_false(contains_vertex(dg1, "A"))
  expect_false(contains_vertex(dg1, "B"))
  expect_length(dg1, 0L)
})

test_that("Adding and removing edges works", {
  dg1 <- datagraph()
  add_vertex(dg1, data.frame(id = "A"))
  add_vertex(dg1, data.frame(id = "B"))

  add_edges(dg1, data.frame(from = "A", to = "B"))
  expect_true(are_adjacent(dg1, "A", "B"))

  remove_edges(dg1, data.frame(from = "A", to = "B"))
  expect_false(are_adjacent(dg1, "A", "B"))
})
