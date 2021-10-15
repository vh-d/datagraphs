test_that("Creating graphs works", {
  dg1 <- datagraph()
  expect_s3_class(dg1, "datagraph")
})

test_that("Adding and removing vertices works", {
  dg1 <- datagraph()

  add_vertex(dg1, data.table(id = "A"))
  expect_true(contains_vertex(dg1, "A"))
  expect_identical(dg1[["A"]][["id"]], "A")

  remove_vertex(dg1, "A")
  expect_false(contains_vertex(dg1, "A"))

  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  expect_true(check(dg1))
  expect_length(V(dg1), 2L)
  expect_true(contains_vertex(dg1, "A"))
  expect_true(contains_vertex(dg1, "B"))

  remove_vertices(dg1, c("A", "B"))
  expect_true(check(dg1))
  expect_false(contains_vertex(dg1, "A"))
  expect_false(contains_vertex(dg1, "B"))
  expect_length(V(dg1), 0L)
})

test_that("Adding and removing edges works", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_vertex(dg1, data.table(id = "C"))

  add_edge(list(from = "A", to = "B"), graph = dg1)
  expect_true(check(dg1))
  expect_true(are_adjacent(dg1, "A", "B"))
  expect_length(E(dg1), 1L)

  add_edge("B", to = "C", graph = dg1)
  expect_true(check(dg1))
  expect_true(are_adjacent(dg1, "B", "C"))
  expect_length(E(dg1), 2L)

  remove_edge(dg1, from = "A", to = "B")
  expect_true(check(dg1))
  expect_false(are_adjacent(dg1, "A", "B"))
  expect_length(E(dg1), 1L)

  remove_edge(dg1, from = "B", to = "C")
  expect_true(check(dg1))
  expect_false(are_adjacent(dg1, "A", "B"))
  expect_length(E(dg1), 0L)
})


test_that("Removing vertices with existing edges works", {
  dg1 <- datagraph()
  add_vertex(dg1, data.table(id = "A"))
  add_vertex(dg1, data.table(id = "B"))
  add_vertex(dg1, data.table(id = "C"))

  add_edge(list(from = "A", to = "B"), graph = dg1)
  expect_true(check(dg1))
  expect_true(are_adjacent(dg1, "A", "B"))
  expect_length(E(dg1), 1L)

  add_edge("B", to = "C", graph = dg1)
  expect_true(check(dg1))
  expect_true(are_adjacent(dg1, "B", "C"))
  expect_length(E(dg1), 2L)

  remove_vertex(dg1, "A")
  expect_equal(neighbors_in(dg1[["B"]], names = TRUE), character())
  expect_true(check(dg1))
})
