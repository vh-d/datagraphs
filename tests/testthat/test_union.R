test_that("Union works", {
  dg1 <- as.datagraph(data.table(from = "A", to = "B"), vertices = data.table(id = c("A", "B")))
  dg2 <- as.datagraph(data.table(from = character(), to = character()), vertices = data.table(id = "C"))
  expect_true(check(dg1))
  expect_true(check(dg2))

  dg3 <- union_list_of_graphs(list(dg1, dg2))
  expect_true(check(dg3))
  dg4 <- union.datagraph(dg1, dg2)
  expect_true(check(dg4))

  expect_true(are_adjacent(dg3, "A", "B"))
  expect_true(are_adjacent(dg4, "A", "B"))
})
