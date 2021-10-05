test_that("Union works", {
  dg1 <- as.datagraph(data.table(from = "A", to = "B"), vertices = data.table(id = "A"))
  dg2 <- as.datagraph(data.table(from = "B", to = "C"), vertices = data.table(id = "C"))

  dg3 <- union_list_of_graphs(list(dg1, dg2))
  dg4 <- union.datagraph(dg1, dg2)

  expect_true(are_adjacent(dg1, "A", "B"))
})
