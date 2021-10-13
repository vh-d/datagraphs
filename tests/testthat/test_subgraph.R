test_that("Subgraphs can be created", {
  dg1 <- as.datagraph(
    data.table(from = LETTERS[3:6], to = LETTERS[4:7]),
    vertices = data.table(id = LETTERS[1:10])
  )

  dg1_subset <- subset(dg1, c("B", "C", "D"))
  expect_setequal(V(dg1_subset), c("B", "C", "D"))
  expect_true(are_adjacent(dg1_subset, "C", "D"))
  expect_true(check(dg1_subset))
})
