test_that("conversions between datagraph and data.table", {
  nodes <- data.table(id = 1L:5L, label = letters[1L:5L])
  edges <- data.table(from = c(1L, 2L, 3L, 4L), to = c(2L, 3L, 4L, 5L))
  dg1 <- as.datagraph(edges, vertices = nodes)
  expect_true(check(dg1))

  expect_s3_class(dg1, "datagraph")
  # TODO: contains 5 nodes, with the id 1:5
})


test_that("conversions between datagraph and igraph", {
  nodes <- data.table(id = 1:5, label = letters[1:5])
  edges <- data.table(from = c(1, 2, 3, 4), to = c(2, 3, 4, 5))
  ig1 <- igraph::graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
  dg1 <- as.datagraph(ig1)
  expect_true(check(dg1))

  expect_s3_class(dg1, "datagraph")
  # TODO: contains 5 nodes, with the id 1:5

  ig1b <- igraph::as.igraph(dg1)
  expect_s3_class(ig1b, "igraph")
  expect_true(igraph::identical_graphs(ig1b, ig1))
})



