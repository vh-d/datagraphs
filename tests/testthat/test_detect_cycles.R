test_that("detect cycles", {
  dg1 <- as.datagraph(data.table(from = c(1, 2, 3), to = c(2, 3, 1)))
  dg1b <- detect_cycles(dg1)
  expect_equal(as.data.table(dg1), as.data.table(dg1b))

  dg2 <- as.datagraph(data.table(from = c(1, 2, 3, 3), to = c(2, 3, 1, 4)))
  dg2b <- detect_cycles(dg1)
  expect_equal(as.data.table(dg2b), as.data.table(dg1b))

})
