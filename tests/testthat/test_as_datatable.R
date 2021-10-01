test_that("datagraphs can be converted to data.table", {
  dt1_v <- data.table(id = LETTERS[1:5], value = 1:5)
  dt1_e <- data.table(from = "A", to = "B")
  dg1 <- as.datagraph(dt1_e, vertices = dt1_v)

  dt2_v <- as.data.table(dg1)
  expect_setequal(dt2_v[, id], LETTERS[1:5])
  expect_setequal(dt2_v[, value], 1:5)
  expect_setequal(names(dt2_v), c("id", "from", "to", "value"))
  expect_true(dt2_v[id == "A", to == "B"])
  expect_true(dt2_v[id == "B", from == "A"])

  dt2_e <- as.data.table(dg1, what = "edges")
  expect_setequal(names(dt2_e), c("from", "to"))
  expect_setequal(dt2_e[, from], "A")
  expect_setequal(dt2_e[, to],   "B")
})
