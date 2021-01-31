
test_that("variables in data", {
  create.sodd.modeling.data(c("E0", "E1"), 2)
  dt <- readRDS(file.path(get.sodd.data.dir(), "a.dt.rds"))
  expect_true("ip" %in% colnames(dt))
  expect_true("fthg" %in% colnames(dt))
  expect_true("ftag" %in% colnames(dt))
})

