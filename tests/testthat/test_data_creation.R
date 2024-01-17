# note run tests/create.test.data.R to create the data needed for tests

test_that("variables in data", {
  set.sodd.options(data.dir="~/sodd.data/test.data/", verbosity=0)
  dt <- readRDS(file.path(get.sodd.data.dir(), "a.dt.rds"))
  expect_true("ip" %in% colnames(dt))
  expect_true("fthg" %in% colnames(dt))
  expect_true("ftag" %in% colnames(dt))
  expect_true("mweek" %in% colnames(dt))
  expect_equal(min(dt[, mweek]), 1)
  expect_equal(max(dt[, mweek]), length(unique(dt[, mweek])))
})

test_that("no previous dates in upcoming spread", {
  set.sodd.options(data.dir="~/sodd.data/test.data/", force.upcoming=TRUE, verbosity=0)
  date <- "2023-09-01"
  model.dt.list <- read.model.data(date, "spread", FALSE, FALSE)
  upcoming.dt <- model.dt.list[[4]]
  expect_true("data.table" %in% class(upcoming.dt))
  expect_true(upcoming.dt[date < "2023-09-01", .N] == 0)
})

test_that("no previous dates in upcoming act", {
  set.sodd.options(data.dir="~/sodd.data/test.data/", force.upcoming=TRUE, verbosity=0)
  date <- "2023-09-01"
  model.dt.list <- read.model.data(date, "act", FALSE, FALSE)
  upcoming.dt <- model.dt.list[[4]]
  expect_true("data.table" %in% class(upcoming.dt))
  expect_true(upcoming.dt[date < "2023-09-01", .N] == 0)
})

