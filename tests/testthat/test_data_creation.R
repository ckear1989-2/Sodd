test_that("variables in data", {
  set.sodd.options(data.dir="~/sodd.data/", verbosity=0)
  create.sodd.modeling.data(c("E0", "E1"), 2)
  dt <- readRDS(file.path(get.sodd.data.dir(), "a.dt.rds"))
  expect_true("ip" %in% colnames(dt))
  expect_true("fthg" %in% colnames(dt))
  expect_true("ftag" %in% colnames(dt))
})

test_that("no previous dates in upcoming", {
  set.sodd.options(data.dir="~/sodd.data/", force.upcoming=FALSE, verbosity=0)
  date <- format((Sys.Date()-7), "%Y-%m-%d")
  model.dt.list <- read.model.data(date, "spread", FALSE, FALSE)
  upcoming.dt <- model.dt.list[[4]]
  if (upcoming.dt != FALSE) {
      expect_true(upcoming.dt[date < Sys.Date(), .N] == 0)
  }
})

