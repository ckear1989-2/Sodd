
test_that("test strategy page", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  expect_silent(model <- build.sodd.model(format(Sys.Date()-7, "%Y-%m-%d"), "spread", weights=FALSE, plot.it=FALSE, keep.data=TRUE))
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
  }
})
