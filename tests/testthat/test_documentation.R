
test_that("test strategy page", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- format(Sys.Date()-7, "%Y-%m-%d")
  output.dir <- get.sodd.output.dir()
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_spread", "_strategy", ".png"))
  expect_silent(model <- build.sodd.model(adate, "spread", weights=FALSE, plot.it=FALSE, keep.data=TRUE))
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    expect_silent(plot.model.run(model))
    expect(file.exists(mpngf1), "strategy png not created")
  }
})
