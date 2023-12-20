
source("../create.test.data.R")

# run from this folder for testing with print messages
# library("sodd")
# library("grid")
# source("../../R/options.R")
# source("../../R/utils.R")
# source("../../R/plot.R")
# set.sodd.options(
#   data.dir="~/sodd.data/test.data/",
#   force.upcoming=TRUE,
#   verbosity=0
# )
# adate <- "2023-09-01"
# output.dir <- get.sodd.output.dir()
# mpngf1 <- file.path(output.dir, paste0("model_", adate, "_spread", "_test_by_date", ".png"))
# eval(read.test.model.spread)
# test.dt <- model$test.dt
# test.table <- detailed.test.date.data.table(test.dt)
# p.obj <- detailed.test.date.gtable(test.dt)
# print(test.table)
# print(p.obj)
# png(mpngf1)
# print(p.obj)
# grid.draw(p.obj)
# dev.off()

test_that("test by date page", {
  set.sodd.options(
    data.dir="~/sodd.data/test.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  # print(model)
  test.dt <- model$test.dt
  a.thin.dt <- test.dt[strat_top_per_match > 0, list(
    strat_top_per_match=round(sum(strat_top_per_match, 2)),
    gain_top_per_match=round(sum(gain_top_per_match), 3),
    expected_return=round(sum(er), 2)
    ), date][order(date)]
  if(a.thin.dt[, .N] > 16) a.thin.dt <- a.thin.dt[1:16, ]
  a.thin.dt[, date := as.character(date)]
  a.thin.dt <- rbind(
    a.thin.dt,
    data.table(
      date="total",
      strat_top_per_match=sum(a.thin.dt[, strat_top_per_match]),
      gain_top_per_match=sum(a.thin.dt[, gain_top_per_match]),
      expected_return=sum(a.thin.dt[, expected_return])
   ), fill=TRUE)
  setnames(a.thin.dt, colnames(a.thin.dt), gsub("_", "\n", colnames(a.thin.dt)))
  expect_silent(test.table <- detailed.test.date.data.table(test.dt))
  expect_silent(test.p.obj <- detailed.test.date.gtable(test.dt))
})

test_that("test strategy page", {
  set.sodd.options(
    data.dir="~/sodd.data/test.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_spread", "_strategy", ".png"))
  eval(read.test.model.spread)
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    expect_silent(plot.model.run(model))
    expect(file.exists(mpngf1), "strategy png not created")
    expect_equal(dim(png::readPNG(mpngf1)), c(600, 800, 3))
  } else {
      warning("null sodd model")
  }
})
