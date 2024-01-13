
source("../create.test.data.R")

# run from this folder for testing with print messages
# library("sodd")
# library("grid")
# source("../../R/constants.R")
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
#   set.sodd.options(
#     data.dir="~/sodd.data/test.data/",
#     force.upcoming=TRUE,
#     verbosity=1
#   )
#   adate <- "2023-09-01"
#   output.dir <- get.sodd.output.dir()
#   mpdff1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", ".pdf"))
#   mpngf1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", "_strategy", ".png"))
#   eval(read.test.model.no.cv)
#   print(colnames(model$train.a.dt))
#   if (!is.null(model)) {
#     test.dt <- model$test.dt
#     leagues <- unique(as.character(model$train.dt[, div]))
#     recent.dt <- get.recent.dt(leagues)
#     test.table <- detailed.strat.data.table(test.dt, recent.dt)
#     print(colnames(test.table))
#     # expect("div" %in% colnames(test.table), "div not in strategy table")
#     yvar <- "act"
#     train.dt <- model$train.dt
#     train.a.dt <- model$train.a.dt
#     train.b.dt <- model$train.b.dt
#     test.dt <- model$test.dt
#     upcoming.dt <- model$upcoming.dt
#     pdffile <- mpdff1
#     uvar <- model$uvar
#     print(colnames(train.a.dt))
#     plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile)
#   }

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
  mpdff1 <- file.path(output.dir, paste0("model_", adate, "_act", ".pdf"))
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_act", "_strategy", ".png"))
  eval(read.test.model.spread)
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    yvar <- "act"
    model.dt.list <- read.model.data(adate, yvar, FALSE, FALSE)
    a.dt <- model.dt.list[[1]]
    train.dt <- model$train.dt
    train.a.dt <- model$train.a.dt
    train.b.dt <- model$train.b.dt
    test.dt <- model$test.dt
    upcoming.dt <- model$upcoming.dt
    pdffile <- mpdff1
    uvar <- model$uvar
    expect_silent(plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile))
    expect(file.exists(mpdff1), "model pdf not created")
    expect(file.exists(mpngf1), "strategy png not created")
    expect_equal(dim(png::readPNG(mpngf1)), c(600, 800, 3))
    # expect png and pdf to be created recently
    expect(difftime(Sys.time(), file.info(mpngf1)$mtime, units="secs") < 30, "model png not created in last 30 seconds.")
    expect(difftime(Sys.time(), file.info(mpdff1)$mtime, units="secs") < 10, "model pdf not created in last 10 seconds.")
  } else {
      warning("null sodd model")
  }
})

test_that("test document model no cv", {
  set.sodd.options(
    data.dir="~/sodd.data/test.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  mpdff1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", ".pdf"))
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", "_strategy", ".png"))
  eval(read.test.model.no.cv)
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    yvar <- "act"
    train.dt <- model$train.dt
    train.a.dt <- model$train.a.dt
    train.b.dt <- model$train.b.dt
    test.dt <- model$test.dt
    upcoming.dt <- model$upcoming.dt
    pdffile <- mpdff1
    uvar <- model$uvar
    expect_silent(plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile))
    expect(file.exists(mpdff1), "model pdf not created")
    expect(file.exists(mpngf1), "strategy png not created")
    expect_equal(dim(png::readPNG(mpngf1)), c(600, 800, 3))
    # expect png and pdf to be created recently
    expect(difftime(Sys.time(), file.info(mpngf1)$mtime, units="secs") < 30, "model png not created in last 30 seconds.")
    expect(difftime(Sys.time(), file.info(mpdff1)$mtime, units="secs") < 10, "model pdf not created in last 10 seconds.")
  } else {
      warning("null sodd model")
  }
})
