
source("../create.test.data.R")

test_that("test by date page", {
  set.sodd.options(
    data.dir="~/sodd.data/test.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  test.dt <- model$test.dt
  expect_silent(test.table <- detailed.test.date.data.table(test.dt))
  expect_silent(test.p.obj <- detailed.test.date.gtable(test.dt))
  pngf <- file.path(get.sodd.output.dir(), paste0("model_", adate, "_spread_test_by_date.png"))
  grDevices::png(pngf, width=800, height=600)
    gridExtra::grid.arrange(test.p.obj)
  grDevices::dev.off()
  expect(difftime(Sys.time(), file.info(pngf)$mtime, units="secs") < 10, "test by date png not created in last 10 seconds.")
})

test_that("test strategy page", {
  set.sodd.options(
    data.dir="~/sodd.data/test.data/",
    force.upcoming=TRUE,
    verbosity=0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  mpdff1 <- file.path(output.dir, paste0("model_", adate, "_spread", ".pdf"))
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_spread", "_strategy", ".png"))
  eval(read.test.model.spread)
  if (!is.null(model)) {
    test.dt <- model$test.dt
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    expect_silent(plot.model(model))
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
    model$pdffile <- mpdff1
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    expect_silent(plot.model(model))
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
