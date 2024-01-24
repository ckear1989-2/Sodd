source("../create.test.data.R")

test_that("model response vars page", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  pdff <- file.path(get.sodd.output.dir(), paste0("model_", adate, "_spread_model_response_vars.pdf"))
  expect_silent(response.p.obj <- plot_response_vars(model))
  grDevices::pdf(pdff, h = 7, w = 14)
  expect_silent({
    grid.arrange(response.p.obj)
  })
  grDevices::dev.off()
  expect(difftime(Sys.time(), file.info(pdff)$mtime, units = "secs") < 10, "model response vars page not created in last 10 seconds.")
})

test_that("model univariates pages", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  pdff <- file.path(get.sodd.output.dir(), paste0("model_", adate, "_spread_model_univariates.pdf"))
  expect_silent(univ.p.objs <- plot_model_univariates(model))
  grDevices::pdf(pdff, h = 7, w = 14)
  expect_silent({
    for (p in univ.p.objs) {
      expect_equal(length(p), 5)
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], ncol = 2)
      grid.arrange(p[[5]], p[[5]], p[[5]], p[[5]], ncol = 2, newpage = FALSE)
    }
  })
  grDevices::dev.off()
  expect(difftime(Sys.time(), file.info(pdff)$mtime, units = "secs") < 10, "model univs pages not created in last 10 seconds.")
})

test_that("model params page", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  expect_silent(model.run.p.obj <- plot.model.run(model))
  pngf <- file.path(get.sodd.output.dir(), paste0("model_", adate, "_spread_model_params.png"))
  grDevices::png(pngf, width = 600, height = 240)
  gridExtra::grid.arrange(model.run.p.obj)
  grDevices::dev.off()
  expect(difftime(Sys.time(), file.info(pngf)$mtime, units = "secs") < 10, "model params png not created in last 10 seconds.")
})

test_that("test by date page", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  eval(read.test.model.spread)
  test.dt <- model$test.dt
  expect_silent(test.table <- detailed.test.date.data.table(test.dt))
  expect_silent(test.p.obj <- detailed.test.date.gtable(test.dt))
  pngf <- file.path(get.sodd.output.dir(), paste0("model_", adate, "_spread_test_by_date.png"))
  grDevices::png(pngf, width = 800, height = 600)
  gridExtra::grid.arrange(test.p.obj)
  grDevices::dev.off()
  expect(difftime(Sys.time(), file.info(pngf)$mtime, units = "secs") < 10, "test by date png not created in last 10 seconds.")
})

test_that("test strategy page", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
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
    expect(difftime(Sys.time(), file.info(mpngf1)$mtime, units = "secs") < 30, "model png not created in last 30 seconds.")
    expect(difftime(Sys.time(), file.info(mpdff1)$mtime, units = "secs") < 20, "model pdf not created in last 20 seconds.")
  } else {
    warning("null sodd model")
  }
})

test_that("test document model no cv", {
  set.sodd.options(
    data.dir = "~/sodd.data/test.data/",
    force.upcoming = TRUE,
    verbosity = 0
  )
  adate <- "2023-09-01"
  output.dir <- get.sodd.output.dir()
  mpdff1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", ".pdf"))
  mpngf1 <- file.path(output.dir, paste0("model_", adate, "_act", "_no_cv", "_strategy", ".png"))
  eval(read.test.model.no.cv)
  expect_equal(model$params$num_trees, 50)
  expect_equal(model$cv_folds, 1)
  if (!is.null(model)) {
    test.dt <- model$test.dt
    model$pdffile <- mpdff1
    leagues <- unique(as.character(model$train.dt[, div]))
    expect_silent(recent.dt <- get.recent.dt(leagues))
    test.table <- detailed.strat.data.table(test.dt, recent.dt)
    expect("div" %in% colnames(test.table), "div not in strategy table")
    w <- capture_warnings(plot.model(model))
    expect_match(w, "plotting model perf with no cv.", all = FALSE)
    expect_match(w, "y2 values constant when attempting rebase.", all = FALSE)
    expect(file.exists(mpdff1), "model pdf not created")
    expect(file.exists(mpngf1), "strategy png not created")
    expect_equal(dim(png::readPNG(mpngf1)), c(600, 800, 3))
    # expect png and pdf to be created recently
    expect(difftime(Sys.time(), file.info(mpngf1)$mtime, units = "secs") < 30, "model png not created in last 30 seconds.")
    expect(difftime(Sys.time(), file.info(mpdff1)$mtime, units = "secs") < 10, "model pdf not created in last 10 seconds.")
  } else {
    warning("null sodd model")
  }
})
