
is.package.available <- function(x) {
  x %in% rownames(utils::installed.packages())
}

if(!isTRUE(is.package.available("huxtable"))) {
  message("printing data.table native style. Try install.packages(\"huxtable\")")
}

if(!is.package.available("TeachingDemos")) {
  message("not setting seed for result tie settling. Try install.packages(\"TeachingDemos\")")
}

if(!is.package.available("cronR")) {
  message("model build scheduling not available. Try install.packages(\"cronR\")")
}

if(!is.package.available("gmailr")) {
  message("email sending not available. Try install.packages(\"gmailr\")")
}

if(!is.package.available("pryr")) {
  message("performance tracking not available. Try install.packages(\"pryr\")")
}

cat0n <- function(..., verbosity=1) {
  # stolen from resample, modified for verbosity
  # cat(), but with sep = "" and a final newline.
  # cat0n("a", "b") is equivalent to cat("a", "b", "\n", sep = "")
  if(get.sodd.verbosity() >= verbosity) cat(..., sep = "", "\n")
}

weight_from_season <- function(s) {
  o <- rep(1, length(s))
  o[s =="1011"] <- 0.1
  o[s =="1112"] <- 0.2
  o[s =="1213"] <- 0.3
  o[s =="1314"] <- 0.4
  o[s =="1415"] <- 0.5
  o[s =="1516"] <- 0.6
  o[s =="1617"] <- 0.7
  o[s =="1718"] <- 0.8
  o[s =="1819"] <- 0.9
  o[s =="1920"] <- 1.0
  o[s =="2021"] <- 1.1
  o
}

pprint <- function(a.dt, caption="", verbosity=1) {
  if(!"data.table" %in% class(a.dt)) a.dt <- data.table(a.dt)
  all_na <- sapply(a.dt, function(x) all(is.na(x)) | all(x == ""))
  for(x in names(all_na)) {
    if(isTRUE(all_na[[x]])) a.dt[[x]] <- NULL
  }
  if(isTRUE(is.package.available("huxtable"))) {
    add_colnames <- TRUE
    if(any(c("V1", "V2", "N") %in% colnames(a.dt))) add_colnames <- FALSE
    p.dt <- huxtable::hux(a.dt, add_rownames=FALSE, add_colnames=add_colnames)
    col_lengths <- sapply(a.dt, function(x) {
      if(is.numeric(x)) {
        y <- max(nchar(round(x[!is.na(x)], 4)))
      } else {
        y <- max(nchar(as.character(x[!is.na(x)])))
      }
      y
    })
    col_lengths <- col_lengths/ sum(col_lengths)
    p.dt <- huxtable::set_col_width(p.dt, col_lengths)
    p.dt <- huxtable::set_bold(p.dt, row=1, col=huxtable::everywhere, value=TRUE)
    p.dt <- huxtable::set_all_borders(p.dt, TRUE)
    p.dt <- huxtable::set_position(p.dt, "left")
    p.dt <- huxtable::set_all_padding(p.dt, 0)
    p.dt <- huxtable::set_outer_padding(p.dt, 0)
    p.dt <- huxtable::set_caption(p.dt, caption)
    p.dt <- huxtable::set_caption_pos(p.dt, "topleft")
    if(get.sodd.verbosity() >= verbosity) {
      huxtable::print_screen(p.dt, min_width=0, max_width=Inf, colnames=FALSE)
      cat0n(verbosity=0)
    }
  } else {print(a.dt)}
  invisible()
}

rebase.y <- function(y1, y2, nreturn=length(y2), verbose=FALSE) {
  # stretch y2 to range of y1
  max_y1 <- max(y1)
  max_y2 <- max(y2)
  min_y1 <- min(y1)
  min_y2 <- min(y2)
  range_y2 <- max_y2 - min_y2
  range_y1 <- max_y1 - min_y1
  new_y2a <- y2 - min_y2 # max sure all are >= 0
  max_new_y2a <- max(new_y2a)
  new_y2b <- new_y2a / max_new_y2a # rescale to (0 1)
  new_y2c <- new_y2b * range_y1 # stretch to (0, range_y1)
  new_y2 <- new_y2c + min_y1 # shift to (min_y1, max_y1)
  # logic checks
  if(isTRUE(verbose)) {
    print(all(new_y2a >= 0))
    print(all(c(min(new_y2b) == 0, max(new_y2b) == 1)))
    print(all(c(min(new_y2c) == 0, max(new_y2c) == range_y1)))
    print(all(c(min(new_y2) == min_y1, max(new_y2) == max_y1)))
  }
  new_y2[1:nreturn]
}

rebase.y.sum <- function(y1, y2) {
  y2max <- y2min <- NULL
  # sum(new_y2) = sum(y1).  order maintained ignoring zeros
  y2l <- length(y2)
  if(all(y2 < 0)) {
    y2 <- (1/abs(y2))
  } else if(any(y2 < 0)) {
    pctiles_y2 <- quantile(y2, probs=seq(0, 0.99, 0.01))
    a.dt <- data.table(y2=y2, order=seq(length(y2)))
    b.dt <- data.table(y2min=pctiles_y2, q=seq(0.01, 1, 0.01))
    b.dt <- b.dt[, y2max := shift(y2min, n=1, fill=NA, type="lead")]
    b.dt <- unique(b.dt)
    b.dt[is.na(y2max), y2max := y2min]
    output <- a.dt[b.dt, on=.(y2 >= y2min, y2 <= y2max), nomatch=NA,
             .(y2, order, y2min, y2max, q)]
    # remove duplicates for some reason
    output <- unique(output, by="order")
    # for some reason 99% percentile isn't merging right
    if(a.dt[!order %in% output[, order], .N] > 0) {
      missing_y2 <- a.dt[, y2][!a.dt[, order] %in% output[, order]]
      missing_order <- a.dt[, order][!a.dt[, order] %in% output[, order]]
      output <- rbind(output, data.table(y2=missing_y2, order=missing_order, q=1.00), fill=TRUE)
    }
    output <- output[!is.na(y2), ]
    output <- output[!is.na(order), ]
    y2 <- output[, q]
    if(length(y2) != y2l) {
      print(output)
      print(summary(a.dt))
      print(summary(b.dt))
      print(summary(output))
      print(length(y2))
      print(y2l)
      print(a.dt[, order][!a.dt[, order] %in% output[, order]])
      print(output[, order][!output[, order] %in% a.dt[, order]])
      print(length(output[, order][output[, order] %in% a.dt[, order]]))
      stop("rebase.y.sum error")
    }
  }
  sum_y1 <- sum(y1[!((y1 == 0) | (y2 == 0))])
  sum_y2 <- sum(y2[!((y1 == 0) | (y2 == 0))])
  new_y2 <- y2 * sum_y1 / sum_y2 # sums are same
  new_y2[(y1 == 0) | (y2 == 0)] <- 0
  new_y2
}

#' @import gbm
model.summary <- quote({
  # model summary
  cat0n(rep("#", 30), "\nModel Summary", verbosity=2)
  best.trees.test <- gbm.perf(model, plot.it=FALSE, method="test")
  best.trees.cv <- NA
  if(get.sodd.model.params()$cv.folds > 1) best.trees.cv <- gbm.perf(model, plot.it=FALSE, method="cv")
  suppressMessages(best.trees.oob <- gbm.perf(model, plot.it=FALSE, method="OOB"))
  cat0n("gbm perf best.trees.test=", best.trees.test, verbosity=2)
  cat0n("gbm perf best.trees.cv=", best.trees.cv, verbosity=2)
  cat0n("gbm perf best.trees.oob=", best.trees.oob, verbosity=2)
  cat0n("gbm summary", verbosity=2)
})

#' @import gbm
score.a.model <- function(dt, model, family, name="gbmp", update.offset=FALSE) {
  model.pred <- weight <- offset <- ip <- NULL
  if(isTRUE(update.offset)) {
    if(model$offset_var == "ip") {
      dt[, offset := log(ip)]
    } else if(model$offset_var == "None") {
      dt[, offset := log(rep(1, dt[, .N]))]
    } else {
      prev.model <- get.prev.model(model$modelfile, model$adate)
      cat0n("scoring model ", prev.model$modelfile, " using ", prev.model$offset_var, " as offset", verbosity=1)
      dt <- score.a.model(dt, prev.model, family, "offset", update.offset=TRUE)
    }
  }
  best.trees.test <- gbm::gbm.perf(model, plot.it=FALSE, method="test")
  suppressWarnings({
    dt[, model.pred := gbm::predict.gbm(model, dt, best.trees.test, type="link")]
    # print(summary(dt[, model.pred]))
    # gaussian predictions come out at response not link scale?
    # if(family=="gaussian") dt[, model.pred := log(model.pred)]
    # print(summary(dt[, model.pred]))
    dt[, model.pred := model.pred * weight]
    # print(summary(dt[, model.pred]))
    # q()
  })
  dt[, model.pred := model.pred + offset]
  if(family=="bernoulli") {
    dt[, model.pred := 1/(1+exp(-model.pred))]
  } else if(family=="gaussian") {
    cat0n("not applying link function to gaussian pred", 1)
    # print(summary(dt[, spread]))
    # print(summary(dt[, offset]))
    # print(summary(dt[, weight]))
    # print(summary(dt[, model.pred]))
    # dt[, model.pred := exp(model.pred)]
    # print(summary(dt[, model.pred]))
    # q()
  } else if(family=="poisson") {
    dt[, model.pred := exp(model.pred)]
  }
  if(any(is.nan(dt[, model.pred]))) {
    print(summary(dt[, model.pred]))
    stop()
  }
  if(name %in% colnames(dt)) dt[[name]] <- NULL
  setnames(dt, "model.pred", name)
  dt
}

#' @import gbm
score.model <- quote({
  # score
  # multiply predictions by weight.  not necessary because balanced by season but still good practice
  # e.g. if weights or balancing technique changes?
  train.dt <- score.a.model(train.dt, model, family)
  test.dt <- score.a.model(test.dt, model, family)
  upcoming.dt <- score.a.model(upcoming.dt, model, family)
  pprint(train.dt[, list(act=sum(y), pred=sum(gbmp))], "train raw score act, pred", verbosity=2)
  pprint(test.dt[, list(act=sum(y), pred=sum(gbmp))], "test raw score  act, pred", verbosity=2)
  pprint(upcoming.dt[, list(act=sum(y), pred=sum(gbmp))], "upcoming raw score  act, pred", verbosity=2)
})

rebalance.model <- quote({
  # rebalance
  cat0n(rep("#", 30), "\nRebalance", verbosity=2)
  pprint(summary(train.dt[, list(y, gbmp)]), "train pre-balance act, pred", verbosity=2)
  pprint(summary(test.dt[, list(y, gbmp)]), "test pre-balance act, pred", verbosity=2)
  pprint(summary(upcoming.dt[, list(y, gbmp)]), "upcoming pre-balance act, pred", verbosity=2)
  season.dt <- train.dt[, list(balance_factor=(sum(y) / sum(gbmp))), season]
  pprint(season.dt, "balance factor by season", verbosity=2)
  setkey(train.dt, season)
  setkey(test.dt, season)
  setkey(upcoming.dt, season)
  setkey(season.dt, season)
  train.dt <- merge(train.dt, season.dt, all.x=TRUE, all.y=FALSE)
  train.dt[, gbmp := gbmp * balance_factor]
  test.dt <- merge(test.dt, season.dt, all.x=TRUE, all.y=FALSE)
  test.dt[is.na(balance_factor), balance_factor := season.dt[season.dt[!is.na(balance_factor), .N], balance_factor]]
  test.dt[, gbmp := gbmp * balance_factor]
  upcoming.dt <- merge(upcoming.dt, season.dt, all.x=TRUE, all.y=FALSE)
  upcoming.dt[is.na(balance_factor), balance_factor := season.dt[season.dt[!is.na(balance_factor), .N], balance_factor]]
  upcoming.dt[, gbmp := gbmp * balance_factor]
  pprint(summary(train.dt[, list(y, gbmp)]), "train post-balance act, pred", verbosity=2)
  pprint(summary(test.dt[, list(y, gbmp)]), "test post-balance act, pred", verbosity=2)
  pprint(summary(upcoming.dt[, list(y, gbmp)]), "upcoming post-balance act, pred", verbosity=2)
  # normalise probabilities by match
  if(yvar == "act") {
    train.dt[, pred_prob := gbmp]
    test.dt[, pred_prob := gbmp]
    upcoming.dt[, pred_prob := gbmp]
  }
  if(yvar == "spread") {
    train.dt[, pred_spread := gbmp]
    test.dt[, pred_spread := gbmp]
    upcoming.dt[, pred_spread := gbmp]
    train.dt[, pred_prob := gbmp + ip]
    test.dt[, pred_prob := gbmp + ip]
    upcoming.dt[, pred_prob := gbmp + ip]
  }
  if(yvar %in% c("fthg", "ftag")) {
    train.dt[, pred_prob := ip]
    test.dt[, pred_prob := ip]
    upcoming.dt[, pred_prob := ip]
  }
  train.dt[, match_pred_prob := sum(pred_prob), match_id]
  test.dt[, match_pred_prob := sum(pred_prob), match_id]
  upcoming.dt[, match_pred_prob := sum(pred_prob), match_id]
  train.dt[, pred_prob := pred_prob / match_pred_prob]
  test.dt[, pred_prob := pred_prob / match_pred_prob]
  upcoming.dt[, pred_prob := pred_prob / match_pred_prob]
  train.dt[, pred_spread := pred_prob - ip]
  test.dt[, pred_spread := pred_prob - ip]
  upcoming.dt[, pred_spread := pred_prob - ip]
  train.dt[, pred_odds := 1/pred_prob]
  test.dt[, pred_odds := 1/pred_prob]
  upcoming.dt[, pred_odds := 1/pred_prob]
  train.dt[, match_pred_prob := sum(pred_prob), match_id]
  test.dt[, match_pred_prob := sum(pred_prob), match_id]
  upcoming.dt[, match_pred_prob := sum(pred_prob), match_id]
})

calc.deviances <- quote({
  # deviances
  cat0n(rep("#", 30), "\nDeviances", verbosity=2)
  train.a.rows <- floor(train.dt[, .N] * get.sodd.model.params()$train.fraction)
  train.mean <- mean(train.dt[, y])
  train.dt[, mean_pred := train.mean]
  test.dt[, mean_pred := train.mean]
  calc.bernoulli.dev <- function(act, pred, weight=1) {
    pred_l <- -log((1/pred) -1)
    if(any(is.nan(pred_l))) {
      print(summary(gbm::predict.gbm(model, train.dt, best.trees.test, type="link")))
      print(summary(gbm::predict.gbm(model, train.dt, best.trees.test, type="link")*weight))
      print(summary(gbm::predict.gbm(model, train.dt, best.trees.test, type="link")*weight + train.dt[, offset]))
      print(summary(pred))
      print(summary(pred_l))
      stop()
    }
    -2 * weight * ((act * pred_l) - log(1.0 + exp(pred_l)))
  }
  calc.poisson.dev <- function(act, pred, weight=1) {
    2 * (act * log(act/pred) - (act - pred)) * weight
  }
  if(family == "bernoulli") {
    train.dt[, null_dev:= calc.bernoulli.dev(y, mean_pred, weight)]
    test.dt[, null_dev:= calc.bernoulli.dev(y, mean_pred, weight)]
    train.dt[, offset_dev:= calc.bernoulli.dev(y, 1/(1+exp(-offset)), weight)]
    test.dt[, offset_dev:= calc.bernoulli.dev(y, 1/(1+exp(-offset)), weight)]
    train.dt[, model_dev:= calc.bernoulli.dev(y, gbmp, weight)]
    test.dt[, model_dev:= calc.bernoulli.dev(y, gbmp, weight)]
  }
  else if(family == "gaussian") {
    train.dt[, null_dev:= ((y - mean_pred) ** 2) * weight]
    test.dt[, null_dev:= ((y - mean_pred) ** 2) * weight]
    train.dt[, offset_dev:= ((y - exp(offset)) ** 2) * weight]
    test.dt[, offset_dev:= ((y - exp(offset)) ** 2) * weight]
    train.dt[, model_dev:= ((y - gbmp) ** 2) * weight]
    test.dt[, model_dev:= ((y - gbmp) ** 2) * weight]
  }
  else if(family == "poisson") {
    train.dt[, null_dev:= calc.poisson.dev(y, mean_pred, weight)]
    test.dt[, null_dev:= calc.poisson.dev(y, mean_pred, weight)]
    train.dt[, offset_dev:= calc.poisson.dev(y, exp(offset), weight)]
    test.dt[, offset_dev:= calc.poisson.dev(y, exp(offset), weight)]
    train.dt[, model_dev:= calc.poisson.dev(y, gbmp, weight)]
    test.dt[, model_dev:= calc.poisson.dev(y, gbmp, weight)]
  }
  train.a.dt <- train.dt[1:train.a.rows, ]
  train.b.dt <- train.dt[train.a.rows:train.dt[, .N], ]
  cat0n("train.a mean null dev=", sum(train.a.dt[, null_dev]) / sum(train.a.dt[, weight]), verbosity=2)
  cat0n("train.a mean offset dev=", sum(train.a.dt[, offset_dev]) / sum(train.a.dt[, weight]), verbosity=2)
  cat0n("train.a mean model dev=", sum(train.a.dt[, model_dev]) / sum(train.a.dt[, weight]), verbosity=2)
  cat0n("train.b mean null dev=", sum(train.b.dt[, null_dev]) / sum(train.b.dt[, weight]), verbosity=2)
  cat0n("train.b mean offset dev=", sum(train.b.dt[, offset_dev]) / sum(train.b.dt[, weight]), verbosity=2)
  cat0n("train.b mean model dev=", sum(train.b.dt[, model_dev]) / sum(train.b.dt[, weight]), verbosity=2)
  cat0n("test mean null dev=", sum(test.dt[, null_dev]) / sum(test.dt[, weight]), verbosity=2)
  cat0n("test mean offset dev=", sum(test.dt[, offset_dev]) / sum(test.dt[, weight]), verbosity=2)
  cat0n("test mean model dev=", sum(test.dt[, model_dev]) / sum(test.dt[, weight]), verbosity=2)
  # how does initF work with offset?
  cat0n("initF=", model$initF, verbosity=2)
})

act.pred.summary <- quote({
  # act pred summary
  cat0n(rep("#", 30), "\nActual and Predicted Summary", verbosity=2)
  cat0n("summary train act, pred", verbosity=2)
  summary(train.dt[, list(act=spread, pred=pred_spread)])
  cat0n("summary test act, pred", verbosity=2)
  summary(test.dt[, list(act=spread, pred=pred_spread)])
  cat0n("summary test act, pred", verbosity=2)
  summary(upcoming.dt[, list(act=spread, pred=pred_spread)])
})

positive.model.predictions <- quote({
  # positive model prediciton
  cat0n(rep("#", 30), "\nPositive Model Predictions", verbosity=2)
  train.ppc <- train.dt[pred_spread > 0, .N]
  cat0n("train positive prediction count ", train.ppc, verbosity=2)
  if(train.ppc > 0) {
    cat0n("train positive prediction", verbosity=2)
    cat0n("train gain", sum(train.dt[pred_spread > 0, gain]), verbosity=2)
    positive.train.dt <- train.dt[pred_spread > 0, list(
      match_id, ftr, actr, ip, pred_odds, pred_spread, gain)][order(-pred_spread)]
    if(positive.train.dt[, .N] > 10) {
      positive.train.dt <- rbind(positive.train.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.train.dt, "train positive prediction", verbosity=2)
  }
  test.ppc <- test.dt[pred_spread > 0, .N]
  cat0n("test positive prediction count ", test.ppc, verbosity=2)
  if(test.ppc > 0) {
    cat0n("test positive prediction", verbosity=2)
    cat0n("test gain", sum(test.dt[pred_spread > 0, gain]), verbosity=2)
    positive.test.dt <- test.dt[pred_spread > 0, list(
      match_id, ftr, actr, ip, pred_odds, pred_spread, gain)][order(-pred_spread)]
    if(positive.test.dt[, .N] > 10) {
      positive.test.dt <- rbind(positive.test.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.test.dt, "test positive prediction", verbosity=2)
  }
  upcoming.ppc <- upcoming.dt[pred_spread > 0, .N]
  cat0n("upcoming positive prediction count ", upcoming.ppc, verbosity=2)
  if(upcoming.ppc > 0) {
    positive.upcoming.dt <- upcoming.dt[pred_spread > 0, list(
      match_id, ftr, ip, pred_odds, pred_spread)][order(-pred_spread)]
    if(positive.upcoming.dt[, .N] > 10) {
      positive.upcoming.dt <- rbind(positive.upcoming.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.upcoming.dt, "upcoming positive prediction", verbosity=2)
  }
})

#' @import gbm
build.model <- quote({
  cat0n(rep("#", 30), "\nBuild Model", verbosity=2)
  model <- gbm(
    formula=formula,
    data=train.dt,
    weights=train.dt[, weight],
    distribution=family,
    train.fraction=get.sodd.model.params()$train.fraction,
    n.trees=get.sodd.model.params()$n.trees,
    shrinkage=get.sodd.model.params()$shrinkage,
    interaction.depth=get.sodd.model.params()$interaction.depth,
    cv.folds=get.sodd.model.params()$cv.folds,
    keep.data=FALSE,
    n.cores=get.sodd.model.params()$n.cores,
    verbose=ifelse(get.sodd.verbosity() >= 2, TRUE, FALSE)
  )
  model$adate <- adate
  model$modelfile <- modelfile
  model$offset_var <- offset_var
  report.memory(model)
  if(get.sodd.model.params()$cv.folds == 1) model$cv.error <- model$valid.error
})

model.params <- quote({
  # model params
  cat0n(rep("#", 30), "\nModel Parameters", verbosity=2)
  cat0n(
    "yvar: ", yvar, "\n",
    "train.fraction: ", get.sodd.model.params()$train.fraction, "\n",
    "cv.folds: ", get.sodd.model.params()$cv.folds, "\n",
    "n.cores: ", get.sodd.model.params()$n.cores, "\n",
    "n.trees: ", get.sodd.model.params()$n.trees, "\n",
    "shrinkage: ", get.sodd.model.params()$shrinkage, "\n",
    "interaction.depth: ", get.sodd.model.params()$interaction.depth, "\n",
    "family: ", family,
    verbosity=2
  )
})

get.prev.model <- function(model.file, adate) {
  prev.model <- NULL
  model.dir <- paste0(get.sodd.output.dir(), "models/")
  if(file.exists(model.dir)) {
    for(i in seq(1, 100, 1)){
      prev.date <- format(as.Date(adate, "%Y-%m-%d") - i, "%Y-%m-%d")
      prev.model.file <- gsub(adate, prev.date, model.file)[[1]]
      if(file.exists(prev.model.file)) {
        return(readRDS(prev.model.file))
       }
    }
  }
  prev.model
}

read.model.data <- function(adate, yvar, previous.model.as.offset, weights) {
  act <- actr <- awayteam <- contains_team_prev_played <- gain <- hometeam <-
  ip <- match_id <- odds <- offset <- season <- spread <- teams <- weight <-
  NULL
  a.dt <- readRDS(file.path(get.sodd.data.dir(), "a.dt.rds"))
  output.dir <- get.sodd.output.dir()
  if(isTRUE(weights)) {
    output.prefix <- paste0("model_", adate, "_", yvar, "_wtd")
    a.dt[, weight := weight_from_season(season)]
  } else {
    output.prefix <- paste0("model_", adate, "_", yvar)
    a.dt[, weight := rep(1, a.dt[, .N])]
  } 
  logfile <- paste0(output.dir, output.prefix, ".log")
  pdffile <- paste0(output.dir, output.prefix, ".pdf")
  modelfile <- paste0(output.dir, "models/", output.prefix, ".rds")
  if(get.sodd.verbosity() >= 1) {
    if(!file.exists(output.dir)) dir.create(output.dir)
    sink(logfile, split=TRUE)
  } else {
    sink("/dev/null")
  }
  # target variables
  # odds is the decimal multiplier of stake
  # gain is the weighted winnings minus stake
  # spread is the actual result (0,1) minus bookie implied odds
  a.dt[, odds := round(1 / ip, 2)]
  a.dt[, gain := (weight * odds * act) - weight]
  a.dt[, spread := act-ip]
  if(yvar == "act") {
    family <- "bernoulli"
  } else if(yvar=="spread") {
    family <- "gaussian"
  } else if(yvar=="fthg") {
    family <- "poisson"
    a.dt <- restandardise.model.dt(a.dt, "fthg")
  } else if(yvar=="ftag") {
    family <- "poisson"
    a.dt <- restandardise.model.dt(a.dt, "ftag")
  }
  # use previous model as offset
  offset_var <- NULL
  prev.model <- get.prev.model(modelfile, adate)
  if(!is.null(prev.model) & isTRUE(previous.model.as.offset)) {
    # need to have same offset as prev.model
    cat0n("using model from ", prev.model$adate, " as offset", verbosity=1)
    offset_var <- prev.model$modelfile
    a.dt <- score.a.model(a.dt, prev.model, family, "offset", update.offset=TRUE)
  } else {
    if(yvar == "act") {
      offset_var <- "ip"
      a.dt[, offset := log(ip)]
    } else if(yvar=="spread") {
      offset_var <- "None"
      a.dt[, offset := log(rep(1, a.dt[, .N]))]
    } else if(yvar=="fthg") {
      offset_var <- "None"
      a.dt[, offset := log(rep(1, a.dt[, .N]))]
    } else if(yvar=="ftag") {
      offset_var <- "None"
      a.dt[, offset := log(rep(1, a.dt[, .N]))]
    }
  }
  cat0n("using ", offset_var, " as offset", verbosity=1)
  a.dt[["y"]] <- a.dt[[yvar]]
  train.dt <- a.dt[actr != "NA" & date < as.Date(adate, "%Y-%m-%d"), ]
  test.dt <- a.dt[actr != "NA" & date >= as.Date(adate, "%Y-%m-%d"), ]
  upcoming.dt <- a.dt[actr == "NA", ]
  if(test.dt[, .N] == 0) {
    warning("no test matches")
    return(FALSE)
  }
  if(upcoming.dt[, .N] == 0) {
    if(isTRUE(get.sodd.force.upcoming())) {
      upcoming.dt <- test.dt[date == max(test.dt[, date]), ]
      test.dt <- test.dt[date < max(test.dt[, date]), ]
      upcoming.dt[, actr := NULL]
      upcoming.dt[, actr := "NA"]
      cat0n("forcing upcoming matches from test.dt", verbosity=1)
    }
    else {
      warning("no upcoming matches")
      return(FALSE)
    }
  }
  if(any(train.dt[, match_id] %in% test.dt[, match_id])) stop("matches in train and test")
  if(any(train.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in train and upcoming")
  if(any(test.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in test and upcoming")
  cat0n("setting test.dt to one set of fixtures", verbosity=2)
  test.matches.dt <- test.dt[, list(
    count=.N,
    distinct_matches=length(unique(hometeam)),
    teams=list(unique((c(hometeam, awayteam)))),
    contains_team_prev_played=0
    ), date][order(date)]
  for(i in seq(2, test.matches.dt[, .N])) {
    for (j in seq(1, (i-1), 1)) {
      if(any(test.matches.dt[i, teams][[1]] %in% test.matches.dt[j, teams][[1]])) {
        test.matches.dt[i:test.matches.dt[, .N], contains_team_prev_played := 1]
        break
      }
    }
  }
  test.matches.one.match.dt <- test.matches.dt[contains_team_prev_played == 0 , list(date)]
  setkey(test.matches.one.match.dt, date)
  setkey(test.dt, date)
  test.dt <- merge(test.matches.one.match.dt, test.dt, all.x=TRUE, all.y=FALSE)
  cat0n("test.dt complete", verbosity=2)
  TRUE
}

#' @import data.table
get.recent.dt <- function(leagues=all.leagues){
  date <- match_id <- hometeam <- awayteam <- actr_new <- ftr <- NULL
  recent.csv <- paste0(get.sodd.data.dir(), all.years[[1]], "/", leagues, ".csv")
  recent.dt <- rbindlist(lapply(recent.csv, fread), fill=TRUE)
  setnames(recent.dt, colnames(recent.dt), tolower(colnames(recent.dt)))
  recent.dt[, date := as.Date(date, '%d/%m/%y')]
  recent.dt[, match_id := paste0(
    date, "|", hometeam, "|", awayteam, collapse="|"),
  list(date, hometeam, awayteam)]
  recent.dt <- recent.dt[, list(match_id, actr_new=ftr)]
  recent.dt
}

