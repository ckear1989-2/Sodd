
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

pprint <- function(a.dt, caption="") {
  p.dt <- huxtable::hux(a.dt, add_rownames=FALSE)
  if("match_id" %in% colnames(a.dt))  {
    p.dt <- set_col_width(p.dt, col=match("match_id", colnames(a.dt)), 0.3)
  }
  p.dt <- set_bold(p.dt, row=1, col=everywhere, value=TRUE)
  p.dt <- set_all_borders(p.dt, TRUE)
  p.dt <- set_position(p.dt, "left")
  p.dt <- set_all_padding(p.dt, 0)
  p.dt <- set_outer_padding(p.dt, 0)
  p.dt <- set_caption(p.dt, caption)
  p.dt <- set_caption_pos(p.dt, "topleft")
  print_screen(p.dt, colnames=FALSE)
  resample::cat0n()
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
    missing_y2 <- a.dt[, y2][!a.dt[, order] %in% output[, order]]
    missing_order <- a.dt[, order][!a.dt[, order] %in% output[, order]]
    output <- rbind(output, data.table(y2=missing_y2, order=missing_order, q=1.00), fill=TRUE)
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

model.summary <- quote({
  # model summary
  resample::cat0n(rep("#", 30), "\nModel Summary")
  best.trees.test <- gbm::gbm.perf(model, plot.it=FALSE, method="test")
  best.trees.cv <- gbm::gbm.perf(model, plot.it=FALSE, method="cv")
  suppressMessages(best.trees.oob <- gbm::gbm.perf(model, plot.it=FALSE, method="OOB"))
  resample::cat0n("gbm perf best.trees.test=", best.trees.test)
  resample::cat0n("gbm perf best.trees.cv=", best.trees.cv)
  resample::cat0n("gbm perf best.trees.oob=", best.trees.oob)
  resample::cat0n("gbm summary")
})

score.model <- quote({
  # score
  # multiply predictions by weight.  not necessary because balanced by season but still good practice
  # e.g. if weights or balancing technique changes?
  suppressWarnings({
    train.dt[, gbmp := predict(model, train.dt, best.trees.cv, type="link") * weight]
    test.dt[, gbmp := predict(model, test.dt, best.trees.cv, type="link") * weight]
    upcoming.dt[, gbmp := predict(model, upcoming.dt, best.trees.cv, type="link") * weight]
  })
  pprint(train.dt[, list(act=sum(y), pred=sum(gbmp))], "train raw score act, pred")
  pprint(test.dt[, list(act=sum(y), pred=sum(gbmp))], "test raw score  act, pred")
  pprint(upcoming.dt[, list(act=sum(y), pred=sum(gbmp))], "upcoming raw score  act, pred")
  if(family=="bernoulli") {
    train.dt[, gbmp := gbmp + offset]
    test.dt[, gbmp := gbmp + offset]
    upcoming.dt[, gbmp := gbmp + offset]
    train.dt[, gbmp := 1/(1+exp(-gbmp))]
    test.dt[, gbmp := 1/(1+exp(-gbmp))]
    upcoming.dt[, gbmp := 1/(1+exp(-gbmp))]
  } else if(family=="gaussian") {
    train.dt[, gbmp := gbmp + offset]
    test.dt[, gbmp := gbmp + offset]
    upcoming.dt[, gbmp := gbmp + offset]
    train.dt[, gbmp := exp(gbmp)]
    test.dt[, gbmp := exp(gbmp)]
    upcoming.dt[, gbmp := exp(gbmp)]
  }
})

rebalance.model <- quote({
  # rebalance
  resample::cat0n(rep("#", 30), "\nRebalance")
  pprint(summary(train.dt[, list(y, gbmp)]), "train pre-balance act, pred")
  pprint(summary(test.dt[, list(y, gbmp)]), "test pre-balance act, pred")
  pprint(summary(upcoming.dt[, list(y, gbmp)]), "upcoming pre-balance act, pred")
  season.dt <- train.dt[, list(balance_factor=(sum(y) / sum(gbmp))), season]
  pprint(season.dt, "balance factor by season")
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
  pprint(summary(train.dt[, list(y, gbmp)]), "train post-balance act, pred")
  pprint(summary(test.dt[, list(y, gbmp)]), "test post-balance act, pred")
  pprint(summary(upcoming.dt[, list(y, gbmp)]), "upcoming post-balance act, pred")
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
  # gausiann SSE
  # bernoulli :
  resample::cat0n(rep("#", 30), "\nDeviances")
  train.a.rows <- floor(train.dt[, .N] *train.fraction)
  train.mean <- mean(train.dt[, y])
  train.dt[, mean_pred := train.mean]
  test.dt[, mean_pred := train.mean]
  calc.bernoulli.dev <- function(act, pred, weight=1) {
    pred_l <- -log((1/pred) -1)
    -2 * weight * ((act * pred_l) - log(1.0 + exp(pred_l)))
  }
  if(family == "bernoulli") {
    train.dt[, null_dev:= calc.bernoulli.dev(y, mean_pred, weight)]
    test.dt[, null_dev:= calc.bernoulli.dev(y, mean_pred, weight)]
    train.dt[, offset_dev:= calc.bernoulli.dev(y, exp(offset), weight)]
    test.dt[, offset_dev:= calc.bernoulli.dev(y, exp(offset), weight)]
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
  train.a.dt <- train.dt[1:train.a.rows, ]
  train.b.dt <- train.dt[train.a.rows:train.dt[, .N], ]
  resample::cat0n("train.a mean null dev=", sum(train.a.dt[, null_dev]) / sum(train.a.dt[, weight]))
  resample::cat0n("train.a mean offset dev=", sum(train.a.dt[, offset_dev]) / sum(train.a.dt[, weight]))
  resample::cat0n("train.a mean model dev=", sum(train.a.dt[, model_dev]) / sum(train.a.dt[, weight]))
  resample::cat0n("train.b mean null dev=", sum(train.b.dt[, null_dev]) / sum(train.b.dt[, weight]))
  resample::cat0n("train.b mean offset dev=", sum(train.b.dt[, offset_dev]) / sum(train.b.dt[, weight]))
  resample::cat0n("train.b mean model dev=", sum(train.b.dt[, model_dev]) / sum(train.b.dt[, weight]))
  resample::cat0n("test mean null dev=", sum(test.dt[, null_dev]) / sum(test.dt[, weight]))
  resample::cat0n("test mean offset dev=", sum(test.dt[, offset_dev]) / sum(test.dt[, weight]))
  resample::cat0n("test mean model dev=", sum(test.dt[, model_dev]) / sum(test.dt[, weight]))
  # how does initF work with offset?
  resample::cat0n("initF=", model$initF)
})

act.pred.summary <- quote({
  # act pred summary
  resample::cat0n(rep("#", 30), "\nActual and Predicted Summary")
  resample::cat0n("summary train act, pred")
  summary(train.dt[, list(act=spread, pred=pred_spread)])
  resample::cat0n("summary test act, pred")
  summary(test.dt[, list(act=spread, pred=pred_spread)])
  resample::cat0n("summary test act, pred")
  summary(upcoming.dt[, list(act=spread, pred=pred_spread)])
})

positive.model.predictions <- quote({
  # positive model prediciton
  resample::cat0n(rep("#", 30), "\nPositive Model Predictions")
  train.ppc <- train.dt[pred_spread > 0, .N]
  resample::cat0n("train positive prediction count ", train.ppc)
  if(train.ppc > 0) {
    resample::cat0n("train positive prediction")
    resample::cat0n("train gain", sum(train.dt[pred_spread > 0, gain]))
    positive.train.dt <- train.dt[pred_spread > 0, list(
      match_id, ftr, actr, ip, pred_odds, pred_spread, gain)][order(-pred_spread)]
    if(positive.train.dt[, .N] > 10) {
      positive.train.dt <- rbind(positive.train.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.train.dt, "train positive prediction")
  }
  test.ppc <- test.dt[pred_spread > 0, .N]
  resample::cat0n("test positive prediction count ", test.ppc)
  if(test.ppc > 0) {
    resample::cat0n("test positive prediction")
    resample::cat0n("test gain", sum(test.dt[pred_spread > 0, gain]))
    positive.test.dt <- test.dt[pred_spread > 0, list(
      match_id, ftr, actr, ip, pred_odds, pred_spread, gain)][order(-pred_spread)]
    if(positive.test.dt[, .N] > 10) {
      positive.test.dt <- rbind(positive.test.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.test.dt, "test positive prediction")
  }
  upcoming.ppc <- upcoming.dt[pred_spread > 0, .N]
  resample::cat0n("upcoming positive prediction count ", upcoming.ppc)
  if(upcoming.ppc > 0) {
    positive.upcoming.dt <- upcoming.dt[pred_spread > 0, list(
      match_id, ftr, ip, pred_odds, pred_spread)][order(-pred_spread)]
    if(positive.upcoming.dt[, .N] > 10) {
      positive.upcoming.dt <- rbind(positive.upcoming.dt[1:10, ], data.table(match_id="..."), fill=TRUE)
    }
    pprint(positive.upcoming.dt, "upcoming positive prediction")
  }
})

build.model <- quote({
  # build model
  resample::cat0n(rep("#", 30), "\nBuild Model")
  model <- gbm::gbm(
    formula=formula,
    data=train.dt,
    weights=train.dt[, weight],
    distribution=family,
    train.fraction=train.fraction,
    n.trees=n.trees,
    shrinkage=shrinkage,
    interaction.depth=interaction.depth,
    cv.folds=cv.folds,
    keep.data=FALSE,
    verbose=TRUE
  )
})

model.params <- quote({
  # model params
  resample::cat0n(rep("#", 30), "\nModel Parameters")
  resample::cat0n(
    "yvar:", yvar, "\n",
    "train.fraction:", train.fraction, "\n",
    "cv.folds:", cv.folds, "\n",
    "n.trees:", n.trees, "\n",
    "shrinkage:", shrinkage, "\n",
    "interaction.depth:", interaction.depth, "\n",
    "family:", family
  )
})

read.model.data <- quote({
  a.dt <- readRDS("~/data/R/rds/a.dt.rds")
  if(isTRUE(weights)) {
    logfile <- paste0("logs/model_", adate, "_", yvar, "_wtd", ".log")
    a.dt[, weight := weight_from_season(season)]
  } else {
    logfile <- paste0("logs/model_", adate, "_", yvar, ".log")
    a.dt[, weight := rep(1, a.dt[, .N])]
  } 
  if(!file.exists("logs")) dir.create("logs")
  sink(logfile, split=TRUE)
  # target variables
  # odds is the decimal multiplier of stake
  # gain is the weighted winnings minus stake
  # spread is the actual result (0,1) minus bookie implied odds
  a.dt[, odds := round(1 / ip, 2)]
  a.dt[, gain := (weight * odds * act) - weight]
  a.dt[, spread := act-ip]
  if(yvar == "act") {
    family <- "bernoulli"
    a.dt[, offset := log(ip)]
  } else if(yvar=="spread") {
    family <- "gaussian"
    a.dt[, offset := log(rep(1, a.dt[, .N]))]
  }
  a.dt[["y"]] <- a.dt[[yvar]]
  train.dt <- a.dt[actr != "NA" & date < as.Date(adate, "%Y-%m-%d"), ]
  test.dt <- a.dt[actr != "NA" & date >= as.Date(adate, "%Y-%m-%d"), ]
  upcoming.dt <- a.dt[actr == "NA", ]
  if(upcoming.dt[, .N] == 0) {
    stop("no upcoming matches")
  }
  if(any(train.dt[, match_id] %in% test.dt[, match_id])) stop("matches in train and test")
  if(any(train.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in train and upcoming")
  if(any(test.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in test and upcoming")
  test.matches.dt <- test.dt[, list(count=.N, distinct_matches=length(unique(hometeam)),
    teams=list(unique((c(as.character(hometeam), as.character(awayteam))))),
    contains_team_prev_played=0), date][order(date)]
  for(i in seq(2, test.matches.dt[, .N])) {
    for (j in seq((i-1), 1, -1)) {
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
})
