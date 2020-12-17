
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
    print(output[, .N])
    output <- unique(output, by="order")
    print(output[, .N])
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
  best.trees <- gbm::gbm.perf(model, plot.it=FALSE, method="test")
  resample::cat0n("gbm perf best.trees=", best.trees)
  resample::cat0n("gbm summary")
  summary(model, plotit=FALSE)
})

score.model <- quote({
  # score
  # multiply predictions by weight.  not necessary because balanced by season but still good practice
  # e.g. if weights or balancing technique changes?
  train.dt[, gbmp := predict(model, train.dt, best.trees, type="response") * weight]
  test.dt[, gbmp := predict(model, test.dt, best.trees, type="response") * weight]
  upcoming.dt[, gbmp := predict(model, upcoming.dt, best.trees, type="response") * weight]
})

rebalance.model <- quote({
  # rebalance
  resample::cat0n(rep("#", 30), "\nRebalance")
  resample::cat0n("train pre-balance act, pred")
  print(train.dt[, list(act=sum(y), pred=sum(gbmp))])
  resample::cat0n("test pre-balance act, pred")
  print(test.dt[, list(act=sum(y), pred=sum(gbmp))])
  season.dt <- train.dt[, list(balance_factor=(sum(y) / sum(gbmp))), season]
  resample::cat0n("balance factor by season")
  test.dt[, list(act=sum(y), pred=sum(gbmp))]
  print(season.dt)
  setkey(train.dt, season)
  setkey(test.dt, season)
  setkey(season.dt, season)
  train.dt <- merge(train.dt, season.dt, all.x=TRUE, all.y=FALSE)
  test.dt <- merge(test.dt, season.dt, all.x=TRUE, all.y=FALSE)
  train.dt[, gbmp := gbmp * balance_factor]
  test.dt[is.na(balance_factor), balance_factor := season.dt[season.dt[!is.na(balance_factor), .N], balance_factor]]
  test.dt[, gbmp := gbmp * balance_factor]
  resample::cat0n("train post-balance act, pred")
  print(train.dt[, list(act=sum(y), pred=sum(gbmp))])
  resample::cat0n("test post-balance act, pred")
  print(test.dt[, list(act=sum(y), pred=sum(gbmp))])
  resample::cat0n("upcoming post-balance act, pred")
  print(upcoming.dt[, list(act=sum(y), pred=sum(gbmp))])
  if(yvar == "act") {
    train.dt[, pred_spread := gbmp - ip]
    test.dt[, pred_spread := gbmp - ip]
    upcoming.dt[, pred_spread := gbmp - ip]
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
  train.dt[, pred_odds := 1/pred_prob]
  test.dt[, pred_odds := 1/pred_prob]
  upcoming.dt[, pred_odds := 1/pred_prob]
})

calc.deviances <- quote({
  # deviances
  # gausiann SSE
  # bernoulli :
  # adWeight[i]*(adY[i]*adF[i] - log(1.0+exp(adF[i])));
  resample::cat0n(rep("#", 30), "\nDeviances")
  train.a.rows <- floor(train.dt[, .N] *train.fraction)
  train.mean <- mean(train.dt[, y])
  train.dt[, mean_pred := train.mean]
  train.dt[, null_dev:= ((y - mean_pred) ** 2) * weight]
  train.dt[, model_dev:= ((y - gbmp) ** 2) * weight]
  test.dt[, mean_pred := train.mean]
  test.dt[, null_dev:= ((y - mean_pred) ** 2) * weight]
  test.dt[, model_dev:= ((y - gbmp) ** 2) * weight]
  train.a.dt <- train.dt[1:train.a.rows, ]
  train.b.dt <- train.dt[train.a.rows:train.dt[, .N], ]
  resample::cat0n("train.a mean null dev=", mean(train.a.dt[, null_dev]))
  resample::cat0n("train.a mean model dev=", mean(train.a.dt[, model_dev]))
  resample::cat0n("train.b mean null dev=", mean(train.b.dt[, null_dev]))
  resample::cat0n("train.b mean model dev=", mean(train.b.dt[, model_dev]))
  resample::cat0n("test mean null dev=", mean(test.dt[, null_dev]))
  resample::cat0n("test mean model dev=", mean(test.dt[, model_dev]))
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
    print(train.dt[pred_spread > 0, list(date, hometeam, awayteam, ftr, actr, ip, pred_spread, y)][order(-pred_spread)])
    resample::cat0n("train y", sum(train.dt[pred_spread > 0, y]))
    resample::cat0n("train mean y", mean(train.dt[pred_spread > 0, y]))
  }
  test.ppc <- test.dt[pred_spread > 0, .N]
  resample::cat0n("test positive prediction count ", test.ppc)
  if(test.ppc > 0) {
    resample::cat0n("test positive prediction")
    print(test.dt[pred_spread > 0, list(date, hometeam, awayteam, ftr, actr, ip, pred_spread, y)][order(-pred_spread)])
    resample::cat0n("test y", sum(test.dt[pred_spread > 0, y]))
    resample::cat0n("test mean y", mean(test.dt[pred_spread > 0, y]))
  }
  upcoming.ppc <- upcoming.dt[pred_spread > 0, .N]
  resample::cat0n("upcoming positive prediction count ", upcoming.ppc)
  if(upcoming.ppc > 0) {
    resample::cat0n("upcoming positive prediction")
    print(upcoming.dt[pred_spread > 0, list(date, hometeam, awayteam, ftr, actr, ip, pred_spread, y)][order(-pred_spread)])
    resample::cat0n("upcoming y", sum(upcoming.dt[pred_spread > 0, y]))
    resample::cat0n("upcoming mean y", mean(upcoming.dt[pred_spread > 0, y]))
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
    keep.data=FALSE,
    verbose=TRUE
  )
})

model.params <- quote({
  # model params
  resample::cat0n(rep("#", 30), "\nModel Parameters")
  resample::cat0n(
    "train.fraction:", train.fraction, "\n",
    "n.trees:", n.trees, "\n",
    "shrinkage:", shrinkage, "\n",
    "interaction.depth:", interaction.depth, "\n",
    "yvar:", yvar, "\n",
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
  sink(logfile)
  # target variables
  # odds is the decimal multiplier of stake
  # gain is the weighted winnings minus stake
  # spread is the actual result (0,1) minus bookie implied odds
  a.dt[, odds := round(1 / ip, 2)]
  a.dt[, gain := (weight * odds * act) - weight]
  a.dt[, spread := act-ip]
  if(yvar == "spread") family <- "gaussian"
  if(yvar == "act") family <- "bernoulli"
  a.dt[["y"]] <- a.dt[[yvar]]
  train.dt <- a.dt[actr != "NA" & date < as.Date(adate, "%Y-%m-%d"), ]
  test.dt <- a.dt[actr != "NA" & date >= as.Date(adate, "%Y-%m-%d"), ]
  upcoming.dt <- a.dt[actr == "NA", ]
  if(any(train.dt[, match_id] %in% test.dt[, match_id])) stop("matches in train and test")
  if(any(train.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in train and upcoming")
  if(any(test.dt[, match_id] %in% upcoming.dt[, match_id])) stop("matches in test and upcoming")
  test.matches.dt <- test.dt[, list(count=.N, distinct_matches=length(unique(hometeam)),
    teams=list(unique((c(as.character(hometeam), as.character(awayteam))))),
    contains_team_prev_played=0), date][order(date)]
  for(i in seq(2, test.matches.dt[, .N])) {
    for (j in seq((i-1), 1, -1)) {
      if(any(test.matches.dt[i, teams][[1]] %in% test.matches.dt[j, teams][[1]])) {
        test.matches.dt[i, contains_team_prev_played := 1]
        next
      }
    }
  }
  test.matches.one.match.dt <- test.matches.dt[contains_team_prev_played == 0 , list(date)]
  setkey(test.matches.one.match.dt, date)
  setkey(test.dt, date)
  test.dt <- merge(test.matches.one.match.dt, test.dt, all.x=TRUE, all.y=FALSE)
})
