.libPaths("~/data/R/packages")
suppressPackageStartupMessages({
  library("data.table")
  library("gbm")
  library("ggplot2")
  library("gridExtra")
  library("resample")
})
source("analysis/plot.R")
source("analysis/strategy.R")
source("utils/utils.R")

build.a.model <- function(adate, weights=FALSE) {
  # print to log file
  set.seed(123)
  a.dt <- readRDS("~/data/R/rds/a.dt.rds")
  if(isTRUE(weights)) {
    logfile <- paste0("logs/model_", adate, "_wtd", ".log")
    a.dt[, weight := weight_from_season(season)]
  } else {
    logfile <- paste0("logs/model_", adate, ".log")
    a.dt[, weight := rep(1, a.dt[, .N])]
  } 
  if(!file.exists("logs")) dir.create("logs")
  sink(logfile)
  a.dt[, odds := 1/ip]
  a.dt[, gain := (weight * odds * act) - weight]
  a.dt[, spread := act-ip]
  yvar <- "spread"
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
  test.matches.one.match.dt <- test.matches.dt[contains_team_prev_played ==0 , list(date)]
  setkey(test.matches.one.match.dt, date)
  setkey(test.dt, date)
  test.dt <- merge(test.matches.one.match.dt, test.dt, all.x=TRUE, all.y=FALSE)
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:4),
    paste0("app", 1:4),
    paste0("hpd", 1:4),
    paste0("apd", 1:4),
    paste0("hphp", 1:3),
    paste0("apap", 1:3),
    paste0("hphd", 1:3),
    paste0("apad", 1:3),
    paste0("hpp_cum", 2:5),
    paste0("app_cum", 2:5)
  )
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  formula <- as.formula(paste("y", paste(xvar, collapse="+"), sep="~"))
  
  # model params
  train.fraction <- 0.7
  n.trees <- 20
  shrinkage <- 0.01
  interaction.depth <- 3
  cat0n(rep("#", 30), "\nModel Parameters")
  cat0n(
    "train.fraction:", train.fraction, "\n",
    "n.trees:", n.trees, "\n",
    "shrinkage:", shrinkage, "\n",
    "interaction.depth:", interaction.depth
  )
  
  # build model
  cat0n(rep("#", 30), "\nBuild Model")
  model <- gbm(
    formula=formula,
    data=train.dt,
    weights=train.dt[, weight],
    distribution="gaussian",
    train.fraction=train.fraction,
    n.trees=n.trees,
    shrinkage=shrinkage,
    interaction.depth=interaction.depth,
    keep.data=FALSE,
    verbose=TRUE
  )

  # model summary
  cat0n(rep("#", 30), "\nModel Summary")
  best.trees <- gbm.perf(model, plot.it=FALSE, method="test")
  cat0n("gbm perf best.trees=", best.trees)
  cat0n("gbm summary")
  summary(model, plotit=FALSE)
  
  # score
  # multiply predictions by weight.  not necessary because balanced by season but still good practice
  # e.g. if weights or balancing technique changes?
  train.dt[, gbmp := predict(model, train.dt, best.trees) * weight]
  test.dt[, gbmp := predict(model, test.dt, best.trees) * weight]
  upcoming.dt[, gbmp := predict(model, upcoming.dt, best.trees) * weight]
  
  # rebalance
  cat0n(rep("#", 30), "\nRebalance")
  cat0n("train pre-balance act, pred")
  print(train.dt[, list(act=sum(y), pred=sum(gbmp))])
  cat0n("test pre-balance act, pred")
  print(test.dt[, list(act=sum(y), pred=sum(gbmp))])
  season.dt <- train.dt[, list(balance_factor=(sum(y) / sum(gbmp))), season]
  cat0n("balance factor by season")
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
  cat0n("train post-balance act, pred")
  print(train.dt[, list(act=sum(y), pred=sum(gbmp))])
  cat0n("test post-balance act, pred")
  print(test.dt[, list(act=sum(y), pred=sum(gbmp))])
  cat0n("upcoming post-balance act, pred")
  print(upcoming.dt[, list(act=sum(y), pred=sum(gbmp))])
  
  # deviances
  cat0n(rep("#", 30), "\nDeviances")
  train.a.rows <- floor(train.dt[, .N] *train.fraction)
  train.mean <- mean(train.dt[, y])
  train.dt[, mean_pred := train.mean]
  train.dt[, null_dev:= ((y - mean_pred) ** 2)]
  train.dt[, model_dev:= ((y - gbmp) ** 2)]
  test.dt[, mean_pred := train.mean]
  test.dt[, null_dev:= ((y - mean_pred) ** 2)]
  test.dt[, model_dev:= ((y - gbmp) ** 2)]
  train.a.dt <- train.dt[1:train.a.rows, ]
  train.b.dt <- train.dt[train.a.rows:train.dt[, .N], ]
  cat0n("train.a mean null dev=", mean(train.a.dt[, null_dev]))
  cat0n("train.a mean model dev=", mean(train.a.dt[, model_dev]))
  cat0n("train.b mean null dev=", mean(train.b.dt[, null_dev]))
  cat0n("train.b mean model dev=", mean(train.b.dt[, model_dev]))
  cat0n("test mean null dev=", mean(test.dt[, null_dev]))
  cat0n("test mean model dev=", mean(test.dt[, model_dev]))
  
  # act pred summary
  cat0n(rep("#", 30), "\nActual and Predicted Summary")
  cat0n("summary train act, pred")
  summary(train.dt[, list(act=y, pred=gbmp)])
  cat0n("summary test act, pred")
  summary(test.dt[, list(act=y, pred=gbmp)])
  cat0n("summary test act, pred")
  summary(upcoming.dt[, list(act=y, pred=gbmp)])
  
  # positive model prediciton
  cat0n(rep("#", 30), "\nPositive Model Predictions")
  train.ppc <- train.dt[gbmp > 0, .N]
  cat0n("train positive prediction count ", train.ppc)
  if(train.ppc > 0) {
    cat0n("train positive prediction")
    print(train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, y)][order(-gbmp)])
    cat0n("train y", sum(train.dt[gbmp>0, y]))
    cat0n("train mean y", mean(train.dt[gbmp>0, y]))
  }
  test.ppc <- test.dt[gbmp > 0, .N]
  cat0n("test positive prediction count ", test.ppc)
  if(test.ppc > 0) {
    cat0n("test positive prediction")
    print(test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, y)][order(-gbmp)])
    cat0n("test y", sum(test.dt[gbmp>0, y]))
    cat0n("test mean y", mean(test.dt[gbmp>0, y]))
  }
  upcoming.ppc <- upcoming.dt[gbmp > 0, .N]
  cat0n("upcoming positive prediction count ", upcoming.ppc)
  if(upcoming.ppc > 0) {
    cat0n("upcoming positive prediction")
    print(upcoming.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, y)][order(-gbmp)])
    cat0n("upcoming y", sum(upcoming.dt[gbmp>0, y]))
    cat0n("upcoming mean y", mean(upcoming.dt[gbmp>0, y]))
  }
  
  # strategies
  run.strategy(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  
  # plots
  plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, logfile)
  
  sink()
}

build.a.model("2020-08-01")
# build.a.model("2020-08-01", weights=TRUE)
# build.a.model("2020-09-01")
# build.a.model("2020-09-01", weights=TRUE)
# build.a.model("2020-10-01")
# build.a.model("2020-10-01", weights=TRUE)
# build.a.model("2020-11-01")
# build.a.model("2020-11-01", weights=TRUE)
# build.a.model("2020-12-01")
# build.a.model("2020-12-01", weights=TRUE)

