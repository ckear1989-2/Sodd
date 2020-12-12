.libPaths("~/data/R/packages")
suppressPackageStartupMessages({
  library("data.table")
  library("gbm")
  library("ggplot2")
  library("gridExtra")
  library("resample")
})
source("analysis/plot.R")
source("utils/utils.R")

build.a.model <- function(adate, weights=FALSE) {
  set.seed(123)
  a.dt <- readRDS("~/data/R/rds/a.dt.rds")
  # print to log file
  if(!file.exists("logs")) dir.create("logs")
  if(isTRUE(weights)) {
    logfile <- paste0("logs/model_", adate, "_wtd", ".log")
    a.dt[, weight := weight_from_season(season)]
  } else {
    logfile <- paste0("logs/model_", adate, ".log")
    a.dt[, weight := rep(1, a.dt[, .N])]
  }
  sink(logfile)
  train.dt <- a.dt[date < as.Date(adate, "%Y-%m-%d"), ]
  test.dt <- a.dt[date >= as.Date(adate, "%Y-%m-%d"), ]
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
  yvar <- "gain"
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))
  
  # model params
  train.fraction <- 0.7
  n.trees <- 250
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
  
  # rebalance
  cat0n(rep("#", 30), "\nRebalance")
  cat0n("train pre-balance act, pred")
  print(train.dt[, list(act=sum(gain), pred=sum(gbmp))])
  cat0n("test pre-balance act, pred")
  print(test.dt[, list(act=sum(gain), pred=sum(gbmp))])
  season.dt <- train.dt[, list(balance_factor=(sum(gain) / sum(gbmp))), season]
  cat0n("balance factor by season")
  test.dt[, list(act=sum(gain), pred=sum(gbmp))]
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
  print(train.dt[, list(act=sum(gain), pred=sum(gbmp))])
  cat0n("test post-balance act, pred")
  print(test.dt[, list(act=sum(gain), pred=sum(gbmp))])
  
  # deviances
  cat0n(rep("#", 30), "\nDeviances")
  train.a.rows <- floor(train.dt[, .N] *train.fraction)
  train.mean <- mean(train.dt[, gain])
  train.dt[, mean_pred := train.mean]
  train.dt[, null_dev:= ((gain - mean_pred) ** 2)]
  train.dt[, model_dev:= ((gain - gbmp) ** 2)]
  test.dt[, mean_pred := train.mean]
  test.dt[, null_dev:= ((gain - mean_pred) ** 2)]
  test.dt[, model_dev:= ((gain - gbmp) ** 2)]
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
  summary(train.dt[, list(act=gain, pred=gbmp)])
  cat0n("summary test act, pred")
  summary(test.dt[, list(act=gain, pred=gbmp)])
  
  # positive model prediciton
  cat0n(rep("#", 30), "\nPositive Model Predictions")
  train.ppc <- train.dt[gbmp > 0, .N]
  cat0n("train positive prediction count ", train.ppc)
  if(train.ppc > 0) {
    cat0n("train positive prediction")
    train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
    cat0n("train gain", sum(train.dt[gbmp>0, gain]))
    cat0n("train mean gain", mean(train.dt[gbmp>0, gain]))
  }
  test.ppc <- test.dt[gbmp > 0, .N]
  cat0n("test positive prediction count ", test.ppc)
  if(test.ppc > 0) {
    cat0n("test positive prediction")
    test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
    cat0n("test gain", sum(test.dt[gbmp>0, gain]))
    cat0n("test mean gain", mean(test.dt[gbmp>0, gain]))
  }
  
  # strategies
  cat0n(rep("#", 30), "\nStrategies")
  train.a.dt[, strat_all := 1]
  train.a.dt[, strat_all_wtd := 0]
  train.a.dt[strat_all > 0, strat_all_wtd :=
    gbmp *
    sum(train.a.dt[strat_all > 0, strat_all]) /
    sum(train.a.dt[strat_all > 0, gbmp])
  ]
  train.a.dt[, gain_all_wtd  := strat_all_wtd * gain]
  train.b.dt[, strat_all := 1]
  train.b.dt[, strat_all_wtd := 0]
  train.b.dt[strat_all > 0, strat_all_wtd :=
    gbmp *
    sum(train.b.dt[strat_all > 0, strat_all]) /
    sum(train.b.dt[strat_all > 0, gbmp])
  ]
  train.b.dt[, gain_all_wtd  := strat_all_wtd * gain]
  # bet on every result sum(gain)
  test.dt[, strat_all := 1]
  # bet on all favourites
  test.dt[, max_ip := max(ip), list(date, hometeam, awayteam)]
  test.dt[, strat_fav := 0]
  test.dt[ip == max_ip, strat_fav := 1]
  test.dt[, gain_fav := strat_fav * gain]
  # bet on all outsiders
  test.dt[, min_ip := min(ip), list(date, hometeam, awayteam)]
  test.dt[, strat_out := 0]
  test.dt[ip == min_ip, strat_out := 1]
  test.dt[, gain_out := strat_out * gain]
  test.dt[, min_ip := min(ip), list(date, hometeam, awayteam)]
  # bet on all home
  test.dt[, strat_home := 0]
  test.dt[ftr == "H", strat_home := 1]
  test.dt[, gain_home := strat_home * gain]
  # bet on all draw
  test.dt[, strat_draw := 0]
  test.dt[ftr == "D", strat_draw := 1]
  test.dt[, gain_draw := strat_draw * gain]
  # bet on all home
  test.dt[, strat_away := 0]
  test.dt[ftr == "A", strat_away := 1]
  test.dt[, gain_away := strat_away * gain]
  # top n % predictions
  setkey(test.dt, gbmp)
  test.dt[, rn := seq(test.dt[, .N])]
  test.dt[, pct_grp_10 := cut(test.dt[, rn], breaks=quantile(test.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  test.dt[, pct_grp_5 := cut(test.dt[, rn], breaks=quantile(test.dt[, rn], probs=seq(0, 1, by=0.05)), include.lowest=TRUE, labels=1:20)]
  test.dt[, pct_grp_1 := cut(test.dt[, rn], breaks=quantile(test.dt[, rn], probs=seq(0, 1, by=0.01)), include.lowest=TRUE, labels=1:100)]
  test.dt[, strat_top_pct_10 := 0]
  test.dt[pct_grp_10 == 10, strat_top_pct_10 := 1]
  test.dt[, gain_top_pct_10 := strat_top_pct_10 * gain]
  test.dt[, strat_top_pct_5 := 0]
  test.dt[pct_grp_5 == 20, strat_top_pct_5 := 1]
  test.dt[, gain_top_pct_5 := strat_top_pct_5 * gain]
  test.dt[, strat_top_pct_1 := 0]
  test.dt[pct_grp_1 == 100, strat_top_pct_1 := 1]
  test.dt[, gain_top_pct_1 := strat_top_pct_1 * gain]
  # weighted stake
  test.dt[, strat_all_wtd := 0]
  test.dt[strat_all > 0, strat_all_wtd :=
    gbmp *
    sum(test.dt[strat_all > 0, strat_all]) /
    sum(test.dt[strat_all > 0, gbmp])
  ]
  test.dt[, gain_all_wtd  := strat_all_wtd * gain]
  test.dt[, strat_fav_wtd := 0]
  test.dt[strat_fav > 0, strat_fav_wtd :=
    gbmp *
    sum(test.dt[strat_fav > 0, strat_fav]) /
    sum(test.dt[strat_fav > 0, gbmp])
  ]
  test.dt[, gain_fav_wtd  := strat_fav_wtd * gain]
  test.dt[, strat_out_wtd := 0]
  test.dt[strat_out > 0, strat_out_wtd :=
    gbmp *
    sum(test.dt[strat_out > 0, strat_out]) /
    sum(test.dt[strat_out > 0, gbmp])
  ]
  test.dt[, gain_out_wtd  := strat_out_wtd * gain]
  test.dt[, strat_home_wtd := 0]
  test.dt[strat_home > 0, strat_home_wtd :=
    gbmp *
    sum(test.dt[strat_home > 0, strat_home]) /
    sum(test.dt[strat_home > 0, gbmp])
  ]
  test.dt[, gain_home_wtd  := strat_home_wtd * gain]
  test.dt[, strat_draw_wtd := 0]
  test.dt[strat_draw > 0, strat_draw_wtd :=
    gbmp *
    sum(test.dt[strat_draw > 0, strat_draw]) /
    sum(test.dt[strat_draw > 0, gbmp])
  ]
  test.dt[, gain_draw_wtd  := strat_draw_wtd * gain]
  test.dt[, strat_away_wtd := 0]
  test.dt[strat_away > 0, strat_away_wtd :=
    gbmp *
    sum(test.dt[strat_away > 0, strat_away]) /
    sum(test.dt[strat_away > 0, gbmp])
  ]
  test.dt[, gain_away_wtd  := strat_away_wtd * gain]
  test.dt[, strat_top_pct_10_wtd := 0]
  test.dt[strat_top_pct_10 > 0, strat_top_pct_10_wtd :=
    gbmp *
    sum(test.dt[strat_top_pct_10 > 0, strat_top_pct_10]) /
    sum(test.dt[strat_top_pct_10 > 0, gbmp])
  ]
  test.dt[, gain_top_pct_10_wtd  := strat_top_pct_10_wtd * gain]
  test.dt[, strat_top_pct_5_wtd := 0]
  test.dt[strat_top_pct_5 > 0, strat_top_pct_5_wtd :=
    gbmp *
    sum(test.dt[strat_top_pct_5 > 0, strat_top_pct_5]) /
    sum(test.dt[strat_top_pct_5 > 0, gbmp])
  ]
  test.dt[, gain_top_pct_5_wtd  := strat_top_pct_5_wtd * gain]
  test.dt[, strat_top_pct_1_wtd := 0]
  test.dt[strat_top_pct_1 > 0, strat_top_pct_1_wtd :=
    gbmp *
    sum(test.dt[strat_top_pct_1 > 0, strat_top_pct_1]) /
    sum(test.dt[strat_top_pct_1 > 0, gbmp])
  ]
  test.dt[, gain_top_pct_1_wtd  := strat_top_pct_1_wtd * gain]
  cat0n("strategy,stake,gain")
  cat0n("all_results,", test.dt[, .N], ",", sum(test.dt[, gain]))
  cat0n("all_fav,", sum(test.dt[, strat_fav]), ",", sum(test.dt[, gain_fav]))
  cat0n("all_out,", sum(test.dt[, strat_out]), ",", sum(test.dt[, gain_out]))
  cat0n("all_home,", sum(test.dt[, strat_home]), ",", sum(test.dt[, gain_home]))
  cat0n("all_draw,", sum(test.dt[, strat_draw]), ",", sum(test.dt[, gain_draw]))
  cat0n("all_away,", sum(test.dt[, strat_away]), ",", sum(test.dt[, gain_away]))
  cat0n("top_pct_10,", sum(test.dt[, strat_top_pct_10]), ",", sum(test.dt[, gain_top_pct_10]))
  cat0n("top_pct_5,", sum(test.dt[, strat_top_pct_5]), ",", sum(test.dt[, gain_top_pct_5]))
  cat0n("top_pct_1,", sum(test.dt[, strat_top_pct_1]), ",", sum(test.dt[, gain_top_pct_1]))
  cat0n("all_results_wtd,", sum(test.dt[, strat_all_wtd]), ",", sum(test.dt[, gain_all_wtd]))
  cat0n("all_fav_wtd,", sum(test.dt[, strat_fav_wtd]), ",", sum(test.dt[, gain_fav_wtd]))
  cat0n("all_out_wtd,", sum(test.dt[, strat_out_wtd]), ",", sum(test.dt[, gain_out_wtd]))
  cat0n("all_home_wtd,", sum(test.dt[, strat_home_wtd]), ",", sum(test.dt[, gain_home_wtd]))
  cat0n("all_draw_wtd,", sum(test.dt[, strat_draw_wtd]), ",", sum(test.dt[, gain_draw_wtd]))
  cat0n("all_away_wtd,", sum(test.dt[, strat_away_wtd]), ",", sum(test.dt[, gain_away_wtd]))
  cat0n("top_pct_10_wtd,", sum(test.dt[, strat_top_pct_10_wtd]), ",", sum(test.dt[, gain_top_pct_10_wtd]))
  cat0n("top_pct_5_wtd,", sum(test.dt[, strat_top_pct_5_wtd]), ",", sum(test.dt[, gain_top_pct_5_wtd]))
  cat0n("top_pct_1_wtd,", sum(test.dt[, strat_top_pct_1_wtd]), ",", sum(test.dt[, gain_top_pct_1_wtd]))
  # print(summary(test.dt[, list(
  #   strat_all_wtd,
  #   strat_fav_wtd,
  #   strat_out_wtd,
  #   strat_home_wtd,
  #   strat_draw_wtd,
  #   strat_away_wtd,
  #   strat_top_pct_10_wtd,
  #   strat_top_pct_5_wtd,
  #   strat_top_pct_1_wtd
  # )]))
  # print(test.dt[, .N, strat_top_pct_10_wtd])
  # print(test.dt[, .N, strat_top_pct_5_wtd])
  # print(test.dt[, .N, strat_top_pct_1_wtd])
  # print(test.dt[strat_top_pct_1 > 0, list(date, hometeam, awayteam, ftr, act, actr, strat_top_pct_1_wtd, gain, gain_top_pct_1_wtd)])
  
  # plots
  plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, uvar, logfile)
  
  sink()
}

build.a.model("2020-08-01")
build.a.model("2020-08-01", weights=TRUE)
build.a.model("2020-09-01")
build.a.model("2020-09-01", weights=TRUE)
build.a.model("2020-10-01")
build.a.model("2020-10-01", weights=TRUE)
build.a.model("2020-11-01")
build.a.model("2020-11-01", weights=TRUE)
build.a.model("2020-12-01")
build.a.model("2020-12-01", weights=TRUE)

