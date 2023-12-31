library("sodd")

random_by_char <- function(cvar) {
  # subsampling helper function
  suppressWarnings(TeachingDemos::char2seed(cvar))
  runif(1)
}

create.a.dt <- quote({
  source("R/constants.R")
  source("R/options.R")
  source("R/utils.R")
  source("R/standardise.R")
  source("R/strategy.R")
  source("R/plot.R")
  # create data for testing a.dt
  all.years <- c("2021", "2122", "2223", "2324")
  set.sodd.options(data.dir="~/sodd.data/test.data/", verbosity=0)
  create.sodd.modeling.data(c("E0", "E1", "E2"), 4)
  a.dt <- readRDS(file.path(get.sodd.data.dir(), "a.dt.rds"))
  # force upcoming matches
  upcoming.dt <- a.dt[date == max(a.dt[, date]), ]
  a.dt <- a.dt[date < max(a.dt[, date]), ]
  upcoming.dt[, ftr := NULL]
  upcoming.dt[, ftr := "NA"]
  a.dt <- rbind(a.dt, upcoming.dt, fill=TRUE)
  data.dir <- get.sodd.data.dir()
  saveRDS(a.dt, file.path(data.dir, "a.dt.rds"))
  adate <- "2023-09-01"
})

create.test.dataset.spread <- quote({    
  # 10% sample of existing dataset
  yvar <- "spread"
  # eval(read.model.data)
  test.a.dt <- copy(a.dt)
  test.a.dt[, rvar := random_by_char(match_id), match_id]
  test.a.dt <- test.a.dt[rvar <= 0.4, ]
  test.a.dt[, rvar := NULL]
  data.dir <- get.sodd.data.dir()
  saveRDS(test.a.dt, file.path(data.dir, "test.a.dt.spread.rds"))
})

read.test.model.data.spread <- quote({
  data.dir <- get.sodd.data.dir()
  a.dt <- readRDS(file.path(data.dir, "test.a.dt.spread.rds"))
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

create.test.model.spread <- quote({
  yvar <- "spread"
  model.dt.list <- read.model.data(adate, yvar, FALSE, FALSE)
  a.dt <- model.dt.list[[1]]
  train.dt <- model.dt.list[[2]]
  test.dt <- model.dt.list[[3]]
  upcoming.dt <- model.dt.list[[4]]
  family <- model.dt.list[[5]]
  offset_var <- model.dt.list[[6]]
  output.dir <- model.dt.list[[7]]
  modelfile <- model.dt.list[[8]]
  pdffile <- model.dt.list[[9]]
  eval(read.test.model.data.spread)
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  # model params
  train.fraction <- 0.7
  n.trees <- 50
  shrinkage <- 0.01
  interaction.depth <- 2
  cv.folds <- 3
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:4)
  )
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  formula <- as.formula(paste("y", paste(xvar, collapse="+"), sep="~offset(offset)+"))
  eval(model.params)
  eval(build.model)
  eval(model.summary)
  eval(score.model)
  eval(rebalance.model)
  eval(calc.deviances)
  eval(act.pred.summary)
  eval(positive.model.predictions)
  run.strategy(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  model$adate <- adate
  model$train.a.dt <- train.a.dt
  model$train.b.dt <- train.b.dt
  model$train.dt <- train.dt
  model$test.dt <- test.dt
  model$upcoming.dt <- upcoming.dt
  model$uvar <- uvar
  model$yvar <- yvar
  model$logfile <- logfile
  model$pdffile <- pdffile
  model$modelfile <- modelfile
  model.output.dir <- paste0(get.sodd.output.dir(), "models/")
  data.dir <- get.sodd.data.dir()
  saveRDS(model, file.path(data.dir, "test.model.spread.rds"))
})

read.test.model.spread <-quote({
  data.dir <- get.sodd.data.dir()
  model <- readRDS(file.path(data.dir, "test.model.spread.rds"))
})

create.test.model.doc.spread <- quote({
  eval(read.test.model.spread)
  pdffile <- model$pdffile
  train.a.dt <- model$train.a.dt
  train.b.dt <- model$train.b.dt
  train.dt <- model$train.dt
  test.dt <- model$test.dt
  upcoming.dt <- model$upcoming.dt
  yvar <- "spread"
  xvar <- c("ip", "div", "ftr", paste0("hpp", 1:4))
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile)
})

create.test.dataset.act <- quote({
  # 10% sample of existing dataset
  yvar <- "act"
  eval(read.model.data)
  test.a.dt <- copy(a.dt)
  test.a.dt[, rvar := random_by_char(match_id), match_id]
  test.a.dt <- test.a.dt[rvar <= 0.4, ]
  test.a.dt[, rvar := NULL]
  data.dir <- get.sodd.data.dir()
  saveRDS(test.a.dt, file.path(data.dir, "test.a.dt.act.rds"))
})

read.test.model.data.act <- quote({
  data.dir <- get.sodd.data.dir()
  a.dt <- readRDS(file.path(data.dir, "test.a.dt.act.rds"))
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

create.test.model.act <- quote({
  yvar <- "act"
  model.dt.list <- read.model.data(adate, yvar, FALSE, FALSE)
  a.dt <- model.dt.list[[1]]
  train.dt <- model.dt.list[[2]]
  test.dt <- model.dt.list[[3]]
  upcoming.dt <- model.dt.list[[4]]
  family <- model.dt.list[[5]]
  offset_var <- model.dt.list[[6]]
  output.dir <- model.dt.list[[7]]
  modelfile <- model.dt.list[[8]]
  pdffile <- model.dt.list[[9]]
  eval(read.test.model.data.act)
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  # model params
  train.fraction <- 0.7
  n.trees <- 50
  shrinkage <- 0.01
  interaction.depth <- 2
  cv.folds <- 3
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:4)
  )
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  formula <- as.formula(paste("y", paste(xvar, collapse="+"), sep="~offset(offset)+"))
  eval(model.params)
  eval(build.model)
  eval(model.summary)
  eval(score.model)
  eval(rebalance.model)
  eval(calc.deviances)
  eval(act.pred.summary)
  eval(positive.model.predictions)
  model$adate <- adate
  model$train.a.dt <- train.a.dt
  model$train.b.dt <- train.b.dt
  model$train.dt <- train.dt
  model$test.dt <- test.dt
  model$upcoming.dt <- upcoming.dt
  model$uvar <- uvar
  model$yvar <- yvar
  model$logfile <- logfile
  model$pdffile <- pdffile
  model$modelfile <- modelfile
  data.dir <- get.sodd.data.dir()
  saveRDS(model, file.path(data.dir, "test.model.act.rds"))
})

read.test.model.act <-quote({
  data.dir <- get.sodd.data.dir()
  model <- readRDS(file.path(data.dir, "test.model.act.rds"))
})

create.test.model.doc.act <- quote({
  yvar <- "act"
  model.dt.list <- read.model.data(adate, yvar, FALSE, FALSE)
  a.dt <- model.dt.list[[1]]
  train.dt <- model.dt.list[[2]]
  test.dt <- model.dt.list[[3]]
  upcoming.dt <- model.dt.list[[4]]
  family <- model.dt.list[[5]]
  offset_var <- model.dt.list[[6]]
  output.dir <- model.dt.list[[7]]
  modelfile <- model.dt.list[[8]]
  pdffile <- model.dt.list[[9]]
  eval(read.test.model.data.act)
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  # model params
  train.fraction <- 0.7
  n.trees <- 50
  shrinkage <- 0.01
  interaction.depth <- 2
  cv.folds <- 3
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:4)
  )
  uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
  formula <- as.formula(paste("y", paste(xvar, collapse="+"), sep="~offset(offset)+"))
  eval(read.test.model.act)
  eval(model.params)
  eval(model.summary)
  eval(score.model)
  eval(rebalance.model)
  eval(calc.deviances)
  eval(act.pred.summary)
  eval(positive.model.predictions)
  run.strategy(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile)
  sink()
})

# eval(create.a.dt)
# eval(create.test.dataset.spread)
# eval(create.test.model.spread)
# eval(create.test.model.doc.spread)
# eval(create.test.dataset.act)
# eval(create.test.model.act)
# eval(create.test.model.doc.act)

