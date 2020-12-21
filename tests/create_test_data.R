
library("data.table")

source("utils/utils.R")

create_test_dataset <- quote({
  # 10% sample of existing dataset
  adate <- "2020-12-11"
  yvar <- "spread"
  eval(read.model.data)
  random_by_char <- function(cvar) {
    suppressWarnings(TeachingDemos::char2seed(cvar))
    runif(1)
  }
  test.a.dt <- copy(a.dt)
  test.a.dt[, rvar := random_by_char(match_id), match_id]
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
  test.a.dt <- test.a.dt[rvar <= 0.1, ]
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
  test.a.dt[, rvar := NULL]
  saveRDS(test.a.dt, "~/data/R/rds/test.a.dt.rds")
  test.a.dt <- readRDS("~/data/R/rds/test.a.dt.rds")
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
})

read.test.model.data <- quote({
  a.dt <- readRDS("~/data/R/rds/test.a.dt.rds")
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

create_test_model <- quote({
  adate <- "2020-12-11"
  yvar <- "spread"
  eval(read.test.model.data)
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
  eval(build.model)
  saveRDS(model, "~/data/R/rds/test.model.rds")
  model <- readRDS("~/data/R/rds/test.model.rds")
  print(attributes(model))

})

args = commandArgs()
this_file <- "create_test_data.R"
file_run <- ""
if(length(args) > 3) file_run <- strsplit(args[[4]], "/")[[1]][[2]]
if(file_run == this_file) {
  # eval(create_test_dataset)
  eval(create_test_model)
}
