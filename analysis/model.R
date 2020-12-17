library("data.table")

source("analysis/plot.R")
source("analysis/strategy.R")
source("utils/utils.R")

build.a.model <- function(adate, yvar, weights=FALSE) {
  # print to log file
  set.seed(123)
  eval(read.model.data)
  # model params
  train.fraction <- 0.7
  n.trees <- 500
  shrinkage <- 0.01
  interaction.depth <- 3
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
  eval(model.params)
  eval(build.model)
  eval(model.summary)
  eval(score.model)
  eval(rebalance.model)
  eval(calc.deviances)
  eval(act.pred.summary)
  eval(positive.model.predictions)
  run.strategy(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, logfile)
  sink()
}


args = commandArgs()
this_file <- "model.R"
file_run <- ""
if(length(args) > 3) file_run <- strsplit(args[[4]], "/")[[1]][[2]]
if(file_run == this_file) {
  dates <- c(paste0("2020-", c("08", "09", "10", "11", "12"), "-01"))
  yvar <- c("spread", "act")
  weights <- c(TRUE, FALSE)
  all.args <- data.table(expand.grid(dates, yvar, weights))
  all.args[, Var1 := as.character(Var1)]
  all.args[, Var2 := as.character(Var2)]
  all.args[, Var3 := as.logical(Var3)]
  print(all.args)
  # build.a.model("2020-08-01", "spread")
  # lapply(1:20, function(x) {build.a.model(all.args[[1]][[x]], all.args[[2]][[x]], all.args[[3]][[x]])})
  # build.a.model("2020-08-01", "spread", weights=TRUE)
  for (x in 1:20) {
    print(all.args[x, Var1])
    print(all.args[x, Var2])
    print(all.args[x, Var3])
    build.a.model(all.args[x, Var1], all.args[x, Var2], weights=all.args[x, Var3])
  }
}
