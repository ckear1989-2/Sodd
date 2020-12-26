
#' Build gbm sodd model
#'
#' @param adate a date in string format "%Y-%m-%d".  Splits train.a and train.b data
#' @param yvar model response variable "act" or "spread"
#' @param weights include weigthing of observations. Defaults to FALSE
#' @param train.fraction gbm parameter. Proportion of training data for train.a. Defaults to 0.7
#' @param n.trees gbm parameter. Number of trees to build in gbm. Defaults to 500
#' @param shrinkage gbm parameter. Rate of change towards prediction. Defaults to 0.01
#' @param interaction.depth gbm parameter. Depth of each tree in model. Defaults to 2
#' @param cv.folds gbm parameter. Number of cross validation folds in training data. Defaults to 3
#' @param plot.it Create output plot. Defaults to FALSE
#' @param log.it Create output log and print to stdout. Defaults to FALSE
#' @return gbm model object
#' @family model
#' @examples
#' \donttest{
#' build.sodd.model("2020-01-01", "act")
#' build.sodd.model("2020-01-01", "spread", weights=TRUE)
#' build.sodd.model("2020-01-01", "spread", n.trees=50)
#' }
#' @export
#' @import data.table
build.sodd.model <- function(
  adate,
  yvar,
  weights=FALSE,
  train.fraction=0.7,
  n.trees=500,
  shrinkage=0.7,
  interaction.depth=2,
  cv.folds=3,
  plot.it=FALSE,
  log.it=FALSE
  ) {
  logfile <- ip <- a.dt <- train.a.dt <- train.b.dt <- test.dt <-
  upcoming.dt <- train.dt <- pdffile <- model <- a.date <- NULL
  set.seed(123)
  eval(read.model.data)
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  # model params
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:4),
    paste0("app", 1:4),
    paste0("hpgf", 1:4),
    paste0("apgf", 1:4),
    paste0("hpga", 1:4),
    paste0("apga", 1:4),
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
  formula <- stats::as.formula(paste("y", paste(xvar, collapse="+"), sep="~offset(offset)+"))
  eval(model.params)
  eval(build.model)
  eval(model.summary)
  eval(score.model)
  eval(rebalance.model)
  eval(calc.deviances)
  eval(act.pred.summary)
  eval(positive.model.predictions)
  run.strategy(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  sink()
  if(isTRUE(plot.it)) plot.model(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile)
  # dunno why attr(model, x) <- x doesn't work
  # attr(model, "adate") <- adate
  class(model) <- c("sodd", class(model))
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
  model
}

#' Create model documentation
#'
#' @param model sodd model object
#' @return character location of output file
#' @family output
#' @examples
#' \donttest{
#' model <- build.sodd.model("2020-12-22", "act")
#' document.sodd.model(model)
#' }
#' @export 
document.sodd.model <- function(model) {
  plot.model(
    model,
    model$adate,
    model$train.a.dt,
    model$train.b.dt,
    model$train.dt,
    model$test.dt,
    model$upcoming.dt,
    model$uvar,
    model$yvar,
    model$pdffile
  )
}

#' Get summary table for upcoming fixture strategy
#'
#' @param model sodd model object
#' @return grobTree object
#' @family output
#' @examples
#' \donttest{
#' model <- build.sodd.model("2020-12-22", "act")
#' upcoming.strategy.sodd.model(model)
#' }
#' @export 
upcoming.strategy.sodd.model <- function(model) {
  div <- NULL
  leagues <- unique(as.character(model$train.dt[, div]))
  recent.dt <- get.recent.dt(leagues)
  detailed.strat.gtable(model$upcoming.dt, recent.dt, "upcoming")
}

#' Build gbm sodd models for all responses and weights
#'
#' @param adate a date in string format "%Y-%m-%d".  Splits train.a and train.b data
#' @param ... arguments to pass to build.sodd.model
#' @return list of gbm model objects
#' @family model
#' @examples
#' \donttest{
#' build.all.sodd.models.one.date("2020-01-01")
#' build.all.sodd.models.one.date("2020-01-01", n.trees=50, interaction.depth=3)
#' }
#' @export 
build.all.sodd.models.one.date <- function(adate, ...) {
  list(
    build.sodd.model(adate, "spread", weights=FALSE, ...),
    build.sodd.model(adate, "spread", weights=TRUE, ...),
    build.sodd.model(adate, "act", weights=FALSE, ...),
    build.sodd.model(adate, "act", weights=TRUE, ...)
  )
}

