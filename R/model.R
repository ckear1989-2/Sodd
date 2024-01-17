
#' Build gbm sodd model
#'
#' @param adate a date in string format "%Y-%m-%d".  Splits train.a and train.b data
#' @param yvar model response variable "act", "spread" or "fthg"
#' @param weights include weigthing of observations. Defaults to FALSE
#' @param plot.it Create output plot. Defaults to FALSE
#' @param previous.model.as.offset  Use model from most recent date found as offset. Defaults to FALSE
#' @return gbm model object
#' @family model
#' @examples
#' \donttest{
#' build.sodd.model("2020-01-01", "act")
#' build.sodd.model("2020-01-01", "spread", weights=TRUE)
#' }
#' @export
#' @import data.table
build.sodd.model <- function(
  adate,
  yvar,
  weights=FALSE,
  plot.it=FALSE,
  previous.model.as.offset=FALSE
  ) {
  logfile <- ip <- train.a.dt <- train.b.dt <- test.dt <- output.dir <-
  upcoming.dt <- train.dt <- pdffile <- modelfile <- model <- a.date <- NULL
  set.seed(123)
  data.success <- FALSE
  model.dt.list <- read.model.data(adate, yvar, previous.model.as.offset, weights)
  a.dt <- model.dt.list[[1]]
  train.dt <- model.dt.list[[2]]
  test.dt <- model.dt.list[[3]]
  upcoming.dt <- model.dt.list[[4]]
  family <- model.dt.list[[5]]
  offset_var <- model.dt.list[[6]]
  output.dir <- model.dt.list[[7]]
  modelfile <- model.dt.list[[8]]
  pdffile <- model.dt.list[[9]]
  if("data.table" %in% class(a.dt)) data.success <- TRUE
  if(!data.success) {
    sink()
    return()
  }
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  # model params
  n.lag <- get.sodd.n.lag()
  xvar <- c(
    "ip",
    "div",
    "ftr",
    paste0("hpp", 1:n.lag),
    paste0("app", 1:n.lag),
    paste0("hpgf", 1:n.lag),
    paste0("apgf", 1:n.lag),
    paste0("hpga", 1:n.lag),
    paste0("apga", 1:n.lag),
    paste0("hpd", 1:n.lag),
    paste0("apd", 1:n.lag),
    paste0("hphp", 1:n.lag),
    paste0("apap", 1:n.lag),
    paste0("hphd", 1:n.lag),
    paste0("apad", 1:n.lag),
    paste0("happ", 1:n.lag),
    paste0("hpp_cum", 2:n.lag),
    paste0("app_cum", 2:n.lag),
    paste0("happ_cum", 2:n.lag)
  )
  if(yvar %in% c("fthg", "ftag")) {
    xvar <- xvar[!c("ip", "ftr") %in% xvar]
    xvar <- c(xvar, "h.ip", "d.ip", "a.ip")
  }
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
  if(isTRUE(plot.it)) plot.model(model)
  # dunno why attr(model, x) <- x doesn't work
  # attr(model, "adate") <- adate
  class(model) <- c("sodd", class(model))
  if(!file.exists(output.dir)) dir.create(output.dir)
  if(!file.exists(model.output.dir)) dir.create(model.output.dir)
  saveRDS(model, modelfile)
  report.memory(model)
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
  plot.model(model)
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
  if(is.null(model)) {
    warning("attempting strategy table with null model")
    return(NULL)
  }
  leagues <- unique(as.character(model$train.dt[, div]))
  recent.dt <- get.recent.dt(leagues)
  detailed.strat.gtable(model$upcoming.dt, recent.dt, "upcoming")
}

#' Build gbm sodd models for all responses and weights
#'
#' @param adate a date in string format "%Y-%m-%d".  Splits train.a and train.b data
#' @param ... arguments to pass to build.sodd.model
#' @return NULL
#' @family model
#' @examples
#' \donttest{
#' set.sodd.options(data.dir="~/sodd.data/")
#' create.sodd.modeling.data()
#' build.all.sodd.models.one.date("2020-01-01")
#' }
#' @export 
build.all.sodd.models.one.date <- function(adate, ...) {
  build.sodd.model(adate, "spread", weights=FALSE, ...)
  # build.sodd.model(adate, "spread", weights=TRUE, ...)
  build.sodd.model(adate, "act", weights=FALSE, ...)
  # build.sodd.model(adate, "act", weights=TRUE, ...)
  # build.sodd.model(adate, "fthg", weights=FALSE, ...)
  # build.sodd.model(adate, "ftag", weights=FALSE, ...)
  invisible(NULL)
}

