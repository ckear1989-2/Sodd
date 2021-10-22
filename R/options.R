
#' Set options for data and modeling
#'
#' @param data.dir path to store modeling data. Defaults to "~/data/"
#' @param output.dir path to store model output. Defaults to "logs/"
#' @param force.upcoming Get upcoming matches from already played matches if
#' none found. Defaults to FALSE
#' @param model.params Named list of model hyperparameters. Defaults to list(
#' train.fraction=0.7, n.trees=500, shrinkage=0.1, interaction.depth=2,
#' cv.folds=3)
#' @param n.lag number of previous results to create predictor variables from. 
#' Defaults t0 5
#' @param verbosity Level at which to cat output 0=no output, 1=minimal,
#' 2=all. Defaults to 0
#' @family data_prep
#' @examples
#' \donttest{
#' set.sodd.options(
#'   data.dir="/home/sodd.data/",
#'   output.dir="/home/sodd.output/",
#'   force.upcoming=TRUE,
#'   model.params=list(
#'     train.fraction=0.9,
#'     n.trees=100
#'   )
#' )
#' }
#' @export
set.sodd.options <- function(
  data.dir="~/data/",
  output.dir="logs/",
  force.upcoming=FALSE,
  model.params=list(
    train.fraction=0.7,
    n.trees=500,
    shrinkage=0.1,
    interaction.depth=2,
    cv.folds=3,
    n.cores=1
  ),
  n.lag=5,
  verbosity=0
  ) {
  options(list(
    sodd.data.dir=data.dir,
    sodd.output.dir=output.dir,
    sodd.force.upcoming=force.upcoming,
    sodd.model.params=model.params,
    sodd.n.lag=n.lag,
    sodd.verbosity=verbosity
  ))
  invisible()
}

get.sodd.data.dir <- function() {
  d <- getOption("sodd.data.dir", "~/data/")
  d <- file.path(gsub("~", Sys.getenv("HOME"), d)[[1]])
  dir.create(d, showWarnings=FALSE)
  d
}

get.sodd.output.dir <- function() {
  d <- getOption("sodd.output.dir", "logs/")
  d <- file.path(gsub("~", Sys.getenv("HOME"), d)[[1]])
  dir.create(d, showWarnings=FALSE)
  d
}

get.sodd.force.upcoming <- function() {
  getOption("sodd.force.upcoming", FALSE)
}

get.sodd.model.params <- function() {
  options <- getOption("sodd.model.params", list(
    train.fraction=0.7,
    n.trees=500,
    shrinkage=0.1,
    interaction.depth=2,
    cv.folds=3,
    n.cores=1
  ))
  if(is.null(options$train.fraction)) options$train.fraction <- 0.7
  if(is.null(options$n.trees)) options$n.trees <- 500
  if(is.null(options$shrinkage)) options$shrinkage <- 0.1
  if(is.null(options$interaction.depth)) options$interaction.depth <- 2
  if(is.null(options$cv.folds)) options$cv.folds <- 3
  if(is.null(options$n.cores)) options$n.cores <- 1
  options
}

get.sodd.n.lag <- function() {
  getOption("sodd.n.lag", 5)
}

get.sodd.verbosity <- function() {
  getOption("sodd.verbosity", 0)
}

