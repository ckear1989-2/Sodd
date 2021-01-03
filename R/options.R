
#' Set options for data and modeling
#'
#' @param data.dir path to store modeling data. Defaults to "~/data/"
#' @param output.dir path to store model output. Defaults to "logs/"
#' @param force.upcoming Get upcoming matches from already played matches if
#' none found. Defaults to FALSE
#' @param model.params Named list of model hyperparameters. Defaults to list(
#' train.fraction=0.7, n.trees=500, shrinkage=0.1, interaction.depth=2,
#' cv.folds=3)
#' @param verbosity Level at which to cat output 0=no output, 1=minimal,
#' 2=all. Defaults to 0
#' @family data_prep
#' @examples
#' \donttest{
#' set.sodd.options(
#'   data.dir="/home/sodd.data/",
#'   output.dir="/home/sodd.output/",
#'   force.upcoming=TRUE
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
    cv.folds=3
  ),
  verbosity=0
  ) {
  options(list(
    sodd.data.dir=data.dir,
    sodd.output.dir=output.dir,
    sodd.force.upcoming=force.upcoming,
    sodd.verbosity=verbosity
  ))
  invisible()
}

get.sodd.data.dir <- function() {
  d <- getOption("sodd.data.dir", "~/data/")
  d <- gsub("~", Sys.getenv("HOME"), d)[[1]]
  d
}

get.sodd.output.dir <- function() {
  d <- getOption("sodd.output.dir", "logs/")
  d <- gsub("~", Sys.getenv("HOME"), d)[[1]]
  d
}

get.sodd.force.upcoming <- function() {
  getOption("sodd.force.upcoming", FALSE)
}

get.sodd.verbosity <- function() {
  getOption("sodd.verbosity", 0)
}

