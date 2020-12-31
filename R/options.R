
#' Set options for data and modeling
#'
#' @param data.dir path to store modeling data. Defaults to "~/data/"
#' @param output.dir path to store model output. Defaults to "logs/"
#' @param force.upcoming Get upcoming matches from already played matches if
#' none found. Defaults to FALSE
#' @param verbosity Not yet implemented. Defaults to FALSE
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
  verbosity=FALSE
  ) {
  options(list(
    sodd.data.dir=data.dir,
    sodd.output.dir=output.dir,
    sodd.force.upcoming=force.upcoming,
    sodd.verbosity=FALSE
  ))
  invisible()
}

get.sodd.data.dir <- function() {
  getOption("sodd.data.dir", "~/data/")
}

get.sodd.output.dir <- function() {
  getOption("sodd.output.dir", "logs/")
}

get.sodd.force.upcoming <- function() {
  getOption("sodd.force.upcoming", FALSE)
}

get.sodd.verbosity <- function() {
  getOption("sodd.verbosity", 0)
}

