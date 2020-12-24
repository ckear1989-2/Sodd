
#' Set options for data and modeling
#'
#' @param data.dir path to store modeling data. Defaults to "~/data/"
#' @param output.dir path to store model output. Defaults to "logs/"
#' @param force.upcoming Get upcoming matches from already played matches if
#' none found. Defaults to FALSE
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
set_sodd_options <- function(
  data.dir="~/data/",
  output.dir="logs/",
  force.upcoming=FALSE
  ) {
  options(list(
    sodd.data.dir=data.dir,
    sodd.output.dir=output.dir,
    sodd.force.upcoming=force.upcoming
  ))
  invisible()
}
