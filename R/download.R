
#' Download a csv file for a single league season
#'
#' @param l League names in format <country><division> (e.g. English Premier League = "E0")
#' @param s Season in format <Y1><Y2> (e.g. 2019/2020 = "1920")
#' @param quiet Download quietly. Defaults to FALSE
#' @param force Overwrite if file exists. Defaults to TRUE
#' @return NULL
#' @family download
#' @examples
#' \donttest{
#' dload.league.season("E0", "1920")
#' }
#' @export 
dload.league.season <- function(l, s, quiet=FALSE, force=TRUE) {
  data.dir <- get.sodd.data.dir()
  if(!file.exists(data.dir)) dir.create(data.dir)
  if(!file.exists(paste0(data.dir, s))) dir.create(paste0(data.dir, s))
  output.path <- file.path(data.dir, s, paste0(l, ".csv"))
  if((!file.exists(output.path)) | isTRUE(force)) {
    utils::download.file(file.path(base_dload_path, historic_subdir, s, paste0(l, ".csv")), output.path, quiet=quiet)
  }
  invisible()
}

#' Download past x years of data
#'
#' @param l League(s) to download data for. Defaults to all.leagues
#' @param x Number of years to download
#' @param ... Args to pass to dload.league.season
#' @family download
#' @examples
#' \donttest{
#' dload.x.years("E0", 10)
#' dload.x.years(c("E0", "S1"), 10, quiet=TRUE)
#' }
#' @export 
dload.x.years <- function(l, x=10, ...) {
  for (li in l) {
    for (i in seq(x)) dload.league.season(li, all.years[[i]], ...)
  }
}

#' Download current year fixtures
#'
#' @param l League(s) to download data for. Defaults to all.leagues
#' @param ... Args to pass to dload.league.season
#' @family download
#' @examples
#' \donttest{
#' dload.current.year()
#' dload.current.year(quiet=TRUE)
#' }
#' @export 
dload.current.year <- function(l=all.leagues, ...) {
  for (li in l) dload.league.season(li, all.years[[1]], ...)
}

#' Download upcoming fixtures
#'
#' @param quiet Download quietly. Defaults to FALSE
#' @family download
#' @examples
#' \donttest{
#' dload.upcoming()
#' dload.upcoming(TRUE)
#' }
#' @export 
dload.upcoming <- function(quiet=FALSE) {
  data.dir <- get.sodd.data.dir()
  utils::download.file(file.path(base_dload_path, upcoming_fixtures), file.path(data.dir, upcoming_fixtures), quiet=quiet)
}

