
#' Download a csv file for a single league season
#'
#' @param l League names in format <country><division> (e.g. English Premier League = "E0")
#' @param s Season in format <Y1?<Y2> e.g. 2019/2020 = "1920"
#' @param quiet Download quietly. Defaults to FALSE
#' @return NULL
#' @examples
#' dload_league_season("E0", "1920")
#' @export 
dload_league_season <- function(l, s, quiet=FALSE) {
  if(!file.exists("~/data/")) dir.create("~/data/")
  if(!file.exists(paste0("~/data/", s))) dir.create(paste0("~/data/", s))
  utils::download.file(file.path(base_dload_path, historic_subdir, s, paste0(l, ".csv")), file.path("~/data/", s, paste0(l, ".csv")), quiet=quiet)
  invisible()
}

dload_10_years <- function(l) {
  dload_league_season(l, "1920")
  dload_league_season(l, "1819")
  dload_league_season(l, "1718")
  dload_league_season(l, "1617")
  dload_league_season(l, "1516")
  dload_league_season(l, "1415")
  dload_league_season(l, "1314")
  dload_league_season(l, "1213")
  dload_league_season(l, "1112")
  dload_league_season(l, "1011")
}

dload_current_year <- function(...) {
  for (l in leagues) dload_league_season(l, years[[1]], ...)
}

#' Download upcoming fixtures
#'
#' @param quiet Download quietly. Defaults to FALSE
#' @examples
#' dload_upcoming()
#' dload_upcoming(TRUE)
#' @export 
dload_upcoming <- function(quiet=FALSE) {
  utils::download.file(file.path(base_dload_path, upcoming_fixtures), file.path("~/data/", upcoming_fixtures), quiet=quiet)
}

