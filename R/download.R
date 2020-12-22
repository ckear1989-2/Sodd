
#' Download a csv file for a single league season
#'
#' @param l League names in format <country><division> (e.g. English Premier League = "E0")
#' @param s Season in format <Y1><Y2> (e.g. 2019/2020 = "1920")
#' @param quiet Download quietly. Defaults to FALSE
#' @param force Overwrite if file exists. Defaults to TRUE
#' @return NULL
#' @examples
#' dload_league_season("E0", "1920")
#' @export 
dload_league_season <- function(l, s, quiet=FALSE, force=TRUE) {
  if(!file.exists("~/data/")) dir.create("~/data/")
  if(!file.exists(paste0("~/data/", s))) dir.create(paste0("~/data/", s))
  output.path <- file.path("~/data/", s, paste0(l, ".csv"))
  if((!file.exists(output.path)) | isTRUE(force)) {
    utils::download.file(file.path(base_dload_path, historic_subdir, s, paste0(l, ".csv")), output.path, quiet=quiet)
  }
  invisible()
}

#' Download past x years of data
#'
#' @param l League(s) to download data for
#' @param x Number of years to download
#' @param ... Args to pass to dload_league_season
#' @examples
#' dload_x_years("E0", 10)
#' dload_x_years(c("E0", "S1"), 10, quiet=TRUE)
#' @export 
dload_x_years <- function(l, x=10, ...) {
  for (li in l) {
    for (i in seq(x)) dload_league_season(li, all.years[[i]], ...)
  }
}

#' Download current year fixtures
#'
#' @param ... Args to pass to dload_league_season
#' @examples
#' dload_current_year()
#' dload_current_year(quiet=TRUE)
#' @export 
dload_current_year <- function(leagues=all.leagues, ...) {
  for (l in leagues) dload_league_season(l, all.years[[1]], ...)
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

