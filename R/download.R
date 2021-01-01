
#' Download a csv file for a single league season
#'
#' @param l League names in format <country><division> (e.g. English Premier League = "E0")
#' @param s Season in format <Y1><Y2> (e.g. 2019/2020 = "1920")
#' @param quiet Download quietly. Defaults to FALSE
#' @param force Overwrite if file exists. Defaults to TRUE
#' @param check check for change from existing data pull. Defaults to FALSE
#' @return check.status TRUE if file has changed since previous pull. False if not
#' @family download
#' @examples
#' \donttest{
#' dload.league.season("E0", "1920")
#' }
#' @export 
dload.league.season <- function(l, s, quiet=FALSE, force=TRUE, check=FALSE) {
  data.dir <- get.sodd.data.dir()
  if(!file.exists(data.dir)) dir.create(data.dir)
  if(!file.exists(paste0(data.dir, s))) dir.create(paste0(data.dir, s))
  output.path <- file.path(data.dir, s, paste0(l, ".csv"))
  tmp.output.path <- tempfile()
  check.status <- FALSE
  if((!file.exists(output.path)) | isTRUE(force)) {
    utils::download.file(
      file.path(base_dload_path, historic_subdir, s, paste0(l, ".csv")),
      tmp.output.path,
      quiet=quiet
    )
    if(isTRUE(check)) {
      check.status <- check.file.diff(tmp.output.path, output.path)
      if(isTRUE(check.status)) file.copy(tmp.output.path, output.path)
    } else {
      file.copy(tmp.output.path, output.path)
    }
  }
  invisible(check.status)
}

check.file.diff <- function(fa, fb) {
  if((!file.exists(fa)) & file.exists(fb)) return(TRUE)
  if((!file.exists(fb)) & file.exists(fa)) return(TRUE)
  r <- tools::md5sum(fa) != tools::md5sum(fb)
  names(r) <- NULL
  r
}

#' Download past x years of data
#'
#' @param l League(s) to download data for. Defaults to all.leagues
#' @param x Number of years to download
#' @param ... Args to pass to dload.league.season
#' @return check.status TRUE if file has changed since previous pull. False if not
#' @family download
#' @examples
#' \donttest{
#' dload.x.years("E0", 10)
#' dload.x.years(c("E0", "S1"), 10, quiet=TRUE)
#' }
#' @export 
dload.x.years <- function(l, x=10, ...) {
  check.status <- c()
  for (li in l) {
    check.status <- c(check.status, sapply(seq(x), function(i) dload.league.season(li, all.years[[i]], ...)))
  }
  names(check.status) <- NULL
  invisible(check.status)
}

#' Download current year fixtures
#'
#' @param l League(s) to download data for. Defaults to all.leagues
#' @param ... Args to pass to dload.league.season
#' @return check.status TRUE if file has changed since previous pull. False if not
#' @family download
#' @examples
#' \donttest{
#' dload.current.year()
#' dload.current.year(quiet=TRUE)
#' }
#' @export 
dload.current.year <- function(l=all.leagues, ...) {
  check.status <- sapply(l, function(li) dload.league.season(li, all.years[[1]], ...))
  names(check.status) <- NULL
  invisible(check.status)
}

#' Download upcoming fixtures
#'
#' @param quiet Download quietly. Defaults to FALSE
#' @param check check for change from existing data pull. Defaults to FALSE
#' @family download
#' @examples
#' \donttest{
#' dload.upcoming()
#' dload.upcoming(TRUE)
#' }
#' @export 
dload.upcoming <- function(quiet=FALSE, check=TRUE) {
  data.dir <- get.sodd.data.dir()
  output.path <- file.path(data.dir, upcoming_fixtures)
  tmp.output.path <- tempfile()
  check.status <- FALSE
  utils::download.file(
    file.path(base_dload_path, upcoming_fixtures),
    tmp.output.path,
    quiet=quiet
  )
  if(isTRUE(check)) {
    check.status <- check.file.diff(tmp.output.path, output.path)
    if(isTRUE(check.status)) file.copy(tmp.output.path, output.path)
  } else {
    file.copy(tmp.output.path, output.path)
  }
  invisible(check.status)
}

