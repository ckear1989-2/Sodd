source("data_prep/constants.R")

dload_league_season <- function(l, s, quiet=FALSE) {
  if(!file.exists("~/data/")) dir.create("~/data/")
  if(!file.exists(paste0("~/data/", s))) dir.create(paste0("~/data/", s))
  utils::download.file(file.path(base_dload_path, historic_subdir, s, paste0(l, ".csv")), file.path("~/data/", s, paste0(l, ".csv")), quiet=quiet)
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
dload_upcoming <- function() {
  utils::download.file(file.path(base_dload_path, upcoming_fixtures), file.path("~/data/", upcoming_fixtures))
}

if(interactive()) {
  if(!file.exists("logs/")) dir.create("logs")
  sink("logs/download.log")
  # lapply(leagues, dload_10_years)
  dload_current_year(leagues)
  dload_upcoming()
  sink()
}

