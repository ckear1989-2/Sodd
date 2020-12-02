# dir.create('~/data/')
print(getwd())
source("data_prep/constants.R")
base_dload_path <- 'https://www.football-data.co.uk/mmz4281/'

dload_league_season <- function(l, s) {
  if(!file.exists(paste0('~/data/', s))) dir.create(paste0('~/data/', s))
  utils::download.file(file.path(base_dload_path, s, paste0(l, '.csv')), file.path('~/data/', s, paste0(l, '.csv')))
}
dload_10_years <- function(l) {
  dload_league_season(l, '1920')
  dload_league_season(l, '1819')
  dload_league_season(l, '1718')
  dload_league_season(l, '1617')
  dload_league_season(l, '1516')
  dload_league_season(l, '1415')
  dload_league_season(l, '1314')
  dload_league_season(l, '1213')
  dload_league_season(l, '1112')
  dload_league_season(l, '1011')
}
dload_current_year <- function(leagues) {
  for (l in leagues) dload_league_season(l, '2021')
}

dload_current_year(leagues)
# dload_10_years('E0')
# dload_10_years('E1')
# dload_10_years('E2')
# dload_10_years('SP1')
# dload_10_years('SP2')
dload_10_years('D1')
dload_10_years('D2')

