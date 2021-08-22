globalVariables(c(
  # hide from R CMD check
  # data.table syntax
  ":=",
  ".N",
  "."
))

base_dload_path <- "https://www.football-data.co.uk/"
historic_subdir <- "mmz4281"
upcoming_fixtures <- "fixtures.csv"
all.leagues <- c("E0", "E1", "E2", "E3", "SP1", "SP2", "D1", "D2", "I1", "I2", "F1", "F2")
all.years <- c(
  "2122",
  "2021",
  "1920",
  "1819",
  "1718",
  "1617",
  "1516",
  "1415",
  "1314",
  "1213",
  "1112",
  "1011"
)

strats <- c(
  "all",
  "fav",
  "out",
  "home",
  "draw",
  "away",
  "top_pct_10",
  "top_pct_5",
  "top_pct_1",
  "top_per_match"
)
strats <- c(strats, paste0(strats, "_wtd"))

