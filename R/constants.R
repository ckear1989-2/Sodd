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
current.year <- as.numeric(substr(Sys.Date(), 3, 4))
test.dload.file <- file.path(base_dload_path, historic_subdir, paste0(current.year, current.year+1), paste0(all.leagues[[1]], ".csv"))
urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  status == HTTP_STATUS_OK
}
if(urlFileExist(test.dload.file)) {
  current.year <- current.year + 1
}
# last 10 years of data. this will break for 0910 and 2100 I guess
all.years <- c()
for (i in seq(10)) {
  all.years <- c(all.years, paste0(current.year-i, current.year-i+1))
}
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

