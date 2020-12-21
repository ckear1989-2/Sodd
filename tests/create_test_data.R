
library("data.table")

source("utils/utils.R")

create_test_dataset <- quote({
  # 10% sample of existing dataset
  adate <- "2020-12-11"
  yvar <- "spread"
  eval(read.model.data)
  random_by_char <- function(cvar) {
    suppressWarnings(TeachingDemos::char2seed(cvar))
    runif(1)
  }
  test.a.dt <- copy(a.dt)
  test.a.dt[, rvar := random_by_char(match_id), match_id]
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
  test.a.dt <- test.a.dt[rvar <= 0.1, ]
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
  test.a.dt[, rvar := NULL]
  saveRDS(test.a.dt, "~/data/R/rds/test.a.dt.rds")
  test.a.dt <- readRDS("~/data/R/rds/test.a.dt.rds")
  print(test.a.dt[, list(count=.N, match_count=length(unique(match_id)))])
})

args = commandArgs()
this_file <- "create_test_data.R"
file_run <- ""
if(length(args) > 3) file_run <- strsplit(args[[4]], "/")[[1]][[2]]
if(file_run == this_file) {
  eval(create_test_dataset)
}
