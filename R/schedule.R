

#' Schedule a daily model build
#'
#' @param leagues Leagues to include in modeling data. Defaults to all.leagues
#' @param years integer. Number of years to look back in modeling data. Defaul
#' ts to 10
#' @param address character. email address to send model results to
#' @return NULL
#' @family model
#' @examples
#' \donttest{
#' schedule.model.build()
#' schedule.model.build(c("E0", "E1", "SP1", "SP2"), 3, "ckear1989@gmail.com")
#' }
#' @export
schedule.model.build <- function(
  leagues=all.leagues,
  years=10,
  address=NULL
  ) {
  if(!is.package.available("cronR")) {
    message(paste("model build scheduling not available.",
      "Try install.packages(\"cronR\")"))
  } else {
    output.dir <- get.sodd.output.dir()
    f <- file.path(output.dir, "scheduled.model.R")
    create.scheduled.model.script(leagues, years, f, address)
    cmd <- cronR::cron_rscript(f)
    tryCatch({
      cronR::cron_add(command=cmd, frequency="daily", at="7AM", id="sodd.model.build")
    }, error=function(cond) message(paste0("cron task already exists\n", cond)))
  }
  invisible()
}

create.scheduled.model.script <- function(leagues, years, f, address) {
  code <- "library(\"sodd\")\n"
  code <- paste0(code, "dload.x.years(c(",
    paste(leagues, collapse=", "), ")", years, ")\n")
  code <- paste0(code,
    "dload.current.year(c(", paste(leagues, collapse=", "), "))\n")
  code <- paste0(code,
    "dload.upcoming()\n")
  code <- paste0(code,
    "create.sodd.modeling.data(c(", paste(leagues, collapse=", "), ")", years, ", log.it=TRUE)\n")
  code <- paste0(code,
    "build.all.sodd.models.one.date(", format((Sys.Date()-7), "%Y-%m-%d"), "log.it=TRUE, plot.it=TRUE)\n")
  if(!is.null(address)) {
    code <- paste0(code,
      "email.sodd.model.results(", format((Sys.Date()-7), "%Y-%m-%d"), ", ", address,  ")\n")
  }
  fc <- file(f)
  writeLines(code, fc)
  close(fc)
}

