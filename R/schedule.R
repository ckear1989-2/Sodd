

#' Schedule a daily model build
#'
#' @param leagues Leagues to include in modeling data. Defaults to all.leagues
#' @param years integer. Number of years to look back in modeling data. Defaul
#' ts to 10
#' @param address character. email address to send model results to. Deaults to
#' NULL
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
  code <- paste0(
    code,
    "set.sodd.options(\n  ",
    "data.dir=\"", get.sodd.data.dir(), "\",\n  ",
    "output.dir=\"", get.sodd.output.dir(), "\",\n  ",
    "force.upcoming=", get.sodd.force.upcoming(), ",\n  ",
    "verbosity=", get.sodd.verbosity(), "\n)\n")
  code <- paste0(code, "check.status <- ", "dload.x.years(c(\"",
    paste(leagues, collapse="\", \""), "\"), ", years, ", force=FALSE, check=TRUE, quiet=TRUE)\n")
  code <- paste0(code, "check.status <- c(check.status, ",
    "dload.current.year(c(\"", paste(leagues, collapse="\", \""), "\"), check=TRUE, quiet=TRUE))\n")
  code <- paste0(code, "check.status <- c(check.status, ",
    "dload.upcoming(check=TRUE, quiet=TRUE))\n")
  code <- paste0(code, "print(check.status)\nprint(any(check.status))\nif(any(check.status)) {\n  ")
  code <- paste0(code,
    "create.sodd.modeling.data(c(\"", paste(leagues, collapse="\", \""), "\"), ", years, ")\n  ")
  code <- paste0(code,
    "build.all.sodd.models.one.date(format((Sys.Date()-7), \"%Y-%m-%d\"), plot.it=TRUE)\n")
  if(!is.null(address)) {
    code <- paste0(code, "  ",
      "email.sodd.model.results(format((Sys.Date()-7), \"%Y-%m-%d\"), \"", address, "\")\n")
  }
  code <- paste0(code,
    "} else {\n  message(\"no changes detected in any files\")\n"
  )
  if(!is.null(address)) {
    code <- paste0(code,
      "  email.no.data.change(\"", address, "\")\n")
  }
  code <- paste0(code, "}\n")
  fc <- file(f)
  writeLines(code, fc)
  close(fc)
}

