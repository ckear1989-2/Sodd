
# #' Email modeling results and logs
# #'
# #' @param adate character. Date of model train/test split in format "%Y-%m%-d"
# #' @param address character. email address to send model results to
# #' @return NULL
# #' @family model
# #' @examples
# #' \donttest{
# #' email.sodd.model.results(format(Sys.Date()-7, "%Y-%m%-d"), "ckear1989@gmail.com")
# #' }
# #' @export
# email.sodd.model.results <- function(
#   adate,
#   address
#   ) {
#   if(!is.package.available("gmailr")) {
#     message("email sending not available. Try install.packages(\"gmailr\")")
#   } else if(!file.exists(Sys.getenv("GMAILR_APP"))) {
#     message("email sending not available. Please set environment variable GMAILR_APP")
#   } else if(!file.exists(Sys.getenv("GMAILR_SECRET"))) {
#     message("email sending not available. Please set environment variable GMAILR_SECRET")
#   } else {
#     output.dir <- get.sodd.output.dir()
#     fs <- c(
#       dlogf=file.path(output.dir, paste0("download.log")),
#       slogf=file.path(output.dir, paste0("standardise.log")),
#       sclogf=file.path(output.dir, paste0("scheduled.model.log")),
#       mlogf0=file.path(output.dir, paste0("model_", adate, "_act", ".log")),
#       mpdff0=file.path(output.dir, paste0("model_", adate, "_act", ".pdf")),
#       mpngf0=file.path(output.dir, paste0("model_", adate, "_act", "_strategy", ".png")),
#       mlogf1=file.path(output.dir, paste0("model_", adate, "_spread", ".log")),
#       mpdff1=file.path(output.dir, paste0("model_", adate, "_spread", ".pdf")),
#       mpngf1=file.path(output.dir, paste0("model_", adate, "_spread", "_strategy", ".png")),
#       mlogf2=file.path(output.dir, paste0("model_", adate, "_fthg", ".log")),
#       mpdff2=file.path(output.dir, paste0("model_", adate, "_fthg", ".pdf")),
#       mpngf2=file.path(output.dir, paste0("model_", adate, "_fthg", "_strategy", ".png")),
#       mlogf3=file.path(output.dir, paste0("model_", adate, "_ftag", ".log")),
#       mpdff3=file.path(output.dir, paste0("model_", adate, "_ftag", ".pdf")),
#       mpngf3=file.path(output.dir, paste0("model_", adate, "_ftag", "_strategy", ".png")),
#       mlogf4=file.path(output.dir, paste0("model_", adate, "_act", "_wtd", ".log")),
#       mpdff4=file.path(output.dir, paste0("model_", adate, "_act", "_wtd", ".pdf")),
#       mpngf4=file.path(output.dir, paste0("model_", adate, "_act", "_wtd", "_strategy", ".png")),
#       mlogf5=file.path(output.dir, paste0("model_", adate, "_spread", "_wtd", ".log")),
#       mpdff5=file.path(output.dir, paste0("model_", adate, "_spread", "_wtd", ".pdf")),
#       mpngf5=file.path(output.dir, paste0("model_", adate, "_spread", "_wtd", "_strategy", ".png")),
#       mlogf6=file.path(output.dir, paste0("model_", adate, "_fthg", "_wtd", ".log")),
#       mpdff6=file.path(output.dir, paste0("model_", adate, "_fthg", "_wtd", ".pdf")),
#       mpngf6=file.path(output.dir, paste0("model_", adate, "_fthg", "_wtd", "_strategy", ".png")),
#       mlogf7=file.path(output.dir, paste0("model_", adate, "_ftag", "_wtd", ".log")),
#       mpdff7=file.path(output.dir, paste0("model_", adate, "_ftag", "_wtd", ".pdf")),
#       mpngf7=file.path(output.dir, paste0("model_", adate, "_ftag", "_wtd", "_strategy", ".png"))
#     )
#     suppressMessages({
#       gmailr::gm_auth_configure(path=Sys.getenv("GMAILR_APP"))
#       gmailr::gm_auth(email=TRUE, cache=Sys.getenv("GMAILR_SECRET"))
#       test_email <- gmailr::gm_mime()
#       test_email <- gmailr::gm_to(test_email, address)
#       test_email <- gmailr::gm_from(test_email, paste0(Sys.info()[["user"]], "@", Sys.info()[["nodename"]], ".com"))
#       test_email <- gmailr::gm_subject(test_email, paste(adate, "model results"))
#       test_email <- gmailr::gm_text_body(test_email, "See attachments")
#       for(f in fs) test_email <- attach.if.available(test_email, f)
#       # gmailr::gm_create_draft(test_email)
#       if(!is.null(address)) gmailr::gm_send_message(test_email)
#     })
#   }
#   invisible()
# }
# 
# #' Email no data change status
# #'
# #' @param address character. email address to send model results to
# #' @return NULL
# #' @family model
# #' @examples
# #' \donttest{
# #' email.no.data.change("ckear1989@gmail.com")
# #' }
# #' @export
# email.no.data.change <- function(address) {
# }
#   if(!is.package.available("gmailr")) {
#     message("email sending not available. Try install.packages(\"gmailr\")")
#   } else {
#     output.dir <- get.sodd.output.dir()
#     fs <- c(
#       dlogf=file.path(output.dir, paste0("download.log")),
#       sclogf=file.path(output.dir, paste0("scheduled.model.log"))
#     )
#     gmailr::gm_auth_configure(path=Sys.getenv("GMAILR_APP"))
#     gmailr::gm_auth(email=TRUE, cache=Sys.getenv("GMAILR_SECRET"))
#     test_email <- gmailr::gm_mime()
#     test_email <- gmailr::gm_to(test_email, address)
#     test_email <- gmailr::gm_from(test_email, paste0(Sys.info()[["user"]], "@", Sys.info()[["nodename"]], ".com"))
#     test_email <- gmailr::gm_subject(test_email, "no data change")
#     test_email <- gmailr::gm_text_body(test_email, "models not run due to no change in data")
#     for(f in fs) test_email <- attach.if.available(test_email, f)
#     if(!is.null(address)) gmailr::gm_send_message(test_email)
#   }
#   invisible()
# }
# 
# attach.if.available <- function(m, f) {
#   if(file.exists(f)) m <- gmailr::gm_attach_file(m, f)
#   m
# }

attach.if.available <- function(m, f) {
    print("debugging attach if available")
}

#' Email modeling results and logs
#'
#' @param adate character. Date of model train/test split in format "%Y-%m%-d"
#' @param address character. email address to send model results to
#' @return NULL
#' @family model
#' @examples
#' \donttest{
#' email.sodd.model.results(format(Sys.Date()-7, "%Y-%m%-d"), "ckear1989@gmail.com")
#' }
#' @export
email.sodd.model.results <- function(
  adate,
  address
  ) {
    print(paste("debugging email for", adate, address))
}

#' Email no data change status
#'
#' @param address character. email address to send model results to
#' @return NULL
#' @family model
#' @examples
#' \donttest{
#' email.no.data.change("ckear1989@gmail.com")
#' }
#' @export
email.no.data.change <- function(address) {
    print(paste("debugging email for", address))
}
