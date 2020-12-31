
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
  if(!is.package.available("gmailr")) {
    message("email sending not available. Try install.packages(\"gmailr\")")
  } else {
    output.dir <- get.sodd.output.dir()
    mzip <- file.path(output.dir, paste0("model_", adate, ".zip"))
    fs <- c(
      dlogf=file.path(output.dir, paste0("standardise.log")),
      mlogf0=file.path(output.dir, paste0("model_", adate, "_act", ".log")),
      mpdff0=file.path(output.dir, paste0("model_", adate, "_act", ".pdf")),
      mlogf1=file.path(output.dir, paste0("model_", adate, "_spread", ".log")),
      mpdff1=file.path(output.dir, paste0("model_", adate, "_spread", ".pdf")),
      mlogf2=file.path(output.dir, paste0("model_", adate, "_act", "_wtd", ".log")),
      mpdff2=file.path(output.dir, paste0("model_", adate, "_act", "_wtd", ".pdf")),
      mlogf3=file.path(output.dir, paste0("model_", adate, "_spread", "_wtd", ".log")),
      mpdff3=file.path(output.dir, paste0("model_", adate, "_spread", "_wtd", ".pdf"))
    )
    gmailr::gm_auth_configure()
    gmailr::gm_auth(email=TRUE, cache="~/.secret")
    test_email <- gmailr::gm_mime()
    test_email <- gmailr::gm_to(test_email, address)
    test_email <- gmailr::gm_from(test_email, address)
    test_email <- gmailr::gm_subject(test_email, paste(adate, "model results"))
    test_email <- gmailr::gm_text_body(test_email, "See attachments")
    for(f in fs) test_email <- attach.if.available(test_email, f)
    gmailr::gm_create_draft(test_email)
    gmailr::gm_send_message(test_email)
  }
  invisible()
}

attach.if.available <- function(m, f) {
  if(file.exists(f)) m <- gmailr::gm_attach_file(m, f)
  m
}

