
test_that("build goals model", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    verbosity=1
  )
  date <- format(Sys.Date()-7, "%Y-%m-%d")
  expect_output(model <- build.sodd.model(date, "fthg", weights=FALSE, plot.it=TRUE))
  if (!is.null(model)) {
    expect_output(build.sodd.model(date, "fthg", weights=FALSE))
    set.sodd.options(
      data.dir="~/sodd.data/",
      verbosity=0
    )
    expect_silent(build.sodd.model(date, "fthg", weights=FALSE))
    expect_silent(build.sodd.model(date, "ftag", weights=FALSE))
  }
})

