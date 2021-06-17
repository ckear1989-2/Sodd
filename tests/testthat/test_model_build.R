
test_that("build goals model", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    verbosity=1
  )
  model <- build.sodd.model(format(Sys.Date()-7, "%Y-%m-%d"), "fthg", weights=FALSE, plot.it=TRUE)
  if (!is.null(model)) {
    print(model)
    expect_output(build.sodd.model(format(Sys.Date()-7, "%Y-%m-%d"), "fthg", weights=FALSE))
    set.sodd.options(
      data.dir="~/sodd.data/",
      verbosity=0
    )
    expect_silent(build.sodd.model(format(Sys.Date()-7, "%Y-%m-%d"), "fthg", weights=FALSE))
    expect_silent(build.sodd.model(format(Sys.Date()-7, "%Y-%m-%d"), "ftag", weights=FALSE))
  }
})

