
test_that("build goals model", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    verbosity=1
  )
  date <- "2023-09-01"
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

test_that("build model no cv", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    verbosity=1
  )
  date <- "2023-09-01"
  expect_output(model <- build.sodd.model(date, "fthg", weights=FALSE, plot.it=TRUE))
  if (!is.null(model)) {
    expect_output(build.sodd.model(date, "fthg", weights=FALSE))
    set.sodd.options(
      data.dir="~/sodd.data/",
      model.params=list(
        train.fraction=0.7,
        n.trees=10,
        shrinkage=0.1,
        interaction.depth=2,
        cv.folds=1
      ),
      verbosity=0
    )
    expect_silent(build.sodd.model(date, "fthg", weights=FALSE))
  }
})

