
# test_that("build goals model", {
#   set.sodd.options(
#     data.dir="~/sodd.data/",
#     verbosity=0
#   )
#   date <- "2023-09-01"
#   expect_silent(model <- build.sodd.model(date, "fthg", weights=FALSE, plot.it=TRUE))
#   if (!is.null(model)) {
#     expect_silent(build.sodd.model(date, "fthg", weights=FALSE))
#     set.sodd.options(
#       data.dir="~/sodd.data/",
#       force.upcoming=TRUE,
#       verbosity=0
#     )
#     expect_silent(build.sodd.model(date, "fthg", weights=FALSE))
#     expect_silent(build.sodd.model(date, "ftag", weights=FALSE))
#   }
# })

test_that("build model no cv", {
  set.sodd.options(
    data.dir="~/sodd.data/",
    verbosity=0
  )
  date <- "2023-09-01"
  expect_silent(model <- build.sodd.model(date, "fthg", weights=FALSE, plot.it=TRUE))
  if (!is.null(model)) {
    expect_silent(model <- build.sodd.model(date, "fthg", weights=FALSE))
    # check default params
    expect_equal(model$params$num_trees, 500)
    expect_equal(model$params$train_fraction, 0.7)
    expect_equal(model$params$shrinkage, 0.1)
    expect_equal(model$params$interaction_depth, 2)
    expect_equal(model$cv_folds, 3)
    set.sodd.options(
      data.dir="~/sodd.data/",
      model.params=list(
        train.fraction=0.6,
        n.trees=10,
        shrinkage=0.05,
        interaction.depth=3,
        cv.folds=1
      ),
      force.upcoming=TRUE,
      verbosity=0
    )
    expect_silent(model <- build.sodd.model(date, "fthg", weights=FALSE))
    # check default params
    expect_equal(model$params$num_trees, 10)
    expect_equal(model$params$train_fraction, 0.6)
    expect_equal(model$params$shrinkage, 0.05)
    expect_equal(model$params$interaction_depth, 3)
    expect_equal(model$cv_folds, 1)
  }
})

